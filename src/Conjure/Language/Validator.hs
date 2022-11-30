{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Validator where

import Conjure.Language.AST.ASTParser
import Conjure.Language.AST.Syntax as S
import Conjure.Language.Definition hiding (Typed)
import qualified Conjure.Language.Definition  as D (Expression(Typed))
import Conjure.Language.Domain
import Conjure.Language.Lexemes
import Conjure.Language.NewLexer (ETok (ETok, lexeme), ETokenStream (ETokenStream), eLex, sourcePos0, tokenSourcePos, totalLength, tokenStart, trueLength)

import Conjure.Language.Attributes
import Conjure.Prelude

import Control.Monad.Writer.Strict (Writer)

import Conjure.Language.Type

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (pack, unpack, toLower, append)
import Text.Megaparsec
    ( SourcePos, mkPos )

import Conjure.Language.Expression.Op
    ( OpSlicing(..),
      Op(..),
      OpPowerSet(..),
      OpImage(OpImage),
      mkOp,
      mkBinOp,
      Op(MkOpRelationProj, MkOpSlicing, MkOpIndexing),
      OpRelationProj(OpRelationProj),
      OpIndexing(OpIndexing), OpType (..), OpAttributeAsConstraint (OpAttributeAsConstraint), OpAnd (OpAnd),
      )
import Conjure.Language.Domain.AddAttributes (allSupportedAttributes)
import Conjure.Language.AST.Reformer (Flattenable (flatten))
import Text.Megaparsec.Pos (SourcePos(..))
import Text.Megaparsec (unPos)
import Data.Sequence (Seq (..), viewr, ViewR (..))
import Conjure.Language.Pretty (Pretty(pretty))
import Control.Monad.Except (runExcept)
import Conjure.Language.TypeOf (TypeOf(typeOf))
import Control.Applicative
import Conjure.Language.Expression.OpTypes (binOpType)
import Conjure.Language.AdHoc ((:<)(project))
import Control.Monad (mapAndUnzipM)
import Conjure.Bug (bug)
import Text.PrettyPrint (text)

data TagType 
    =TtType        
    |TtClass       
    |TtEnum        
    |TtStruct      
    |TtParameter   
    |TtVariable    
    |TtProperty    
    |TtEnumMember  
    |TtEvent       
    |TtFunction    
    |TtMethod      
    |TtMacro       
    |TtKeyword     
    |TtModifier    
    |TtComment     
    |TtString      
    |TtNumber      
    |TtRegexp      
    |TtOperator    
    |TtOther Text   
    deriving Show
data TaggedToken
    = TaggedToken TagType ETok
    deriving Show


class WithRegion a where
    getRegion :: (WithRegion a) => a -> DiagnosticRegion

instance WithRegion (DiagnosticRegion,a) where
    getRegion (r,_) = r

instance WithRegion DiagnosticRegion where
    getRegion = id

instance WithRegion LToken where
    getRegion = symbolRegion

type RegionTagged a = (DiagnosticRegion,a)
unregion :: RegionTagged a -> a
unregion (_,a) =a

data Typed a = Typed Type a
    deriving Show
instance Functor Typed where
  fmap f (Typed ty' a) = Typed ty' (f a)

instance TypeOf (Typed a) where
    typeOf (Typed t _) = return t

untype :: Typed a -> a
untype (Typed _ a) = a

typeOf_ :: Typed a -> Type
typeOf_ (Typed t _) = t

untypeAs :: Type -> Typed a -> ValidatorS a
untypeAs r ((Typed t a)) = if let ?typeCheckerMode=StronglyTyped in typeUnify r t
                            then return a
                            else contextTypeError (TypeError r t) >> return a


type TypeCheck a = Typed a -> ValidatorS ()
exactly :: Type -> TypeCheck a
exactly t r = void $ untypeAs t r

typeAs :: Type -> Maybe a -> Maybe (Typed a)
typeAs t (Just a) = Just $ Typed t a
typeAs t Nothing = Nothing

(?=>) :: ValidatorS (Typed a) -> TypeCheck a -> ValidatorS a
v ?=> t = v >>= (\a -> t a >> return (untype a))

castAny :: Validator a -> Validator (Typed a)
castAny a = typeAs TypeAny <$> a

tInt :: Type
tInt = TypeInt TagInt

typeSplit :: Typed a -> (Type,a)
typeSplit (Typed t v) = (t,v)

getTypeList :: [Typed a] -> [(Type,a)]
getTypeList = map typeSplit

data ErrorType
    = TokenError LToken
    | SyntaxError Text
    | WithReplacements ErrorType [Text]
    | SemanticError Text
    | CustomError Text
    | SkippedTokens
    | MissingArgsError Int Int
    | UnexpectedArg
    | TypeError Type Type -- Expected, got
    | ComplexTypeError Text Type -- Expected, got
    | InternalError --Used to explicitly tag invalid pattern matches
    | InternalErrorS Text -- Used for giving detail to bug messages
    deriving  (Show,Eq,Ord)
data WarningType = UnclassifiedWarning Text deriving (Show,Eq,Ord)
data InfoType = UnclassifiedInfo Text deriving (Show,Eq,Ord)


data Diagnostic = Error ErrorType | Warning WarningType | Info InfoType
    deriving (Show,Eq,Ord)


data ValidatorDiagnostic = ValidatorDiagnostic DiagnosticRegion Diagnostic
    deriving Show

isError :: ValidatorDiagnostic -> Bool
isError (ValidatorDiagnostic _ (Error _)) = True
isError _ = False

-- data ValidatorDiagnostic
--     = TypeError String
--     | StateError String
--     | SyntaxError String
--     | RegionError String -- Add region
--     | TokenError LToken
--     | TaggedTokenError String LToken
--     | IllegalToken LToken -- Should not occur in practice and indicates a logical error somewhere
--     | NotImplemented String
--     deriving (Show)
data DeclarationType = Definition | LiteralDecl | Ref DiagnosticRegion
    deriving Show
data RegionInfo = RegionInfo {
    rRegion :: DiagnosticRegion,
    rType :: Type,
    rText :: Text,
    rDeclaration :: DeclarationType
} deriving Show

mkDeclaration :: DiagnosticRegion -> Text -> Typed a -> RegionInfo
mkDeclaration r n (Typed t _) = RegionInfo r t n Definition

mkLiteral :: DiagnosticRegion -> Text -> Typed a -> RegionInfo
mkLiteral r n (Typed t _) = RegionInfo r t n LiteralDecl

--Infix symbol validation and tagging
isA :: LToken -> TagType -> ValidatorS ()
isA a b= validateSymbol a >> flagToken a b

are :: [LToken] -> TagType -> ValidatorS ()
are a b = mapM_ (`isA` b) a

flagToken :: LToken -> TagType -> ValidatorS ()
flagToken (RealToken _ t) c = modify (\x@ValidatorState{symbolCategories=sc}->x{symbolCategories= M.insert t (TaggedToken c t) sc})
flagToken _ _ = return ()


tagWithType :: NameNode -> Type -> ValidatorS ()
tagWithType (NameNode lt) ty = flagToken lt $ case ty of
   TypeEnum _ -> TtEnum
   TypeInt (TagEnum _) -> TtEnumMember
   TypeInt (TagUnnamed _) -> TtEnumMember
   TypeUnnamed _ -> TtEnum
   TypeVariant _ -> TtProperty
   TypeRecord _ -> TtProperty
   TypeFunction _ _ -> TtFunction
   TypeSequence _ -> TtFunction
   TypeRelation _ -> TtFunction
   _ -> TtVariable
data ValidatorState = ValidatorState {
    typeChecking :: Bool,
    regionInfo :: [RegionInfo],
    symbolTable :: SymbolTable,
    symbolCategories ::Map ETok TaggedToken,
    currentContext :: DiagnosticRegion
}
    deriving Show
instance Default ValidatorState where
    def = ValidatorState {
        typeChecking = True,
        regionInfo=[],
        symbolCategories=M.empty,
        symbolTable=M.empty,
        currentContext=GlobalRegion
        }
type SymbolTable = (Map Text SymbolTableValue)
type SymbolTableValue = (DiagnosticRegion,Bool,Type)
-- instance Show SymbolTableValue where
--     show (SType t) = show $ pretty t
--     show (SDomain d) = show $ pretty d 
newtype ValidatorT r w a = ValidatorT (MaybeT (StateT r (Writer [w])) a)
    deriving (Monad,Applicative ,Functor,MonadState r ,MonadWriter [w],MonadFail)


--synonym wrapped in maybe to allow errors to propagate
type Validator a = ValidatorT ValidatorState ValidatorDiagnostic (Maybe a)

--Non maybe version used in outward facing applications/ lists 
type ValidatorS a = ValidatorT ValidatorState ValidatorDiagnostic a

-- addEnumDefns ::  [Text] -> SymbolTable -> SymbolTable
-- addEnumDefns names (SymbolTable enums) = SymbolTable $ enums ++  map (\m -> (m,"Enum")) names

modifySymbolTable :: (SymbolTable -> SymbolTable) -> ValidatorS ()
modifySymbolTable f = modify (\x -> x{symbolTable=f.symbolTable $ x})

getSymbol :: Text -> ValidatorS (Maybe SymbolTableValue)
getSymbol n = M.lookup n <$> getSymbolTable

putSymbol :: (Name , SymbolTableValue) -> ValidatorS Bool
putSymbol (Name name,t) = do
                    x <- getSymbol name
                    modifySymbolTable (M.insert name t)
                    case x of
                      Nothing -> return False
                      Just _ -> return True
putSymbol _ = return False -- skip types for meta and machine vars

addRegion :: RegionInfo -> ValidatorS ()
addRegion r = modify (\x->x{regionInfo=r:regionInfo x})



makeEnumDomain :: Name -> Maybe [Range Expression] -> Domain () Expression
makeEnumDomain n es = DomainEnum n es Nothing

makeUnnamedDomain :: Name -> Domain () ()
makeUnnamedDomain n = DomainUnnamed n ()

getSymbolTable :: ValidatorS SymbolTable
getSymbolTable = symbolTable <$> get

getContext :: ValidatorS DiagnosticRegion
getContext = currentContext <$> get

setContext :: DiagnosticRegion -> ValidatorS ()
setContext r = modify (\p -> p{currentContext = r})

setContextFrom :: Flattenable a => a -> ValidatorS ()
setContextFrom a = setContext $ symbolRegion a

strict :: Validator a -> ValidatorS a
strict a = do Just res <- a; return res

deState :: ((a,r),n) -> (a,n,r)
deState ((a,r),n) = (a,n,r)

runValidator :: (ValidatorT r w a) -> r -> ((Maybe a),[w],r)
runValidator (ValidatorT r) d = deState $ runWriter (runStateT (runMaybeT r) d)

todoTypeAny :: Maybe a -> Maybe (Typed a)
todoTypeAny = typeAs TypeAny

validateModelS :: ProgramTree -> ValidatorS Model
validateModelS = strict . validateModel

validateModel :: ProgramTree -> Validator Model
validateModel model = do
        sts <- validateProgramTree (statements  model)
        langVersion <- validateLanguageVersion $ langVersionInfo model
        return $ Model <$> langVersion <*> sts <*> pure def


validateProgramTree :: [StatementNode] -> Validator [Statement]
validateProgramTree sts = do
    q <- validateArray validateStatement sts
    return . pure $ concat q


isValidLanguageName :: Text -> Bool
isValidLanguageName t = T.toLower t `elem` ["essence","essence'"]

validateLanguageVersion :: Maybe LangVersionNode -> Validator LanguageVersion
validateLanguageVersion Nothing = return $ pure $ LanguageVersion "Essence" [1,3]
validateLanguageVersion (Just lv@(LangVersionNode l1 n v)) = do
    setContextFrom lv
    let NameNode nt = n
    l1 `isA` TtKeyword
    name <- validateIdentifier n
    unless (maybe False isValidLanguageName name) (raiseError $symbolRegion  n <!> SyntaxError "Not a valid language name")
    nums <- catMaybes <$> validateSequence_ getNum v
    return . pure $
        LanguageVersion
            (Name $ fromMaybe "Essence" name)
            (if null nums then [1,3] else nums)
    where
        getNum :: LToken -> Validator Int
        getNum c = do
            c' <- validateSymbol c
            case c' of
                Just (LIntLiteral x) -> return . pure $ fromInteger x
                _ -> invalid $ c <!> InternalError


validateStatement :: StatementNode -> ValidatorS [Statement]
validateStatement (DeclarationStatement dsn) = validateDeclarationStatement dsn
validateStatement (BranchingStatement bsn) = validateBranchingStatement bsn
validateStatement (SuchThatStatement stsn) = validateSuchThatStatement stsn
validateStatement (WhereStatement wsn) = validateWhereStatement wsn
validateStatement (ObjectiveStatement osn) = validateObjectiveStatement osn
validateStatement (HeuristicStatement lt exp) = validateHeuristicStatement lt exp
validateStatement (UnexpectedToken lt) = return [] <* (invalid $ lt <!> TokenError  lt) --TODO address as part of skip token refactor

validateHeuristicStatement :: LToken -> ExpressionNode -> ValidatorS [Statement]
validateHeuristicStatement lt exp = do
    let validHeuristics = ["static", "sdf", "conflict", "srf", "ldf", "wdeg", "domoverwdeg"]
    lt `isA` TtKeyword
    h <- case exp of
      IdentifierNode nn@(NameNode (RealToken _ (ETok{lexeme=(LIdentifier nm)}))) -> do
                    if nm `elem` validHeuristics then
                        return $ pure [SearchHeuristic  (Name nm)]
                    else
                        invalid $ symbolRegion nn <!> (SemanticError $ T.concat ["Invalid heuristic " , nm , " Expected one of: ", (pack $ show validHeuristics )])
      _ ->  (invalid $ symbolRegion exp <!> SemanticError "Only identifiers are allowed as heuristics")
    return $ fromMaybe [] h


tCondition :: TypeCheck a
tCondition (Typed TypeAny _) = pure ()
tCondition (Typed TypeBool _) = pure ()
tCondition (Typed (TypeMatrix _ TypeBool) _) = pure ()
tCondition (Typed (TypeList TypeBool) _) = pure ()
tCondition t = contextTypeError $ ComplexTypeError "Bool or [Bool]" $ typeOf_ t

validateWhereStatement :: WhereStatementNode -> ValidatorS [Statement]
validateWhereStatement (WhereStatementNode l1 exprs) = do
    l1 `isA` TtKeyword
    ws <-  Where <$> validateSequence_ (\x -> do setContextFrom x; validateExpression x ?=> tCondition) exprs
    return [ws]

validateObjectiveStatement :: ObjectiveStatementNode -> ValidatorS [Statement]
validateObjectiveStatement (ObjectiveMin lt en) = do
    lt `isA` TtKeyword
    exp <- validateExpression en
    return [Objective Minimising $ untype exp]
validateObjectiveStatement (ObjectiveMax lt en) =do
    lt `isA` TtKeyword
    exp <- validateExpression en
    return  [Objective Maximising $ untype exp]

validateSuchThatStatement :: SuchThatStatementNode -> ValidatorS [Statement]
validateSuchThatStatement (SuchThatStatementNode l1 l2 exprs) = do
    l1`isA` TtKeyword
    l2`isA` TtKeyword
    exprs' <- validateSequence validateExpression exprs
    bools <- mapM (\(a,b)->do setContext a; return b ?=> tCondition) exprs'
    let bool_exprs = bools
    return [SuchThat  bool_exprs]

validateBranchingStatement :: BranchingStatementNode -> ValidatorS [Statement]
validateBranchingStatement (BranchingStatementNode l1 l2 sts) = do
    l1 `isA` TtKeyword
    l2 `isA` TtKeyword
    branchings <-catMaybes <$> validateList_ (f2n validateBranchingParts) sts
    return [SearchOrder branchings]
    where
        validateBranchingParts :: ExpressionNode -> ValidatorS SearchOrder
        validateBranchingParts (IdentifierNode nn) =  do
            n <- validateNameAs TtVariable nn
            return $ BranchingOn n
        validateBranchingParts exp = do
            x <- validateExpression exp ?=> exactly TypeAny
            return $ Cut x

validateDeclarationStatement :: DeclarationStatementNode -> ValidatorS [Statement]
validateDeclarationStatement stmt = do
    stmt' <- case stmt of
        FindStatement l1 fs ->  l1 `isA` TtKeyword >> validateStatementSeq validateFind fs
        GivenStatement l1 gs ->  l1 `isA` TtKeyword >> validateStatementSeq validateGiven gs
        LettingStatement l1 ls ->  l1 `isA` TtKeyword >> validateStatementSeq validateLetting ls
    return  $ Declaration <$> stmt'
    where
        validateStatementSeq v l= do
            decls <- validateSequence_ v l
            return $ concat decls

validateGiven :: GivenStatementNode -> ValidatorS [Declaration]
validateGiven (GivenStatementNode idents l1 domain) =
    do
        checkSymbols [l1] -- Colon
        names <- validateSequence (validateNameAs TtVariable) idents
        (dType, dom) <- typeSplit <$> validateDomain domain
        let declarations = [(mkDeclaration r n (Typed dType ())) | (r, Name n) <- names]
        mapM_ addRegion declarations
        mapM_ (\(r,x) -> putSymbol (x,(r,False,dType)) ) names
        return  $ [ FindOrGiven Given nm dom|(_,nm) <- names ]
validateGiven (GivenEnumNode se l1 l2 l3) =
    do
        [l1, l2, l3] `are` TtKeyword --new Type enum
        names <- validateSequence (validateNameAs TtEnum) se
        let eType = TypeEnum
        mapM_ (\(r,x) -> putSymbol (x,(r,True,eType x) )) names
        return  $  [GivenDomainDefnEnum n | (_,n) <- names]

validateFind :: FindStatementNode -> ValidatorS [Declaration]
validateFind (FindStatementNode names colon domain) = do
    checkSymbols [colon] --colon
    names' <- validateSequence (validateNameAs TtVariable) names
    (dType, dom) <- typeSplit <$> validateDomain domain
    mapM_ (\(r,x) -> putSymbol (x,(r,False,dType) )) names'
    mapM_ addRegion [(mkDeclaration r n (Typed dType 1)) | (r, Name n) <- names']
    return  $ [ FindOrGiven Find nm dom|(_,nm) <- names']

validateLetting :: LettingStatementNode -> ValidatorS [Declaration]
-- Letting [names] be
validateLetting (LettingStatementNode names l1 assign) = do
    l1 `isA` TtKeyword
    validateLettingAssignment names assign

validateLettingAssignment :: (Sequence NameNode) -> LettingAssignmentNode -> ValidatorS [Declaration]
validateLettingAssignment names (LettingExpr en)  = do
    expr <- validateExpression en
    setContextFrom en
    names' <- validateSequence (validateNameAs TtVariable) names
    let (t,e) = typeSplit expr
    let declarations = [mkDeclaration r n (Typed t 1) |(r, Name n) <- names']
    mapM_ addRegion declarations
    mapM_ (\(r,x) -> putSymbol (x, (r,False,t) )) names'
    return  $ [Letting n e | (_,n) <- names']
validateLettingAssignment names (LettingDomain lt dn) = do
    lt `isA` TtModifier --TODO classify
    (tDomain,domain) <- typeSplit <$> validateDomain dn
    names' <- validateSequence (validateNameAs TtClass) names
    let declarations = [ mkDeclaration r n (Typed tDomain ()) |(r, Name n) <- names']
    mapM_ addRegion declarations
    mapM_ (\(r,x) -> putSymbol (x, (r,False,tDomain))) names'
    return $ [Letting n  (Domain domain)| (_,n) <- names']
validateLettingAssignment names (LettingEnum l1 l2 l3 enames) = do
    [l1, l2, l3] `are` TtKeyword
    names' <- validateSequence (validateNameAs TtEnum) names
    memberNames <- catMaybes  <$> listElems enames
    [n | NameNode n <- memberNames] `are` TtEnumMember
    members <- mapM (validateNameAs TtEnumMember) memberNames
    sequence_
        [
            (do
                let nameMap = zip members ([1..] :: [Int])
                let tVal = TypeInt $ TagEnum n
                addRegion $ RegionInfo {rRegion=r, rText=n, rType=tVal, rDeclaration=Ref r}
                void $ putSymbol (Name n,(r,True,tVal))
                mapM_ (
                    \(x,i) -> putSymbol (x,(r,False,tVal))
                    ) nameMap
            )
            |(r, Name n) <- names'
        ]
    return $ [LettingDomainDefnEnum n members| (_,n) <- names']
validateLettingAssignment names (LettingAnon l1 l2 l3 l4 szExp) = do
    [l1, l2, l3, l4] `are` TtKeyword --TODO keywords
    names' <- validateSequence (validateNameAs TtEnum) names
    size <- do
                    setContextFrom szExp
                    validateExpression szExp ?=> exactly tInt
    let d = TypeUnnamed
    --TODO delcs
    mapM_ (\(r,x) -> putSymbol (x,(r,False,d x))) names'
    return  $ [LettingDomainDefnUnnamed n size| (_,n) <- names']


-- validate :: Validator a -> Validator (Maybe a)
-- validate n = do
--     case n of
--         Validator Nothing ves -> Validator (Just Nothing) ves
--         Validator (Just a) ves -> Validator (Just $ Just a) ves

-- getPrefix :: Validator Int
-- getPrefix = Validator Nothing [TypeError "ERR"]

-- g :: Validator Foo
-- g = do
--     _ <- validate getPrefix
--     a <-  do return 1
--     b <-  do return 1 :: Validator Int
--     c <-  do return 1
--     return $ Foo <$> a <*> b <*> c

-- verify :: Maybe a -> Validator a
-- verify (Just a) = Validator{value = Just a, errors = []}
-- verify Nothing = Validator{value = Nothing, errors = []}

invalid :: ValidatorDiagnostic -> Validator a
invalid err = do
    raiseError err
    return Nothing
    --  Validator Nothing [err]

-- rg :: String
-- rg = case g of
--     (Validator x es) -> show (x, es)

-- type Checker a = State [ValidatorDiagnostic] (Maybe a)

validateSymbol :: LToken -> Validator Lexeme
validateSymbol s =
    case s of
        RealToken ss et -> do
            checkSymbols (map SkippedToken ss)
            return . pure  $ lexeme et
        _ -> invalid $ ValidatorDiagnostic (getRegion s) $ Error $ TokenError s

-- [MissingTokenError ]


type TypedDomain = Typed (Domain () Expression)

type DomainValidator = Validator TypedDomain

validateDomainWithRepr :: DomainNode -> ValidatorS (Typed (Domain HasRepresentation Expression))
validateDomainWithRepr dom = do
    (t,dom') <- typeSplit <$> validateDomain dom
    return . (Typed t) $ changeRepr NoRepresentation dom'

validateDomain :: DomainNode -> ValidatorS TypedDomain
validateDomain dm = case dm of
    MetaVarDomain lt ->  do mv <- validateMetaVar lt ; return . Typed TypeAny $ DomainMetaVar mv
    BoolDomainNode lt -> (lt `isA` TtType >> (return . Typed TypeBool) DomainBool)
    RangedIntDomainNode l1 rs -> l1 `isA` TtType >> validateRangedInt rs
    RangedEnumNode nn ranges -> validateEnumRange nn ranges
    ShortTupleDomainNode lst -> validateTupleDomain lst
    TupleDomainNode l1 doms -> l1 `isA` TtType >> validateTupleDomain doms
    RecordDomainNode l1 ndom -> l1 `isA` TtType >> validateRecordDomain ndom
    VariantDomainNode l1 ndom -> l1 `isA` TtType >> validateVariantDomain ndom
    MatrixDomainNode l1 m_ib idoms l2 dom -> l1 `isA` TtType >> l2 `isA` TtKeyword >> validateIndexedByNode m_ib >> validateMatrixDomain idoms dom
    SetDomainNode l1 attrs l2 dom -> [l1, l2] `are` TtType >> validateSetDomain attrs dom
    MSetDomainNode l1 attrs l2 dom -> [l1, l2] `are` TtType >> validateMSetDomain attrs dom
    FunctionDomainNode l1 attrs dom1 l2 dom2 -> l1 `isA` TtType >> l2 `isA` TtOperator >> validateFunctionDomain attrs dom1 dom2
    SequenceDomainNode l1 attrs l2 dom -> [l1, l2] `are` TtType >> validateSequenceDomain attrs dom
    RelationDomainNode l1 attrs l2 doms -> [l1, l2] `are` TtType >> validateRelationDomain attrs doms
    PartitionDomainNode l1 attrs l2 dom -> [l1, l2] `are` TtType >> validatePartitionDomain attrs dom
    MissingDomainNode lt -> do raiseError $ lt <!> TokenError lt; return $ fallback "Missing Domain"
  where
    validateRangedInt :: Maybe (ListNode RangeNode) -> ValidatorS TypedDomain
    validateRangedInt (Just ln@(ListNode _ (Seq [SeqElem a _]) _)) = do
            d <- case a of
              SingleRangeNode en -> do
                (t,e) <- typeSplit <$> validateExpression en
                case t of
                    TypeInt TagInt -> return $ DomainInt TagInt [RangeSingle e]
                    TypeMatrix _ tInt -> return $ DomainIntE e
                    TypeList tInt -> return $ DomainIntE e
                    TypeSet tInt -> return $ DomainIntE e
                    _ -> return (DomainIntE e) <* raiseTypeError (symbolRegion en <!> ComplexTypeError "Set/List of int or Int" t)
              _ -> do
                r <- validateRange tInt a
                return $ DomainInt TagInt [r]
            return $ Typed tInt d
    validateRangedInt (Just ranges) = do
        ranges' <-  catMaybes <$> validateList_ (f2n (validateRange tInt)) ranges
        return . Typed tInt $ DomainInt TagInt ranges'
    validateRangedInt Nothing = return . Typed tInt $ DomainInt TagInt []
    validateEnumRange :: NameNode -> Maybe (ListNode RangeNode) -> ValidatorS TypedDomain
    validateEnumRange name ranges = do
        Just name' <- validateIdentifier name
        _ <- resolveReference (symbolRegion name,Name name')
        a <- getSymbol name'
        case a of
            Just (_,True,t) -> do
                rs <-case ranges of
                    Just rs -> pure . catMaybes <$> validateList_ (f2n $ validateRange t) rs
                    Nothing -> return Nothing
                return $ Typed t $ DomainEnum (Name name') rs Nothing
            Just (_,False,t) -> case ranges of
              Nothing -> return . Typed t $  DomainReference (Name name') Nothing
              Just rs -> do
                void $ validateList_ (f2n (validateRange TypeAny)) rs
                raiseError (symbolRegion  name <!> SemanticError "range not supported on non enum ranges")
                return . Typed t $ DomainReference (Name name') Nothing
            Nothing -> return $ fallback "unknown symbol"

    validateTupleDomain :: ListNode DomainNode -> ValidatorS TypedDomain
    validateTupleDomain doms = do
        (ts,ds) <- unzip . map typeSplit <$> validateList_ validateDomain doms
        return $ Typed (TypeTuple ts)  (DomainTuple  ds)
    validateRecordDomain :: ListNode NamedDomainNode -> ValidatorS TypedDomain
    validateRecordDomain namedDoms = do
                lst <- validateList  (f2n validateNamedDomainInVariant) namedDoms
                let lst' = mapMaybe (\(r,m)->(\x->(r,x))<$>m) lst
                let (ts,ds) = unzip $ map (\(r,(x,typeSplit->(t,d)))->((x,t),(r,(x,d)))) lst'
                --push members
                let t = TypeRecord ts
                mapM_ (\(r,(a,_))->putSymbol (a,(r,False,t))) ds
                return $ Typed t (DomainRecord (unregion <$> ds))
    validateVariantDomain :: ListNode NamedDomainNode -> ValidatorS TypedDomain
    validateVariantDomain namedDoms = do
                lst <- validateList (f2n validateNamedDomainInVariant) namedDoms
                let lst' = mapMaybe (\(r,m)->(\x->(r,x))<$>m) lst
                let (ts,ds) = unzip $ map (\(r,(x,typeSplit->(t,d)))->((x,t),(r,(x,d)))) lst'
                --push members
                let t = TypeVariant ts
                mapM_ (\(r,(a,_))->putSymbol (a,(r,False,t))) ds
                return $ Typed t (DomainVariant (unregion <$> ds))
    validateMatrixDomain :: ListNode DomainNode -> DomainNode -> ValidatorS TypedDomain
    validateMatrixDomain indexes dom = do
        idoms <-  validateList_ validateDomain indexes
        dom' <-  validateDomain dom
        return $ foldr acc dom' idoms
        where
            -- TODO: This could well be backwards
            acc :: TypedDomain -> TypedDomain -> TypedDomain
            acc (Typed t d) (Typed t' d') = Typed (TypeMatrix t t') (DomainMatrix d d')


    validateSetDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validateSetDomain attrs dom = do
        let repr = ()
        attrs' <- case attrs of
            Just a ->  validateSetAttributes a
            Nothing -> return def
        (t,dom') <- typeSplit <$>  validateDomain dom
        return . Typed (TypeSet t) $ DomainSet repr attrs' dom'

    validateMSetDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validateMSetDomain attrs dom = do
        let repr = ()
        attrs' <- case attrs of
            Just a ->  validateMSetAttributes a
            Nothing -> return $ def
        (t,dom') <- typeSplit <$>  validateDomain dom
        return . Typed (TypeMSet  t) $ DomainMSet  repr  attrs' dom'
    validateFunctionDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> DomainNode -> ValidatorS TypedDomain
    validateFunctionDomain attrs dom1 dom2 = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a ->  validateFuncAttributes a
            Nothing -> return $ def
        (t1,d1) <- typeSplit <$> validateDomain dom1
        (t2,d2) <- typeSplit <$> validateDomain dom2
        let dType = Typed $ TypeFunction t1 t2
        return . dType $ DomainFunction () attrs' d1 d2

    -- attrs <- validateAttributes
    validateSequenceDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> ValidatorS TypedDomain
    validateSequenceDomain attrs dom = do
        let repr = ()
        attrs' <- case attrs of
            Just a ->  validateSeqAttributes a
            Nothing -> return $ def
        (t,dom') <- typeSplit <$> validateDomain dom
        return . Typed (TypeSequence t) $ DomainSequence  repr  attrs'  dom'
    validateRelationDomain ::Maybe (ListNode AttributeNode)-> ListNode DomainNode -> ValidatorS TypedDomain
    validateRelationDomain attrs doms = do
        let repr =  ()
        attrs' <- case attrs of
            Just a ->  validateRelationAttributes a
            Nothing -> return $  def

        (ts,doms') <- unzip . map typeSplit <$> validateList_ validateDomain doms
        return . Typed (TypeRelation ts) $ DomainRelation repr  attrs' doms'
    validatePartitionDomain :: Maybe (ListNode AttributeNode)-> DomainNode -> ValidatorS TypedDomain
    validatePartitionDomain attrs dom = do
        let repr = ()
        attrs' <- case attrs of
            Just a ->  validatePartitionAttributes a
            Nothing -> return $ def
        (t,dom') <- typeSplit <$> validateDomain dom
        return . Typed  (TypePartition t) $ DomainPartition repr attrs' dom'

validateIndexedByNode :: Maybe IndexedByNode -> ValidatorS ()
validateIndexedByNode Nothing = return ()
validateIndexedByNode (Just (IndexedByNode a b)) = [a,b] `are` TtKeyword

todo :: Text -> Validator a
todo s = invalid $ ValidatorDiagnostic GlobalRegion $ Error $ InternalErrorS (append "Not Implemented: " s)

validateSizeAttributes :: [(Lexeme,Maybe Expression)] -> ValidatorS (SizeAttr Expression)
validateSizeAttributes attrs = do
    let sizeAttrs = [L_size,L_minSize,L_maxSize]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $  SizeAttr_None
      [(L_size,Just a)] -> return $  (SizeAttr_Size a)
      [(L_minSize, Just a)] -> return $  (SizeAttr_MinSize a)
      [(L_maxSize, Just a)] -> return $  (SizeAttr_MaxSize a)
      [(L_minSize, Just a),(L_maxSize, Just b)] -> return $ (SizeAttr_MinMaxSize a b)
      as -> return . def <* contextError $ SemanticError $ pack $ "Incompatible attributes size:" ++ show as

validatePartSizeAttributes :: [(Lexeme,Maybe Expression)] -> ValidatorS (SizeAttr Expression)
validatePartSizeAttributes attrs = do
    let sizeAttrs = [L_partSize,L_minPartSize,L_maxPartSize]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $  SizeAttr_None
      [(L_partSize,Just a)] -> return $  (SizeAttr_Size a)
      [(L_minPartSize, Just a)] -> return $  (SizeAttr_MinSize a)
      [(L_maxPartSize, Just a)] -> return $  (SizeAttr_MaxSize a)
      [(L_minPartSize, Just a),(L_maxPartSize, Just b)] -> return $  (SizeAttr_MinMaxSize a b)
      as -> return . def <* contextError $ SemanticError $ pack $ "Incompatible attributes partitionSize :" ++ show as

validateNumPartAttributes :: [(Lexeme,Maybe Expression)] -> ValidatorS (SizeAttr Expression)
validateNumPartAttributes attrs = do
    let sizeAttrs = [L_numParts,L_maxNumParts,L_minNumParts]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $  SizeAttr_None
      [(L_numParts,Just a)] -> return $  (SizeAttr_Size a)
      [(L_minNumParts, Just a)] -> return $  (SizeAttr_MinSize a)
      [(L_maxNumParts, Just a)] -> return $  (SizeAttr_MaxSize a)
      [(L_minNumParts, Just a),(L_maxNumParts, Just b)] -> return $  (SizeAttr_MinMaxSize a b)
      as -> return . def <* contextError $ SemanticError $ pack $ "Incompatible attributes partitionSize :" ++ show as


validateJectivityAttributes :: [(Lexeme,Maybe Expression)] -> ValidatorS JectivityAttr
validateJectivityAttributes attrs = do
    let sizeAttrs = [L_injective,L_surjective,L_bijective]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $  JectivityAttr_None
      [(L_injective,_)] -> return $  JectivityAttr_Injective
      [(L_surjective, _)] -> return $  JectivityAttr_Surjective
      [(L_bijective, _)] -> return $  JectivityAttr_Bijective
      [(L_injective, _),(L_surjective, _)] -> do
        contextInfo $ UnclassifiedInfo "Inj and Sur can be combined to bijective"
        return $  JectivityAttr_Bijective
      as ->do
        void . contextError $ SemanticError $ pack $ "Incompatible attributes jectivity" ++ show as
        return def


validateSetAttributes :: ListNode AttributeNode -> ValidatorS (SetAttr Expression)
validateSetAttributes atts = do
    setContextFrom atts
    attrs <- catMaybes <$> validateList_ (validateAttributeNode setValidAttrs) atts
    size <- validateSizeAttributes attrs
    return $ SetAttr  size


validateMSetAttributes :: ListNode AttributeNode -> ValidatorS (MSetAttr Expression)
validateMSetAttributes atts = do
    setContextFrom atts
    attrs <- catMaybes <$> validateList_ (validateAttributeNode msetValidAttrs) atts
    size <- validateSizeAttributes attrs
    occurs <- validateOccursAttrs attrs
    return $ MSetAttr size  occurs
        where
            validateOccursAttrs attrs = do
                let sizeAttrs = [L_minOccur,L_maxOccur]
                let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
                case filtered of
                    [] -> return $  OccurAttr_None
                    [(L_minOccur,Just a)] -> return $  (OccurAttr_MinOccur a)
                    [(L_maxOccur, Just a)] -> return $  (OccurAttr_MaxOccur a)
                    [(L_minOccur, Just a),(L_maxOccur, Just b)] -> return $ (OccurAttr_MinMaxOccur a b)
                    as ->do void . contextError $ SemanticError $ pack $ "Bad args to occurs" ++ show as;return def


validateFuncAttributes :: ListNode AttributeNode -> ValidatorS (FunctionAttr Expression)
validateFuncAttributes atts = do
    attrs <- catMaybes <$> validateList_ (validateAttributeNode funAttrs) atts
    size <- validateSizeAttributes attrs
    parts <- return $ if L_total `elem` map fst attrs then PartialityAttr_Total else PartialityAttr_Partial
    jectivity <- validateJectivityAttributes attrs
    return $  (FunctionAttr  size  parts  jectivity)

validateSeqAttributes :: ListNode AttributeNode -> ValidatorS (SequenceAttr Expression)
validateSeqAttributes atts = do
    attrs <- catMaybes <$> validateList_ (validateAttributeNode seqAttrs) atts
    size <- validateSizeAttributes attrs
    jectivity <- validateJectivityAttributes attrs
    return $ SequenceAttr size jectivity


validateRelationAttributes :: ListNode AttributeNode -> ValidatorS (RelationAttr Expression)
validateRelationAttributes atts = do
    setContextFrom atts
    attrs <- catMaybes <$> validateList_ (validateAttributeNode relAttrs) atts
    size <- validateSizeAttributes attrs
    others <- catMaybes <$> validateArray validateBinaryRel (filter (\x -> fst x `elem` map fst binRelAttrs) attrs)
    return $ RelationAttr size  (BinaryRelationAttrs $ S.fromList others )
        where
            validateBinaryRel :: (Lexeme , Maybe Expression) -> Validator BinaryRelationAttr
            validateBinaryRel (l,_) = do
                case lexemeToBinRel l of
                    Just b -> return . pure $ b
                    Nothing ->contextError $ InternalErrorS $ pack $ "Not found (bin rel) " ++ show l

validatePartitionAttributes :: ListNode AttributeNode -> ValidatorS (PartitionAttr Expression)
validatePartitionAttributes atts = do
    attrs <- catMaybes <$> validateList_ (validateAttributeNode partitionAttrs) atts
    --guard size attrs and complete as this is default
    size <- validateNumPartAttributes attrs
    partSize <- validatePartSizeAttributes attrs
    regular <- return $ L_regular `elem` map fst attrs
    return $ PartitionAttr size  partSize regular

validateAttributeNode :: Map Lexeme Bool -> AttributeNode -> Validator (Lexeme,Maybe Expression)
validateAttributeNode vs (NamedAttributeNode t Nothing) = do
    flagToken t TtModifier
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
      Just  True -> invalid $ t <!> CustomError "Argument required"
      Just False ->  return . pure $ (name , Nothing)

validateAttributeNode vs (NamedAttributeNode t (Just e)) = do
    flagToken t TtModifier
    setContextFrom e
    expr <- validateExpression e ?=> exactly tInt
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
      Just False -> invalid $ t <!> SemanticError "attribute %name% does not take an argument"
      Just True -> return . pure $(\x -> (name,Just x)) expr


validateNamedDomainInVariant :: NamedDomainNode -> ValidatorS (Name, TypedDomain)
validateNamedDomainInVariant (NameDomainNode name m_dom) = do
    name' <-  validateNameAs TtProperty name
    domain' <-case m_dom of
      Nothing ->  do return . Typed tInt $ DomainInt TagInt [RangeSingle 0]
      Just (l,d) -> do l `isA` TtOperator; validateDomain d
    return $ (name' ,  domain')

validateNamedDomainInRecord :: NamedDomainNode -> ValidatorS (Name, TypedDomain)
validateNamedDomainInRecord (NameDomainNode name m_dom) = do
    name' <-  validateNameAs TtProperty name
    domain' <-case m_dom of
      Just (l,d) ->l `isA` TtOperator >> validateDomain d
      Nothing -> do
        raiseError $ symbolRegion name <!> SemanticError "Dataless not allowed in record"
        (return (fallback "Dataless RecordMemeber"))
    return $  (name', domain')

validateRange ::Type -> RangeNode -> ValidatorS ((Range Expression))
validateRange t range = case range of
    SingleRangeNode en -> do setContextFrom en; ex <- validateExpression en ?=> exactly t; return $ RangeSingle ex
    OpenRangeNode dots -> do dots `isA` TtOther "Ellips" ; return  RangeOpen
    RightUnboundedRangeNode e1 dots -> do dots `isA` TtOther "Ellips" ;setContextFrom e1; ex <- validateExpression e1 ?=> exactly t  ; return $ RangeLowerBounded ex
    LeftUnboundedRangeNode dots e1 -> do dots `isA` TtOther "Ellips" ; setContextFrom e1; ex <- validateExpression e1 ?=> exactly t  ; return $ RangeUpperBounded ex
    BoundedRangeNode e1 dots e2 -> do
        dots `isA` TtOther "Ellips"
        setContextFrom e1
        e1' <- validateExpression e1 ?=> exactly t
        setContextFrom e2
        e2' <-  validateExpression e2 ?=> exactly t
        return $  RangeBounded e1' e2'

validateArrowPair :: ArrowPairNode -> Validator (RegionTagged (Typed Expression), RegionTagged (Typed Expression))
validateArrowPair (ArrowPairNode e1 s e2) = do
    s `isA` TtOperator
    e1' <-  validateExpression e1
    e2' <-  validateExpression e2
    return .pure $ (\a b->((symbolRegion e1,a),(symbolRegion e2,b))) e1' e2'

validateExpression :: ExpressionNode -> ValidatorS (Typed Expression)
validateExpression expr = case expr of
    Literal ln -> validateLiteral ln
    IdentifierNode nn -> validateIdentifierExpr nn
    MetaVarExpr tok -> do
        x <- validateMetaVar tok ;
        return $ Typed TypeAny $ ExpressionMetaVar x
    QuantificationExpr qen -> validateQuantificationExpression qen
    OperatorExpressionNode oen -> validateOperatorExpression oen
    DomainExpression dex -> validateDomainExpression dex
    ParenExpression (ParenExpressionNode l1 exp l2) -> checkSymbols [l1,l2] >> validateExpression exp
    AbsExpression (ParenExpressionNode l1 exp l2) -> do
        [l1,l2] `are` TtOperator
        setContextFrom exp
        exp' <- validateExpression exp ?=> exactly TypeAny
        return . Typed tInt $ mkOp TwoBarOp  [exp']
    FunctionalApplicationNode lt ln -> validateFunctionApplication  lt ln
    AttributeAsConstriant lt exprs -> validateAttributeAsConstraint lt exprs
    SpecialCase  scn ->  validateSpecialCase scn
    MissingExpressionNode lt -> do raiseError (lt <!> TokenError lt) ; return (fallback "Missing expression")


validateAttributeAsConstraint :: LToken -> ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateAttributeAsConstraint l1 exprs = do
    es <- map untype <$> validateList_ validateExpression exprs
    do
        flagToken l1 TtMethod
        Just lx <- validateSymbol l1
        let n = lookup (Name (lexemeText lx)) allSupportedAttributes
        r <- case (n,es) of
          (Just 1 , [e,v]) -> return . pure . Typed TypeBool  $ aacBuilder e lx (Just v)
          (Just 1 , _) -> invalid $  l1 <!> (SemanticError $ pack $ "Expected 2 args to " ++ (show lx)  ++ "got" ++ (show $ length es))
          (Just 0 , [e]) -> return . pure . Typed TypeBool $ aacBuilder e lx Nothing
          (Just 0 , _) -> invalid $ l1 <!> (SemanticError $ pack $ "Expected 1 arg to " ++ (show lx)  ++ "got" ++ (show $ length es))
          (_,_) -> invalid $ l1 <!> InternalErrorS "Bad AAC"
        return $ fromMaybe (fallback "bad AAC") r
    where
        aacBuilder e lx y= Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e (fromString (lexemeFace lx)) y

validateSpecialCase :: SpecialCaseNode -> ValidatorS (Typed Expression)
validateSpecialCase (ExprWithDecls l1 ex l2 sts l3) = do
    checkSymbols [l1,l2,l3] --TODO maybe add tokens
    Just conds <- validateProgramTree sts
    let decls =
            [ Declaration (FindOrGiven LocalFind nm dom)
            | Declaration (FindOrGiven Find nm dom) <- conds ]
    let cons = concat
            [ xs
            | SuchThat xs <- conds
            ]
    let locals = if null decls
                    then DefinednessConstraints cons
                    else AuxiliaryVars (decls ++ [SuchThat cons])
    (t,expr) <- typeSplit <$> validateExpression ex
    return . Typed t $ WithLocals  (expr)  locals

translateQnName :: Lexeme -> OpType
translateQnName qnName = case qnName of
    L_ForAll -> FunctionOp L_fAnd
    L_Exists -> FunctionOp L_fOr
    L_Sum -> FunctionOp L_Sum
    L_Product -> FunctionOp L_Product
    _        -> FunctionOp qnName

validateQuantificationExpression :: QuantificationExpressionNode -> ValidatorS (Typed Expression)
validateQuantificationExpression (QuantificationExpressionNode name pats over m_guard dot expr) =
    do
        dot `isA` TtKeyword
        scoped $ do
            flagToken name TtMacro
            name' <-  validateSymbol name
            over' <-  validateQuantificationOver pats over
            -- patterns <-  validateSequence_ validateAbstractPattern pats
            g' <- validateQuantificationGuard m_guard
            setContextFrom expr
            let (iType,rType) = case name' of
                    Just L_ForAll -> (tCondition,TypeBool)
                    Just L_Exists ->(tCondition,TypeBool)
                    Just L_Sum -> (exactly tInt,tInt)
                    Just L_Product -> (exactly tInt,tInt)
                    _ -> bug $ text ("Unkown quantifier " ++ show name')
            body <-  validateExpression expr ?=> iType
            let qBody =  Comprehension body  (over'++g')
            let result = Typed rType <$> (mkOp <$> (translateQnName <$> name') <*> pure  [qBody])
            return $ fromMaybe (fallback "Quantification error") result
    where
        validateQuantificationGuard :: Maybe QuanticationGuard -> ValidatorS [GeneratorOrCondition]
        validateQuantificationGuard Nothing = return []
        validateQuantificationGuard (Just (QuanticationGuard l1 exp) ) = do
            checkSymbols [l1]
            setContextFrom exp
            expr' <- validateExpression exp ?=> exactly TypeBool
            return $ [Condition expr']
        validateQuantificationOver :: Sequence AbstractPatternNode -> QuantificationOverNode -> ValidatorS [GeneratorOrCondition]
        validateQuantificationOver pats ( QuantifiedSubsetOfNode lt en ) = do
            lt `isA` TtOperator --TODO sets only
            ps <- sequenceElems pats
            exp <- validateExpression en
            let (t,e) = typeSplit exp
            pt <- projectionType (symbolRegion en) t
            apats <- unifyPatterns pt ps
            return [Generator $ GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet (e)) | pat <- apats]
        -- x in exp
        validateQuantificationOver pats ( QuantifiedMemberOfNode lt en ) = do
            lt `isA` TtKeyword
            ps <- sequenceElems pats
            exp <- validateExpression en
            let (t,e) = typeSplit exp
            pt <- projectionType (symbolRegion en) t
            apats <- unifyPatterns pt ps
            return [Generator $ GenInExpr pat e|pat <- apats]
        -- x : domain
        validateQuantificationOver pats ( QuantifiedDomainNode (OverDomainNode l1 dom) ) = do
            l1 `isA` TtOther "Colon in comprehension"
            ps <- sequenceElems pats
            (dType,dom') <- typeSplit <$> validateDomain dom
            pt <- projectionTypeDomain (symbolRegion dom) dType
            apats <- unifyPatterns pt ps
            return [ Generator $ GenDomainNoRepr pat dom'| pat <- apats]



validateMetaVar :: LToken -> ValidatorS String
validateMetaVar tok = do
    Just lx <- validateSymbol tok
    case lx of
        LMetaVar s -> return $ unpack s
        _ -> error $ "Bad MetaVar" ++ show lx

validateDomainExpression :: DomainExpressionNode -> ValidatorS (Typed Expression)
validateDomainExpression (DomainExpressionNode  l1 dom l2) = do
    [l1,l2]  `are` TtOther "Backtick"
    (tdom,dom') <- typeSplit <$> validateDomain dom
    return . Typed tdom $ Domain  dom'

validateFunctionApplication :: LToken -> ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateFunctionApplication name args = do
    args' <-  validateList validateExpression args
    flagToken name TtMethod
    Just name' <-  validateSymbol name
    setContextFrom args
    validateFuncOp name' args'



validateIdentifierExpr :: NameNode -> ValidatorS (Typed Expression)
validateIdentifierExpr name = do
    Just n <- validateIdentifier name
    t <- resolveReference (symbolRegion name,Name n)
    tagWithType name t
    return . Typed t $ Reference (Name n) Nothing

--TODO Adress the major hole in the type system current
validateOperatorExpression :: OperatorExpressionNode -> ValidatorS (Typed Expression)
validateOperatorExpression (PrefixOpNode lt expr) = do
    flagToken lt TtOperator
    Just op <-  validateSymbol lt 
    setContextFrom expr
    let (refT) = case op of
            L_Minus -> tInt
            L_ExclamationMark -> TypeBool
            _ -> bug . text $ "Unknown prefix op " ++ show op
    expr' <-  validateExpression expr ?=> exactly refT
    return . Typed refT $ mkOp (PrefixOp op) [expr']
    --lookup symbol
validateOperatorExpression (BinaryOpNode lexp op rexp) = do
    (lType,lExpr) <- typeSplit <$> validateExpression lexp
    (rType,rExpr) <- typeSplit <$> validateExpression rexp
    flagToken op TtOperator
    Just op' <-  validateSymbol op
    let resultValidator = binOpType op'
    let resultType = resultValidator lType rType
    return . Typed resultType  $ mkBinOp ( pack $ lexemeFace op') (lExpr) (rExpr)
validateOperatorExpression (PostfixOpNode expr pon) = do
    postFixOp <-  validatePostfixOp pon
    postFixOp expr

validatePostfixOp :: PostfixOpNode -> ValidatorS (ExpressionNode -> ValidatorS (Typed Expression))
validatePostfixOp (OpFactorial lt) = do
        lt `isA` TtOperator
        setContextFrom lt
        return  $ \exp -> do
            v <- validateExpression exp ?=> exactly tInt
            return $ Typed tInt $ mkOp FactorialOp [v]
validatePostfixOp (ApplicationNode args) = do
        return $ \exp -> do
            let reg = symbolRegion exp
            (t,e) <- typeSplit <$> validateExpression exp
            args' <- validateList validateExpression args
            case t of
                TypeFunction _ _ -> validateFuncOp L_image ((reg,Typed t e):args')
                TypeSequence _ -> validateFuncOp L_image ((reg,Typed t e):args')
                _ -> do
                    let underscore = Reference "_" Nothing
                    let ys = [if underscore == v then Nothing else Just x| x@(_,Typed _ v) <- args']
                    iType <- case t of
                        TypeRelation ts -> checkProjectionArgs ts ys
                        _ -> do
                                raiseTypeError $ symbolRegion exp <!> ComplexTypeError "Relation or function" t
                                let ts = map (maybe TypeAny (typeOf_ . snd)) ys
                                return  $ TypeRelation $ ts
                    let op = Op $ MkOpRelationProj $ OpRelationProj e (map (untype . snd <$>)  ys)
                    let resType = if any null ys then iType else TypeBool
                    return . Typed resType $ op
            where
                checkProjectionArgs :: [Type] -> [Maybe (RegionTagged (Typed Expression))] -> ValidatorS Type
                checkProjectionArgs ref bind= do
                    unless (length ref == length bind) $
                        raiseError $ symbolRegion args <!> SemanticError "Member size mismatch for relation"
                    let pairs = zip ref bind
                    let (free,bound) = partition (null.snd) pairs
                    mapM_ (\(t,x)-> case x of
                        Nothing -> pure ()
                        Just v -> void $ unifyTypes t v
                        ) bound
                    let freeTypes = map fst free
                    return $ if null freeTypes then TypeBool else TypeRelation freeTypes

validatePostfixOp (IndexedNode ln) = do
        res <- catMaybes <$> listElems ln
        return $ \exp -> do
            setContextFrom exp
            e <- validateExpression exp
            foldM validateIndexingOrSlicing e res


validatePostfixOp (ExplicitDomain l1 l2 dom l3) = do
    l1 `isA` TtOther "Colon in expr"
    [l2,l3] `are` TtOther "BackTick"
    (_,dom') <- typeSplit <$> validateDomain dom
    t <- case getDType dom' of
        Just t -> return t
        Nothing -> return TypeAny <* (raiseError $ symbolRegion  dom <!> InternalErrorS (pack ("Some type bug with:" ++ show dom')))
    return $ \exp -> do
        e <- validateExpression exp ?=> exactly t
        return . Typed t $ D.Typed e t
    where
        getDType :: Domain () Expression -> Maybe Type
        getDType d = let ?typeCheckerMode = StronglyTyped in typeOfDomain d


validateIndexingOrSlicing :: Typed Expression -> RangeNode -> ValidatorS (Typed Expression)
validateIndexingOrSlicing (Typed t exp) (SingleRangeNode r) = do
    setContextFrom r
    -- i <- validateExpression r ?=> exactly iType
    (vType,e) <- case t of
        TypeRecord ts -> validateRecordMemberIndex (ts) r
        TypeVariant ts-> validateRecordMemberIndex (ts) r
        t -> do
            t' <- getIndexingType t
            e <- (validateExpression r) ?=> (exactly t')
            setContextFrom r
            vType <- getIndexedType t (Typed t' e)
            return (vType,e)
    return . Typed vType $ Op $ MkOpIndexing (OpIndexing exp e)

validateIndexingOrSlicing exp range = do
    let (mType,m) = typeSplit exp
    setContextFrom range
    sType <- getSlicingType mType
    r' <- validateRange sType range
    let (i,j) =case r' of
            RangeOpen -> (Nothing,Nothing)
            RangeLowerBounded ex ->  (Just ex,Nothing)
            RangeUpperBounded ex ->  (Nothing,Just ex)
            RangeBounded exl exr ->  (Just exl,Just exr)
            RangeSingle ex ->  (Just ex,Just ex) -- This never gets hit in a well formed program
    return $ Typed mType $ Op $ MkOpSlicing (OpSlicing m i j)

validateRecordMemberIndex :: [(Name,Type)] -> ExpressionNode -> ValidatorS (Type,Expression)
validateRecordMemberIndex ns (IdentifierNode nn) = do
    n <- validateNameAs TtProperty nn
    let t = lookup n ns
    ty <- case t of
      Just ty -> return ty
      Nothing -> do
        raiseError $ symbolRegion nn <!> WithReplacements
            (SemanticError "Expected member of record/variant ")
            [x | (Name x,_) <- ns]
        return TypeAny
    return $ (ty,Reference n Nothing)
validateRecordMemberIndex ns (MissingExpressionNode nn) = do
    raiseError $ symbolRegion nn <!>
        WithReplacements
            (SemanticError "Expected member of record/variant ")
            [x | (Name x,_) <- ns]
    return (TypeAny,fallback "bad Index")
validateRecordMemberIndex ns en = do
    g <- validateExpression en
    let msg = T.concat
            [
                "Expected one of ",
                T.intercalate "," [x | (Name x,_) <- ns],
                " "
            ]
    raiseTypeError $ symbolRegion en <!> ComplexTypeError msg (typeOf_ g)
    return (TypeAny,untype g)




getSlicingType :: Type -> ValidatorS Type
getSlicingType TypeAny = return $ TypeAny
getSlicingType (TypeMatrix i _) = return i
getSlicingType (TypeSequence _) = return tInt
getSlicingType t = do
    contextTypeError (CustomError . pack $ "Type " ++ (show $ pretty t) ++ " does not support slicing")
    return TypeAny

getIndexingType :: Type -> ValidatorS Type
getIndexingType TypeAny = return $ TypeAny
getIndexingType (TypeMatrix i _) = return i
getIndexingType (TypeSequence _) = return tInt
getIndexingType (TypeList _) = return tInt
getIndexingType (TypeTuple _) = return tInt
getIndexingType t = do
    contextTypeError (CustomError . pack $ "Type " ++ (show $ pretty t) ++ " does not support indexing")
    return TypeAny

getIndexedType :: Type -> (Typed Expression) -> ValidatorS Type
getIndexedType (TypeMatrix _ ms) _  = return ms
getIndexedType (TypeSequence t) _   = return t
getIndexedType (TypeTuple ts) ex      = do
    case intOut "Index" (untype ex) of
        Left _ -> do
            (void . contextTypeError $ CustomError $ "Non constant value indexing tuple")
            return TypeAny
        Right v | v <= 0 || v > toInteger ( length ts) -> do
            (contextTypeError $ CustomError . pack $ "Tuple index "++ show v ++ " out of bounds" )
            return TypeAny
        Right v -> return $ ts `at` (fromInteger v -1)
getIndexedType t@(TypeRecord vs) (Typed _ e)   = return TypeAny --TODO implement
getIndexedType t@(TypeVariant _) _  = return TypeAny --TODO implement
getIndexedType _ _ = return TypeAny

validateLiteral :: LiteralNode -> ValidatorS (Typed Expression)
validateLiteral litNode = case litNode of
    IntLiteral lt -> validateIntLiteral lt >>= \x -> return $ Typed tInt $ Constant  x
    BoolLiteral lt -> validateBoolLiteral lt >>= \x -> return $ Typed TypeBool $ Constant  x
    MatrixLiteral mln -> validateMatrixLiteral mln
    TupleLiteralNode (LongTuple lt xs) ->  do
        lt `isA` TtType
        validateLiteral (TupleLiteralNodeShort (ShortTuple xs))
    TupleLiteralNodeShort (ShortTuple xs) -> do
        es <- validateExprList_ xs
        makeTupleLiteral es
    RecordLiteral lt ln -> lt `isA` TtType >> validateRecordLiteral ln
    VariantLiteral lt ln -> lt `isA` TtType >> validateVariantLiteral ln
    SetLiteral ls -> validateSetLiteral ls
    MSetLiteral lt ls -> lt `isA` TtType >> validateMSetLiteral ls
    FunctionLiteral lt ln -> lt `isA` TtType >> validateFunctionLiteral ln
    SequenceLiteral lt ln -> lt `isA` TtType >> validateSequenceLiteral ln
    RelationLiteral lt ln -> lt `isA` TtType >> validateRelationLiteral ln
    PartitionLiteral lt ln -> lt `isA` TtType >> validatePartitionLiteral ln

validateSequenceLiteral :: ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateSequenceLiteral x = do
    (t,ss) <- typeSplit <$> ( sameType =<< validateExprList x)
    let lType = TypeSequence t
    return . Typed  lType $  mkAbstractLiteral  $ AbsLitSequence ss


validateRelationLiteral :: ListNode RelationElemNode -> ValidatorS (Typed Expression)
validateRelationLiteral ln = do
    ms <- validateList_ validateRelationMember ln
    let members = map (map untype) ms
    return . Typed TypeAny  $ mkAbstractLiteral $ AbsLitRelation members
    where
        validateRelationMember :: RelationElemNode -> ValidatorS [Typed Expression]
        validateRelationMember x = case x of
          RelationElemNodeLabeled (LongTuple lt xs) ->  lt `isA` TtType >> validateExprList_ xs
          RelationElemNodeShort (ShortTuple xs) -> validateExprList_ xs


validatePartitionLiteral :: ListNode PartitionElemNode -> ValidatorS (Typed Expression)
validatePartitionLiteral ln = do
    members <- validateList validatePartitionElem ln
    (t,xs) <- typeSplit <$> sameType members
    let eType = TypePartition t
    return $ Typed eType (mkAbstractLiteral $ AbsLitPartition xs)
    where
        validatePartitionElem :: PartitionElemNode -> ValidatorS (Typed [Expression])
        validatePartitionElem (PartitionElemNode exprs) = do
            xs <- validateExprList exprs
            sameType xs


validateRecordLiteral :: ListNode RecordMemberNode -> ValidatorS (Typed Expression)
validateRecordLiteral ln = do
    members <- catMaybes <$> listElems ln
    case members of
        [] -> return $ Typed (TypeRecord []) (mk [])--REVIEW: should empty records be allowed?
        xs -> do
            (ns,unzip . map typeSplit->(ts,es)) <- mapAndUnzipM validateRecordMember xs
            let t =TypeRecord $ zip ns ts
            return $ Typed t $ mk (zip ns es)
    where
        mk = mkAbstractLiteral . AbsLitRecord
validateVariantLiteral :: ListNode RecordMemberNode -> ValidatorS (Typed Expression)
validateVariantLiteral ln = do
    members <- catMaybes <$> validateList_ (f2n validateRecordMember) ln
    res <- case members of
      [] -> invalid $ symbolRegion ln <!> SemanticError "Variants must contain exactly one member"
      [(n,Typed t v)]-> return . pure . Typed (TypeVariant [(n,t)]) $ mkAbstractLiteral $ AbsLitVariant Nothing n v
      _:_ -> invalid $ symbolRegion ln <!> SyntaxError "Variants must contain exactly one member" --tag subsequent members as unexpected 
    return $ fromMaybe (fallback "bad variant") res


validateRecordMember :: RecordMemberNode -> ValidatorS (Name,Typed Expression)
validateRecordMember (RecordMemberNode name lEq expr) = do
    lEq `isA` TtKeyword
    name' <-  validateName name
    expr' <-  validateExpression expr
    return ( name' , expr')

validateFunctionLiteral :: ListNode ArrowPairNode -> ValidatorS (Typed Expression)
validateFunctionLiteral ln = do
    pairs <- catMaybes <$> validateList_ validateArrowPair ln
    let (pl,pr) = unzip pairs
    (lhType,ls) <- typeSplit <$> sameType pl
    (rhType,rs) <- typeSplit <$> sameType pr
    let fType = TypeFunction lhType rhType
    return . Typed fType  $ mkAbstractLiteral $ AbsLitFunction $ zip ls rs

validateSetLiteral :: ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateSetLiteral ls = do
    xs <- validateList validateExpression ls
    (t,es) <- typeSplit <$> sameType xs
    return . Typed (TypeSet t) $ mkAbstractLiteral $ AbsLitSet es

validateMSetLiteral :: ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateMSetLiteral ls = do
    xs <- validateList validateExpression ls
    (t,es) <-typeSplit<$> sameType xs
    let eType = TypeMSet t
    let result = mkAbstractLiteral $ AbsLitMSet es
    return  $ Typed eType result


validateMatrixLiteral :: MatrixLiteralNode -> ValidatorS (Typed Expression)
--Matrix proper
validateMatrixLiteral (MatrixLiteralNode l1 se m_dom Nothing l2) = do
    [l1,l2] `are` TtOther "SquareBrackets"
    elems <-  validateSequence validateExpression se
    (t,es) <- typeSplit <$> sameType elems
    let defaultDomain :: TypedDomain = Typed tInt (mkDomainIntB 1 (fromInt $ genericLength elems))
    dom <- fromMaybe defaultDomain <$> validateOverDomain m_dom
    let lit = AbsLitMatrix (untype $ dom) es
    return $ Typed (TypeMatrix tInt t) $ mkAbstractLiteral lit
    where
        validateOverDomain :: Maybe OverDomainNode -> Validator TypedDomain
        validateOverDomain Nothing = return Nothing
        validateOverDomain (Just (OverDomainNode l3 dom)) = do l3 `isA` TtOther "Semicolon in matrix"; pure <$> validateDomain dom


-- Matrix as comprehension
validateMatrixLiteral (MatrixLiteralNode l1 se m_dom (Just comp) l2) = do
    [l1,l2] `are` TtOther "SquareBrackets"
    case m_dom of
        Nothing -> return ()
        Just p@(OverDomainNode l3 dom) -> do
            l3 `isA` TtOther "Semicolon in matrix"
            void $ validateDomain dom
            raiseError $ symbolRegion p <!> SemanticError "Index domains are not supported in comprehensions"
    scoped $
        do
            --check gens and put locals into scope
            gens <-  validateComprehension comp
            --now validate expression(s)
            es <- validateSequence validateExpression se
            Just r <- case es of
                    [] -> invalid $ symbolRegion se <!> SemanticError "MissingExpression"
                    ((_,x):xs) ->  flagExtraExpressions xs >> (return . pure $ x)
            let bodyType = typeOf_ r
            return . Typed (TypeList bodyType) $ Comprehension (untype r) gens
    where
        flagExtraExpressions :: [RegionTagged a] -> ValidatorS ()
        flagExtraExpressions []  = pure ()
        flagExtraExpressions xs  = raiseError $ catRegions xs <!> SemanticError "Comprehensension may have only one expression before |"


validateComprehension :: ComprehensionNode -> ValidatorS [GeneratorOrCondition]
validateComprehension (ComprehensionNode l1 body) = do
        l1 `isA` TtKeyword
        concat <$> validateSequence_ validateComprehensionBody body

validateComprehensionBody :: ComprehensionBodyNode -> ValidatorS [GeneratorOrCondition]
--guard
validateComprehensionBody (CompBodyCondition en) = do
    e <- validateExpression en
    setContextFrom en
    assertType e TypeBool "Guards must be bools"
    return [Condition $ untype e]
--x in dom
validateComprehensionBody (CompBodyDomain apn l1 dom) = do
    l1 `isA` TtKeyword
    (td,domain) <- typeSplit <$> validateDomain dom
    td' <- projectionTypeDomain (symbolRegion dom) td
    pats <- validateSequence_ (flip unifyPattern td' . Just) apn
    return  $ [Generator  (GenDomainNoRepr pat domain) | pat <- pats]

-- x <- expr
validateComprehensionBody (CompBodyGenExpr apn lt en) = do
    lt `isA` TtKeyword
    e <- validateExpression en
    let (t,exp) = typeSplit e
    t' <- projectionType (symbolRegion en) t
    pats <- validateSequence_ (flip unifyPattern t' . Just) (apn)
    return  $ [Generator (GenInExpr pat exp)| pat <- pats]
--letting x be
validateComprehensionBody (CompBodyLettingNode l1 nn l2 en) = do
    [l1,l2] `are` TtKeyword
    (t,expr) <- typeSplit <$> validateExpression en
    pat <- unifyPattern (Just nn) t
    return  [ComprehensionLetting pat expr]


projectionType :: DiagnosticRegion -> Type -> ValidatorS Type
projectionType r t = case t of
          TypeAny -> return  TypeAny
          TypeTuple tys -> return t
          TypeMatrix i ty -> return ty
          TypeList ty -> return ty
          TypeSet ty -> return ty
          TypeMSet ty -> return ty
          TypeSequence ty -> return $ TypeTuple [tInt,ty]
          TypeRelation ts -> return $ TypeTuple ts
          TypeFunction fr to -> return $ TypeTuple [fr,to]
          _ -> (raiseTypeError $ r <!> SemanticError  (pack $ "Expression of type " ++ (show $pretty t) ++ " cannot be projected in a comprehension")) >> return TypeAny
projectionTypeDomain :: DiagnosticRegion -> Type -> ValidatorS Type
projectionTypeDomain r t = case t of --TODO check and do properly
          TypeAny -> return  TypeAny
          TypeTuple tys -> return t
          TypeMatrix i ty -> return ty
          TypeInt t -> return $ TypeInt t
          TypeList ty -> return ty
          TypeSet ty -> return ty
          TypeMSet ty -> return ty
          TypeSequence ty -> return $ TypeTuple [tInt,ty]
          TypeRelation ts -> return $ TypeTuple ts
          TypeFunction fr to -> return $ TypeTuple [fr,to]
          _ -> (raiseTypeError $ r <!> SemanticError  (pack $ "Expression of type " ++ (show $pretty t) ++ " cannot be projected in a comprehension")) >> return TypeAny
mkAbstractLiteral :: AbstractLiteral Expression -> Expression
mkAbstractLiteral x = case e2c (AbstractLiteral x) of
                        Nothing -> AbstractLiteral x
                        Just c -> Constant c


enforceConstraint :: Maybe Bool -> String -> ValidatorS ()
enforceConstraint p msg = do
    case p of
        Just True-> return ()
        _ -> void (contextError (CustomError $ pack msg))



checkSymbols :: [LToken] -> ValidatorS ()
checkSymbols = mapM_ (\t -> validateSymbol t)

--Raise a non structural error (i.e type error)
raiseError :: ValidatorDiagnostic -> ValidatorS ()
raiseError e = tell [e]

raiseTypeError :: ValidatorDiagnostic -> ValidatorS ()
raiseTypeError e = do
    tc <- gets typeChecking
    unless (not tc) $ raiseError e
makeTupleLiteral :: [Typed Expression] -> ValidatorS (Typed Expression)
makeTupleLiteral members = do
    let memberTypes = unzip $ map typeSplit members
    let eType = TypeTuple (fst memberTypes)
    return . Typed eType . mkAbstractLiteral . AbsLitTuple $ snd memberTypes


validateIntLiteral :: LToken -> ValidatorS Constant
validateIntLiteral t = do
    flagToken t TtNumber
    l <- validateSymbol t
    case l of
        Just (LIntLiteral x) -> return $ ConstantInt TagInt x
        _ -> error "Bad int literal"

validateBoolLiteral :: LToken -> ValidatorS Constant
validateBoolLiteral t = do
    flagToken t $ TtOther "Boolean"
    Just l <- validateSymbol t
    case l of
        L_true -> return $ ConstantBool True
        L_false -> return $ ConstantBool False
        _ -> error "Bad bool literal"

validateNameList :: Sequence NameNode -> ValidatorS [RegionTagged Name]
validateNameList = validateSequence validateName

validateNameList_ :: Sequence NameNode -> ValidatorS [Name]
validateNameList_ = validateSequence_ validateName

validateIdentifier :: NameNode -> Validator Text
validateIdentifier (NameNode iden) = do
    Just q <-  validateSymbol iden
    case q of
        LIdentifier x -> checkName x
        _ -> return Nothing
    where
        checkName :: Text -> Validator Text
        checkName "" = invalid $ iden <!> SemanticError "Empty names not allowed"
        checkName "\"\"" = invalid $ iden <!> SemanticError  "Empty names not allowed"
        checkName x = return . pure $ x

validateName :: NameNode -> ValidatorS Name
validateName name = do
        n <- validateIdentifier name
        return $ fromMaybe (fallback "bad Identifier") (Name <$> n)

validateNameAs :: TagType -> NameNode -> ValidatorS Name
validateNameAs f name@(NameNode n) = do
        flagToken n f
        validateName name

listToSeq :: ListNode a -> ValidatorS (Sequence a)
listToSeq (ListNode l1 s l2) = checkSymbols [l1,l2] >> return s

--visit a sequence, return a list of elements, nothing if missing
sequenceElems :: (Flattenable a) => Sequence a -> ValidatorS [Maybe a]
sequenceElems (Seq els) = mapM (validateSequenceElem_ validateIdentity) els

listElems :: Flattenable a => ListNode a -> ValidatorS [Maybe a]
listElems = sequenceElems <=< listToSeq


validateIdentity :: a -> Validator a
validateIdentity = return . pure

validateArray :: (a -> ValidatorS b) -> [a] -> ValidatorS [b]
validateArray f l = mapM f l

validateList :: (Flattenable a,Fallback b) =>(a -> ValidatorS b) -> ListNode a -> ValidatorS [RegionTagged b]
validateList validator (ListNode st seq end) = do
    _ <- validateSymbol st
    _ <- validateSymbol end
    validateSequence validator seq

validateList_ :: (Flattenable a,Fallback b) =>(a -> ValidatorS b) -> ListNode a -> ValidatorS [b]
validateList_ validator (ListNode st seq end) = do
    _ <- validateSymbol st
    _ <- validateSymbol end
    validateSequence_ validator seq

-- mapPrefixToOp :: Lexeme -> Text
-- mapPrefixToOp x = case x of
--     L_Minus -> "negate"
--     L_ExclamationMark -> "not"
--     _ -> pack $ lexemeFace x

validateSequence :: (Flattenable a,Fallback b) =>(a -> ValidatorS b) -> Sequence a -> ValidatorS [RegionTagged b]
validateSequence f (Seq vals) = validateArray (validateSequenceElem f) vals
validateSequence_ :: (Flattenable a,Fallback b) =>(a -> ValidatorS b) -> Sequence a -> ValidatorS [b]
validateSequence_ f s = do
    q <- validateSequence f s
    return . map snd $ q

validateSequenceElem :: (Flattenable a,Fallback b) => (a -> ValidatorS b) -> SeqElem a -> ValidatorS (RegionTagged b)
validateSequenceElem f (SeqElem i s) = do
                            case s of
                              Nothing -> pure ()
                              Just lt -> void $ validateSymbol lt
                            v <- f i
                            return (symbolRegion i,v)
validateSequenceElem _ (MissingSeqElem plc sep) = do
    checkSymbols [sep]
    raiseError $ symbolRegion plc <!> TokenError plc
    return $ (symbolRegion plc , fallback "Missing elem")


validateSequenceElem_ :: (Flattenable a,Fallback b) => (a -> ValidatorS b) -> SeqElem a -> ValidatorS (b)
validateSequenceElem_ f (SeqElem i s) = do
                            case s of
                              Nothing -> pure ()
                              Just lt -> void $ validateSymbol lt
                            f i
validateSequenceElem_ _ (MissingSeqElem plc sep) = do
    checkSymbols [sep]
    raiseError $ symbolRegion plc <!> TokenError plc
    return $ fallback "Missing Elem"

validateExprList :: ListNode ExpressionNode -> ValidatorS [RegionTagged (Typed Expression)]
validateExprList = validateList validateExpression
validateExprList_ :: ListNode ExpressionNode -> ValidatorS [Typed Expression]
validateExprList_ = validateList_ validateExpression


offsetPositionBy :: Int -> SourcePos -> SourcePos
offsetPositionBy amt sp@(SourcePos _ _ (unPos->r)) = sp {sourceColumn=mkPos (amt+r) }

data DiagnosticRegion = DiagnosticRegion {
    drSourcePos::SourcePos,
    drEndPos :: SourcePos,
    drOffset :: Int,
    drLength :: Int
} | GlobalRegion
    deriving Show
-- getTokenRegion :: LToken -> DiagnosticRegion
-- getTokenRegion a =  do
--         let h =case a of
--               RealToken et -> et
--               MissingToken et -> et
--               SkippedToken et -> et
--         let start = tokenSourcePos h
--         let offset = tokenStart h
--         let tLength =case a of
--               RealToken _ -> trueLength h
--               MissingToken _ -> 1
--               SkippedToken _ -> trueLength h
--         DiagnosticRegion start (offsetPositionBy tLength start) offset tLength

symbolRegion :: Flattenable a => a -> DiagnosticRegion
symbolRegion a = case range of
        (h :<| rst) -> do
                let end =case viewr rst of
                        EmptyR -> h
                        _ :> et -> et
                let start = tokenSourcePos h
                let offset = tokenStart h
                let tLength = let some :|> last = range in sum (totalLength <$> some) + trueLength last --TODO Tidy up
                let en = tokenSourcePos end
                DiagnosticRegion start (offsetPositionBy (trueLength end) en) offset tLength
        _ -> GlobalRegion
        where range :: Seq ETok = flatten a


(<!>) :: WithRegion a => a -> ErrorType -> ValidatorDiagnostic
t <!> e = ValidatorDiagnostic (getRegion t) $ Error e

(/!\) :: WithRegion a  => a -> WarningType -> ValidatorDiagnostic
t /!\ e = ValidatorDiagnostic (getRegion t) $ Warning e

(<?>) :: WithRegion a  => a -> InfoType -> ValidatorDiagnostic
t <?> e = ValidatorDiagnostic (getRegion t) $ Info e

(<?!>) :: WithRegion a  => Maybe a -> ErrorType -> ValidatorDiagnostic
Nothing <?!> e =  ValidatorDiagnostic GlobalRegion $ Error e
Just t <?!> e =  t <!> e

contextError :: ErrorType -> Validator a
contextError e = do
    q <- getContext
    invalid $ ValidatorDiagnostic q $ Error e

contextTypeError :: ErrorType -> ValidatorS ()
contextTypeError e = do
    q <- getContext
    tc <- gets typeChecking
    when tc $ raiseError $ ValidatorDiagnostic q $ Error e

contextInfo :: InfoType -> ValidatorS ()
contextInfo e = do
    q <- getContext
    tell $ [ValidatorDiagnostic q $ Info e]
    return ()

getType :: (Pretty a ,TypeOf a) => a -> ValidatorS Type
getType a = do
        tc <- gets typeChecking
        (if tc then (do
           let t = let ?typeCheckerMode = StronglyTyped  in typeOf a
           case t of
               Left err -> do
                   void $ contextError (CustomError . pack $ "type err in :" ++ show (pretty a) ++ "err:" ++ show err)
                   return  TypeAny
               Right t -> return t) else return TypeAny)


assertType :: (Pretty a,TypeOf a) => Typed a -> Type -> Text -> ValidatorS ()
assertType v ref msg = do
    let Typed t _ = v
    tc <- gets typeChecking
    unless (not tc || t == ref) $ void . contextError $ CustomError msg

resolveReference :: RegionTagged Name -> ValidatorS Type
resolveReference (r,Name n) = do
    c <- getSymbol n
    case c of
      Nothing -> raiseTypeError (r <!> (CustomError . pack $ "Symbol not found "++ show n)) >> return TypeAny
      Just (reg,_,t) -> do
        addRegion (RegionInfo {rRegion=r,rText=n, rType=t, rDeclaration=Ref reg})
        return t
resolveReference _ = return TypeAny

sameType :: [RegionTagged (Typed a)] -> ValidatorS (Typed [a])
sameType [] = return $ Typed TypeAny []
sameType xs@(x:_) = do
    let ?typeCheckerMode = StronglyTyped
    let t = mostDefined $ map (typeOf_.snd) xs
    let t' = mostDefined [t , typeOf_ $ snd x] --Do this again to set type to first elem if possible 
    xs' <- mapM (unifyTypes t') xs
    return $ Typed t' xs'

typesUnifyS :: [Type] -> Bool
typesUnifyS = let ?typeCheckerMode=StronglyTyped in typesUnify

mostDefinedS :: [Type] -> Type
mostDefinedS [] = TypeAny
mostDefinedS [x] =  x
mostDefinedS (x:xs) = let ?typeCheckerMode=StronglyTyped in case mostDefined (xs++[x]) of
                                                                TypeAny -> x
                                                                t -> t

unifyTypes :: Type -> RegionTagged (Typed a) -> ValidatorS a
unifyTypes _ (r,Typed TypeAny a) = do raiseError (r /!\ UnclassifiedWarning "TypeAny used") >> return a
unifyTypes t (r,Typed t' a) = do
    let ?typeCheckerMode = StronglyTyped
    if typesUnify [t', t] then pure () else raiseTypeError $ r <!> TypeError t t'
    return a

unifyTypesFailing :: Type -> RegionTagged (Typed a) -> Validator a
unifyTypesFailing _ (r,Typed TypeAny a) = do raiseError (r /!\ UnclassifiedWarning "TypeAny used") >> (return $ Just a)
unifyTypesFailing t (r,Typed t' a) = do
    tc <- gets typeChecking
    let ?typeCheckerMode = StronglyTyped
    if not tc || typesUnify [t', t]  then return $ Just a else invalid (r <!> TypeError t t')


scoped :: ValidatorS a -> ValidatorS a
scoped m = do
    st <- gets symbolTable
    res <- m
    modifySymbolTable $ const st
    return res

unifyPatterns :: Type -> [Maybe AbstractPatternNode] -> ValidatorS [AbstractPattern]
unifyPatterns t =  mapM (flip unifyPattern t)

unifyPattern :: Maybe AbstractPatternNode -> Type -> ValidatorS AbstractPattern
unifyPattern  (Just (AbstractIdentifier nn@(NameNode lt))) t = do
    (Name n) <- validateNameAs TtParameter nn
    -- traceM $ show n ++ ":" ++ show t
    --REVIEW don't put symbol if _ ?
    void $ putSymbol (Name n,(symbolRegion nn,False,t))
    addRegion (RegionInfo (symbolRegion nn) t n Definition)
    return  $ Single $  Name n

unifyPattern (Just(AbstractMetaVar lt)) _ = do
    s <- validateMetaVar lt
    return $ AbstractPatternMetaVar s

unifyPattern (Just(AbstractPatternTuple m_lt ln)) t = do
    sps <-listToSeq ln
    ps <-sequenceElems sps
    case m_lt of
        Nothing -> void $ return ()
        Just lt -> lt `isA` TtType
    memberTypes <- getMemberTypes t
    let q = zip ps memberTypes
    aps <- mapM (uncurry unifyPattern) q
    return  $ AbsPatTuple aps

unifyPattern (Just(AbstractPatternMatrix ln)) t = do
    sps <-listToSeq ln
    ps <-sequenceElems sps
    memberTypes <- getMemberTypes t
    let q = zip ps memberTypes
    aps <- mapM (uncurry unifyPattern) q
    return  $ AbsPatMatrix aps

unifyPattern (Just(AbstractPatternSet ln)) t = do
    sps <-listToSeq ln
    ps <-sequenceElems sps
    memberTypes <- getMemberTypes t
    let q = zip ps memberTypes
    aps <-mapM (uncurry unifyPattern) q
    return $ AbsPatSet aps

unifyPattern Nothing _ = return . Single $ fallback "No Pattern"


catRegions :: [RegionTagged a] -> DiagnosticRegion
catRegions [] = GlobalRegion
catRegions xs = DiagnosticRegion {
    drSourcePos=drSourcePos .fst  $ head xs,
    drEndPos=drEndPos .fst  $ last xs,
    drOffset=drOffset.fst $ head xs,
    drLength=sum $ map (drLength.fst) xs
    }


getMemberTypes :: Type -> ValidatorS [Type]
getMemberTypes t = case t of
  TypeAny -> return $ repeat TypeAny
--   TypeUnnamed na -> 
  TypeTuple tys -> return tys
  _ -> return $ repeat TypeAny
-- unifyAbstractPatternOverExpression :: AbstractPatternNode -> Expression -> Validator (Name,Type)
-- unifyAbstractPatternOverExpression pat exp = do
--     t <- typeOf exp

--     empty

data DomainTyped a = DomainTyped DomainType a

data DomainSize = Unkown | Infinite | Sized Int
data DomainType
    = DomainTypeBool
    | DomainTypeInt DomainSize
    | DomainTypeTuple [DomainType]

f2n :: (a -> ValidatorS b) -> a ->Validator b
f2n f a = do
    q <- f a
    return $ Just q

class Fallback a where
    fallback :: Text -> a

instance Fallback (Domain () Expression) where
    fallback reason = DomainAny reason TypeAny

instance Fallback Expression where
    fallback reason = Reference (Name reason) Nothing

instance (Fallback a) => Fallback (Typed a) where
    fallback = Typed TypeAny . fallback

instance Fallback (Maybe a) where
    fallback = const Nothing
instance Fallback Name where
    fallback = Name


instance Fallback [a] where
    fallback :: Text -> [a]
    fallback = const []

instance Fallback AbstractPattern where
    fallback = Single . fallback

type FuncOpDec = (Int)


funcOpBuilder :: Lexeme -> [Arg] -> ValidatorS (Typed Expression)
funcOpBuilder l = (functionOps l) (mkOp $ FunctionOp l)
-- functionOps l@L_fAnd = (validateArgList [isLogicalContainer],const TypeBool)
functionOps :: Lexeme -> ([Expression] -> Expression) -> [Arg] -> ValidatorS (Typed Expression)
functionOps l = case l of
    L_fAnd -> unFunc isLogicalContainer (pure . const TypeBool)
    L_fOr ->   unFunc isLogicalContainer (pure . const TypeBool)
    L_fXor ->  unFunc isLogicalContainer (pure . const TypeBool)
    L_Sum ->   unFunc sumArgs (pure . const tInt)
    L_Product ->   unFunc sumArgs (pure . const tInt)
    L_true -> unFunc anyType (pure . const TypeBool)
    L_toInt -> unFunc (only TypeBool) (pure . const tInt)
    L_makeTable -> unFunc (only TypeBool) (pure . const TypeBool)
    L_table -> biFunc tableArgs (const2 TypeBool)
    L_gcc -> triFunc (each3 listInt) (const3 TypeBool)
    L_atleast -> triFunc (each3 listInt) (const3 TypeBool)
    L_atmost -> triFunc (each3 listInt) (const3 TypeBool)
    L_defined -> unFunc funcSeq funcDomain
    L_range -> unFunc funcSeq funcRange
    L_restrict -> biFunc restrictArgs restrictTypes
    L_allDiff -> unFunc listOrMatrix (const $ pure TypeBool)
    L_alldifferent_except -> biFunc (indep listOrMatrix enumerable) (const2 TypeBool)
    L_catchUndef ->  biFunc unifies (\a b -> pure $ mostDefinedS $ map typeOf_ $ catMaybes [a,b])
    L_dontCare -> unFunc anyType (const $ pure TypeBool)
    L_toSet -> unFunc toSetArgs typeToSet
    L_toMSet -> unFunc toMSetArgs typeToMSet
    L_toRelation -> unFunc func typeToRelation
    L_max -> unFunc minMaxArgs minMaxType
    L_min -> unFunc minMaxArgs minMaxType
    L_image -> biFunc imageArgs (const . funcRange)
    L_transform -> biFunc transformArgs (const (typeOf_ <$>))
    L_imageSet -> biFunc imSetArgs (\a -> const $ TypeSet <$> funcRange a)
    L_preImage -> biFunc preImageArgs (\a -> const $ TypeSet <$> funcDomain a)
    L_inverse -> biFunc inverseArgs (const2 TypeBool)
    L_freq -> biFunc freqArgs (const2 tInt)
    L_hist -> unFunc histArgs histType
    L_parts -> unFunc part partsType
    L_together -> biFunc setPartArgs (const2 TypeBool)
    L_apart -> biFunc setPartArgs (const2 TypeBool)
    L_party -> biFunc partyArgs partyType
    L_participants -> unFunc part partInner
    L_active -> biFunc activeArgs (const2 TypeBool)
    L_pred -> unFunc enumerable enumerableType
    L_succ -> unFunc enumerable enumerableType
    L_factorial -> unFunc (only tInt) (const $ pure tInt)
    L_powerSet -> unFunc set powerSetType
    L_concatenate -> unFunc concatArgs concatType
    L_flatten -> \ b a -> case a of
                            [] -> (unFunc (unaryFlattenArgs) (flattenType Nothing)) b a
                            [_] -> (unFunc (unaryFlattenArgs) (flattenType Nothing)) b a
                            _ -> (biFunc (binaryFlattenArgs) (\(getNum->a) -> flattenType a)) (b) a
    _  -> bug $ text $ "Unkown functional operator " ++ show l
    where
        valid = return $ pure ()
        const2 = const.const . pure
        const3 = const.const.const . pure
        getNum :: Maybe (Typed Expression) -> Maybe Int
        getNum (Just(Typed _ x)) = case intOut "" x of
                                    Nothing -> Nothing
                                    Just n -> pure $ fromInteger n
        getNum _ = Nothing
        each3 f a b c= f a >> f b >> f c
        anyType = const . return $ Just  ()


        indep :: (Arg -> Validator ()) -> (Arg -> Validator ()) -> (Arg -> Arg -> Validator ())
        indep f1 f2 a b = do
            v1 <- f1 a
            v2 <- f2 b
            if null $ catMaybes $ [v1,v2] then return $ pure () else return Nothing
        binaryFlattenArgs :: Arg -> Arg -> Validator ()
        binaryFlattenArgs (r1,d) b = do
            off <- case intOut "" (untype d) of
                        Just (fromInteger->a) | a > 0 -> return $ Just a
                        _ -> invalid $ r1 <!> CustomError "1st arg must be a constant positive int"
            let ref = map (const TypeList) [1..fromMaybe 1 (off)]
            let ref' = foldr id TypeAny ref
            r <- unifyTypesFailing ref' b
            return $ if null off || null r then  Nothing else Just ()
        unaryFlattenArgs :: Arg -> Validator ()
        unaryFlattenArgs (r,typeOf_->(TypeMatrix _ _)) = valid
        unaryFlattenArgs (r,typeOf_->(TypeList _)) = valid
        unaryFlattenArgs (r,typeOf_->TypeAny) = valid
        unaryFlattenArgs (r,typeOf_->t) = invalid $ r <!> ComplexTypeError "List or Matrix " t

        concatType :: Maybe (Typed Expression) -> Maybe Type
        concatType (fmap typeOf_->Just(TypeMatrix _ (TypeList t))) = Just $ TypeList t
        concatType (fmap typeOf_->Just(TypeMatrix _ (TypeMatrix _ t))) = Just $ TypeList t
        concatType (fmap typeOf_->Just(TypeList (TypeList t))) = Just $ TypeList t
        concatType (fmap typeOf_->Just(TypeList (TypeMatrix _ t))) = Just $ TypeList t
        concatType _ = Just $ TypeList TypeAny
        concatArgs :: Arg -> Validator ()
        concatArgs = binaryFlattenArgs (GlobalRegion,Typed tInt $ Constant $ ConstantInt TagInt 1)
        tableArgs :: Arg -> Arg -> Validator ()
        tableArgs (r1,typeOf_->t1) (r2,typeOf_->t2) = do
            a <- case t1 of
                t | isValidInner t -> valid
                _ -> invalid $ r1 <!> ComplexTypeError "Matrix of Int/Enum" t1
            b <-  case t2 of
                TypeAny -> valid
                TypeList t | isValidInner t-> valid
                TypeMatrix _ t | isValidInner t-> valid
                _ -> invalid $ r2 <!> ComplexTypeError "Matrix of Matrix of Int/Enum" t2

            return $ if null a || null b then Nothing else Just ()
            where
                isValidInner t = case t of
                    TypeAny -> True
                    TypeList TypeInt{} -> True
                    TypeList  TypeAny-> True
                    TypeMatrix _ TypeInt{} -> True
                    TypeMatrix _ TypeAny -> True
                    _ -> False

        toMSetArgs :: Arg -> Validator ()
        toMSetArgs (r,typeOf_-> a) = case a of
          TypeAny -> return $ pure ()
          TypeList _ -> return $ pure ()
          TypeMatrix {} -> return $ pure ()
          TypeMSet {} -> return $ pure ()
          TypeSet {} -> return $ pure ()
          TypeFunction {} -> return $ pure ()
          TypeRelation {} -> return $ pure ()
          _ -> invalid $ r <!> ComplexTypeError "Matrix ,list,function,relation,mset,set " a
        toSetArgs :: Arg -> Validator ()
        toSetArgs (r,typeOf_-> a) = case a of
          TypeAny -> return $ pure ()
          TypeList _ -> return $ pure ()
          TypeMatrix {} -> return $ pure ()
          TypeMSet {} -> return $ pure ()
          TypeFunction {} -> return $ pure ()
          TypeRelation {} -> return $ pure ()
          _ -> invalid $ r <!> ComplexTypeError "Matrix ,list,function,relation,mset " a
        listOrMatrix :: Arg -> Validator ()
        listOrMatrix (r,typeOf_-> a) = case a of
          TypeAny -> return $ pure ()
          TypeList _ -> return $ pure ()
          TypeMatrix {} -> return $ pure ()
          _ -> invalid $ r <!> ComplexTypeError "Matrix or list" a
        freqArgs :: Arg -> Arg -> Validator ()
        freqArgs (r1,a) (r2,b) = do
            let tb = typeOf_ b
            let (rt,ti) = case typeOf_ a of
                    TypeMatrix idx ms -> (TypeMatrix idx md,md) where md = mostDefinedS [tb,ms]
                    TypeMSet ms -> (TypeMSet md,md) where md = mostDefinedS [tb,ms]
                    _ -> (TypeMatrix tInt tb,tb)
            a' <- unifyTypesFailing rt (r1,a)
            b' <- unifyTypesFailing ti (r2,b)
            return $ if null a' || null b' then  Nothing else Just ()

        unifies :: Arg -> Arg -> Validator ()
        unifies a b = do
            let md = mostDefinedS $ map (typeOf_.unregion) [a,b]
            a' <- unifyTypesFailing md a
            b' <- unifyTypesFailing md b
            return $ if null a' || null b' then Nothing else Just ()
        func :: Arg -> Validator ()
        func (_,Typed (TypeFunction _ _) _) = valid
        func (_,Typed TypeAny _) = valid
        func (r,Typed t _) = invalid $ r <!> TypeError (TypeFunction TypeAny TypeAny) t
        set :: Arg -> Validator Type
        set (_,Typed (TypeSet t) _) = return $ pure t
        set (_,Typed TypeAny _) = return $ pure TypeAny
        set (r,Typed t _) = invalid $ r <!> TypeError (TypeSet TypeAny) t

        powerSetType (Just (Typed (TypeSet i) _)) = Just $ TypeSet (TypeSet i)
        powerSetType _ = Just $ TypeSet $ TypeSet TypeAny

        only t (r,typeOf_->t')= do setContext r; if t'==TypeAny || t == t' then return $ Just t else invalid $ r <!> TypeError t t'

        listInt (r,typeOf_->t') = case t' of
          TypeAny -> return $ Just t'
          TypeList TypeInt{} -> return $ Just t'
          TypeMatrix ty TypeInt{} -> return $ Just t'
          _ -> invalid $ r <!> ComplexTypeError "Matrix or list of int or enum" t'
        partInner :: Maybe (Typed Expression) -> Maybe Type
        partInner (fmap typeOf_->Just (TypePartition a)) = Just $ TypeSet a
        partInner _ = Just $ TypeSet TypeAny

        restrictArgs :: Arg -> Arg -> Validator ()
        restrictArgs (r1,t1) (r2,t2) = valid --TODO
        restrictTypes :: Maybe (Typed Expression) -> Maybe (Typed Expression) -> Maybe Type
        restrictTypes t1 t2 = Nothing --TODO
        imSetArgs :: Arg -> Arg -> Validator ()
        imSetArgs (r1,a) (r2,b) = do
            let t = case (typeOf_ a,typeOf_ b) of
                    (TypeFunction i _,tb) -> mostDefinedS [i,tb]
                    (TypeSequence _,_) -> tInt
                    (_,tb ) -> tb
            a' <- unifyTypesFailing (TypeFunction t TypeAny) (r1,a)
            b' <- unifyTypesFailing t (r2,b)
            return $ if null a' || null b' then  Nothing else Just ()
        preImageArgs :: Arg -> Arg -> Validator ()
        preImageArgs (r1,a) (r2,b) = do
            let t = case (typeOf_ a,typeOf_ b) of
                    (TypeFunction _ i,tb) -> mostDefinedS [i,tb]
                    (TypeSequence i,_) -> i
                    (_,tb ) -> tb
            a' <- unifyTypesFailing (TypeFunction TypeAny t) (r1,a)
            b' <- unifyTypesFailing t (r2,b)
            return $ if null a' || null b' then  Nothing else Just ()

        partyArgs :: Arg -> Arg -> Validator ()
        partyArgs (r1,a) (r2,b) = do
            let t = case (typeOf_ a,typeOf_ b) of
                    (ta,TypePartition tb) -> mostDefinedS [ta,tb]
                    (ta,_ ) -> ta
            a' <- unifyTypesFailing (t) (r1,a)
            b' <- unifyTypesFailing (TypePartition t) (r2,b)
            return $ if null a' || null b' then  Nothing else Just ()

        inverseArgs :: Arg -> Arg -> Validator ()
        inverseArgs (r1,a) (r2,b) = do
            let (fi,fo) = case (typeOf_ a,typeOf_ b) of
                    (TypeFunction fi fo,TypeFunction gi go) -> (mostDefinedS [fi,go],mostDefinedS [fo,gi])
                    (TypeFunction fi fo,_ ) -> (fi,fo)
                    (_,TypeFunction gi go) -> (gi,go)
                    _ -> (TypeAny,TypeAny)
            a' <- unifyTypesFailing (TypeFunction fi fo) (r1,a)
            b' <- unifyTypesFailing (TypeFunction fo fi) (r2,b)
            return $ if null a' || null b' then  Nothing else Just ()
        setPartArgs :: Arg -> Arg -> Validator ()
        setPartArgs (r1,a) (r2,b) = do
            let t  = case (typeOf_ a,typeOf_ b) of
                    (TypeSet st,TypePartition pt) -> mostDefinedS [st,pt]
                    (TypeSet st,_) -> st
                    (_,TypePartition ts) -> ts
                    _ -> TypeAny
            a' <- unifyTypesFailing (TypeSet t) (r1,a)
            b' <- unifyTypesFailing (TypePartition t) (r2,b)
            return $ if null a' || null b' then  Nothing else Just ()

        partyType ::  Maybe (Typed Expression) ->Maybe (Typed Expression) -> Maybe Type
        partyType a b = do
            let at = case typeOf_ <$> a of
                        Just (TypePartition t) -> t
                        _ -> TypeAny
            let bt = maybe TypeAny typeOf_ b
            return $ TypeSet $ mostDefinedS [at,bt]
        partsType ::  Maybe (Typed Expression) -> Maybe Type
        partsType (fmap typeOf_->Just (TypePartition a)) = Just $ TypeSet $ TypeSet a
        partsType (fmap typeOf_->Just TypeAny) = Just $ TypeSet $ TypeSet TypeAny
        partsType _ = Nothing
        minMaxArgs :: Arg -> Validator ()
        minMaxArgs (r,(Typed t e)) | Just (dom :: Domain () Expression) <- project e =
            case t of
                TypeInt TagInt -> valid
                TypeInt (TagEnum _) -> valid
                TypeEnum {} -> valid
                TypeAny -> valid
                _ -> invalid $ r <!> ComplexTypeError "Domain of int-like or matrix of int-like" t
        minMaxArgs (r,Typed t _) = do
            inner <- case t of
                TypeList tyInner -> return tyInner
                TypeMatrix _ tyInner -> return tyInner
                TypeSet tyInner -> return tyInner
                TypeMSet tyInner -> return tyInner
                TypeAny -> return TypeAny
                _ -> return TypeAny <* invalid (r <!> ComplexTypeError "Domain of int-like or matrix of int-like" t)
            case inner of
                TypeInt TagInt -> valid
                TypeInt (TagEnum _) -> valid
                TypeEnum {} -> valid
                TypeAny -> valid
                _ -> invalid $ r <!> ComplexTypeError "Domain of int-like or matrix of int-like" t

        minMaxType :: Maybe (Typed Expression) -> Maybe Type
        minMaxType (Just (Typed t@(TypeInt _) _)) = Just t
        minMaxType (Just (Typed t@(TypeEnum {}) _)) = Just t
        minMaxType (Just (Typed (TypeMatrix _ a) v)) = minMaxType (Just (Typed a v))
        minMaxType (Just (Typed (TypeList  a) v)) =  minMaxType (Just (Typed a v))
        minMaxType (Just (Typed (TypeSet  a) v)) =  minMaxType (Just (Typed a v))
        minMaxType (Just (Typed (TypeMSet a) v)) =  minMaxType (Just (Typed a v))
        minMaxType _ = Just TypeAny

        transformArgs :: Arg -> Arg -> Validator ()
        transformArgs a b = do
            return $ pure ()
        activeArgs :: Arg -> Arg -> Validator ()
        activeArgs (r,(typeOf_->t@(TypeVariant vs))) b = do
            void <$> unifyTypesFailing t b --todo this could be better
        activeArgs (r,(typeOf_->TypeAny)) b =valid
        activeArgs (r,(typeOf_->t)) b = invalid $ r <!> ComplexTypeError "Variant " t

        typeToSet :: Maybe (Typed Expression) -> Maybe Type
        typeToSet (Just (typeOf_->t)) = TypeSet <$> tMembers t
        typeToSet _ = Nothing
        typeToMSet :: Maybe (Typed Expression) -> Maybe Type
        typeToMSet (Just (typeOf_->t)) = TypeMSet <$> tMembers t
        typeToMSet _ = Nothing
        typeToRelation :: Maybe (Typed Expression) -> Maybe Type
        typeToRelation (Just(typeOf_->(TypeFunction i j))) = Just $ TypeRelation [i,j]
        typeToRelation (Just(typeOf_->TypeAny)) = Just $ TypeRelation [TypeAny,TypeAny]
        typeToRelation _ = Nothing
        tMembers t = case t of
                        TypeAny ->  Just TypeAny
                        TypeList ty -> Just ty
                        TypeMatrix _ i -> Just i
                        TypeSet ty -> Just ty
                        TypeMSet ty -> Just ty
                        TypeFunction i j -> Just $ TypeTuple [i,j]
                        TypeRelation tys -> Just $ TypeTuple tys
                        _ -> Nothing

        imageArgs :: Arg -> Arg -> Validator ()
        imageArgs (r1,typeOf_->t1) r2 = do
            Just from <- case t1 of
                TypeAny -> return $ Just TypeAny
                TypeFunction a _ -> return $Just a
                TypeSequence _ -> return $Just tInt
                _ -> (return Nothing) <* (raiseTypeError $ (r1 <!> ComplexTypeError "Function or Sequence" t1))
            _ <- unifyTypes from r2
            return $ pure ()

        sumArgs :: Arg -> Validator ()
        sumArgs (r,typeOf_->t') = do
            t <- case t' of
                TypeAny -> return TypeAny
                TypeList t -> return t
                TypeMatrix _ t -> return t
                TypeSet t -> return t
                TypeMSet t -> return t
                _ -> do
                    (raiseTypeError $ r <!> ComplexTypeError "Matrix or Set" t')
                    fail ""
            case t of
              TypeAny -> return $ pure ()
              TypeInt TagInt -> return $ pure ()
              _ -> return Nothing <* raiseTypeError (r <!> ComplexTypeError "Integer elements" t)
        funcSeq :: Arg -> Validator ()
        funcSeq (r,typeOf_->t') = case t' of
            TypeAny -> return $ pure ()
            TypeSequence _ -> return $ pure ()
            TypeFunction _ _ -> return $ pure ()
            _ -> invalid $ r <!> ComplexTypeError "Function or Sequence" t'
        funcDomain :: Maybe (Typed a) -> Maybe Type
        funcDomain (Just (typeOf_->(TypeFunction a _))) = Just a
        funcDomain (Just (typeOf_->(TypeSequence _))) = Just tInt
        funcDomain _ = Just TypeAny
        funcRange :: Maybe (Typed a) -> Maybe Type
        funcRange (Just (typeOf_->(TypeFunction _ b))) = Just b
        funcRange (Just (typeOf_->((TypeSequence b)))) = Just b
        funcRange _ = Just TypeAny
        part :: Arg -> Validator ()
        part (r,typeOf_->t) = case t of
            TypeAny -> valid
            TypePartition _ -> return $ pure ()
            _ -> invalid $ r <!> TypeError (TypePartition TypeAny) t

        histArgs :: Arg -> Validator ()
        histArgs (r,typeOf_->a) = case a of
                            TypeMSet _ -> return $ pure ()
                            TypeList _ -> return $ pure ()
                            TypeMatrix _ _ -> return $ pure ()
                            TypeAny -> return $ pure ()
                            _ -> invalid $ r <!> ComplexTypeError "Matrix, List or MSet" a
        histType ::  Maybe (Typed Expression) -> Maybe Type
        histType (Just (Typed (TypeMSet a) _ )) = Just $ TypeMatrix tInt $ TypeTuple [a,tInt]
        histType (Just (Typed (TypeMatrix _ a) _ )) = Just $ TypeMatrix tInt $ TypeTuple [a,tInt]
        histType (Just (Typed (TypeList a) _ )) = Just $ TypeMatrix tInt $ TypeTuple [a,tInt]
        histType _ = Just $ TypeMatrix tInt $ TypeTuple [TypeAny,tInt]
        enumerable :: Arg -> Validator ()
        enumerable (r,typeOf_->t) = case t of
            TypeAny -> return $ pure ()
            TypeInt TagUnnamed{} -> invalid $ r <!> CustomError "Anonymous enums are not explictly enumerable"
            TypeInt _ -> return $ pure ()
            TypeEnum{} -> return $ pure ()
            TypeBool -> return $ pure ()
            _ -> invalid $ r <!> ComplexTypeError "int enum or bool" t
        enumerableType :: Maybe (Typed Expression) -> Maybe Type
        enumerableType (Just (Typed t@(TypeInt TagInt) _)) = Just t
        enumerableType (Just (Typed t@(TypeInt (TagEnum _)) _)) = Just t
        enumerableType (Just (Typed t@(TypeEnum{}) _)) = Just t
        enumerableType _ = Nothing


flattenType :: Maybe Int -> Maybe (Typed Expression) -> Maybe Type
flattenType (Just n) (Just (Typed a _ )) | n < 0 = Just a
flattenType (Just n) (Just (Typed (TypeList m) e )) = flattenType (Just (n-1)) (Just (Typed m e))
flattenType (Just n) (Just (Typed (TypeMatrix _  m) e )) = flattenType (Just (n-1)) (Just (Typed m e))

flattenType Nothing (Just (Typed (TypeMatrix _  m) e )) = flattenType Nothing (Just (Typed m e))
flattenType Nothing (Just (Typed (TypeList  m) e )) = flattenType Nothing (Just (Typed m e))
flattenType Nothing (Just (Typed TypeAny _)) = Just $ TypeList TypeAny
flattenType _ _ = Just $ TypeList TypeAny

validateFuncOp :: Lexeme -> [RegionTagged (Typed Expression)] -> ValidatorS (Typed Expression)
validateFuncOp l args = do
    let b = funcOpBuilder l
    b args
    -- case argCheck of
    --   Nothing -> return $ Typed  (r []) $ fallback "arg fail"
    --   Just tys -> return $ Typed (r tys)(b $ map untype tys)

isOfType :: Type -> RegionTagged (Typed Expression) -> ValidatorS Bool
isOfType t (r,v) = setContext r >> return v ?=> exactly t  >> (return $ typesUnifyS [t,typeOf_ v])

isLogicalContainer :: RegionTagged (Typed Expression) -> Validator ()
isLogicalContainer (r,Typed t e) = do
    case t of
      TypeAny -> return $ pure ()
      TypeList TypeAny -> return $ pure ()
      TypeList TypeBool -> return $ pure ()
      TypeMatrix _ TypeAny -> return $ pure ()
      TypeMatrix _ TypeBool -> return $ pure ()
      TypeSet TypeAny -> return $ pure ()
      TypeMSet TypeBool -> return $ pure ()
      _ -> invalid $ r <!> ComplexTypeError "Collection of boolean" t


-- validateArgList :: [RegionTagged (Typed Expression) -> ValidatorS Bool] -> [RegionTagged (Typed Expression)] -> Validator [Typed Expression]
-- validateArgList ps args | length args < length ps = do invalid $ args <!> MissingArgsError (length ps)
-- validateArgList ps args = do
--     let ps' = ps ++ repeat argOverflow
--     xs <- zipWithM id ps' args
--     return (if and xs then  Just $ map unregion  args else Nothing)

-- argOverflow :: RegionTagged a -> ValidatorS Bool
-- argOverflow (region,_) = do
--     setContext region
--     void . contextError $ CustomError "Extra Args"
--     return False

type Arg = RegionTagged (Typed Expression)
unFunc ::  (Arg -> Validator a) --Arg validator
        -> (Maybe (Typed Expression) -> Maybe Type) --typeEvaluator
        -> ([Expression]->Expression)  --mkOp or similar
        -> [Arg] -> ValidatorS (Typed Expression)
unFunc argVal t f args = do
    (v,ts) <- case args of
        [] -> do tooFewArgs 1 0 >> return (Nothing,Nothing)
        [x] -> do
            r<- argVal x
            tc <- gets typeChecking
            let result = case r of
                    Nothing | tc -> Nothing
                    _ -> Just $ map (untype . unregion) [x]
            return (result,(Just $ unregion x))
        (x:rs) -> do
            tooManyArgs rs
            r <- argVal x
            let result =case r of
                  Nothing -> Nothing
                  Just _ -> Just $ map (untype . unregion) [x]
            return (result,(Just $ unregion x))
    let res = maybe (fallback "Arg Fail Unfunc")  f v
    return $ Typed (fromMaybe TypeAny $ t ts) res
biFunc :: (Arg -> Arg -> Validator a) -> (Maybe (Typed Expression) -> Maybe (Typed Expression) -> Maybe Type) -> ([Expression]->Expression)  -> [Arg]-> ValidatorS (Typed Expression)
biFunc argVal t f args = do
    (v,ts) <- case args of
        [] -> do tooFewArgs 2 0 >> return (Nothing,(Nothing,Nothing))
        [x] -> do tooFewArgs 2 1 >> return (Nothing,(Just $ unregion x,Nothing))
        [x,y] -> do
            r <- argVal x y
            tc <- gets typeChecking
            let result = case r of
                    Nothing | tc -> Nothing
                    _ -> Just $ map (untype . unregion) [x,y]
            return (result,(Just (unregion x) , Just (unregion y)))
        (x:y:rs) -> do
            tooManyArgs rs
            r <- argVal x y
            let result =case r of
                  Nothing -> Nothing
                  Just _ -> Just $ map (untype . unregion) [x,y]
            return (result,(Just (unregion x) , Just (unregion y)))
    let res = maybe (fallback "Arg Fail BiFunct")  f v
    return $ Typed (fromMaybe TypeAny $ uncurry t ts) res

triFunc :: (Arg  -> Arg -> Arg -> Validator a) -> (Maybe (Typed Expression) -> Maybe (Typed Expression) -> Maybe (Typed Expression) -> Maybe Type) -> ([Expression]->Expression)  -> [Arg]-> ValidatorS (Typed Expression)
triFunc argVal t f args = do
    (v,ts) <- case args of
        [] -> do tooFewArgs 3 0 >> return (Nothing,(Nothing,Nothing,Nothing))
        [x] -> do tooFewArgs 3 1 >> return (Nothing,(Just $ unregion x,Nothing,Nothing))
        [x,y] -> do tooFewArgs 3 2 >> return (Nothing,(Just $ unregion x,Just $ unregion y,Nothing))
        [x,y,z] -> do
            r <- argVal x y z
            tc <- gets typeChecking
            let result = case r of
                    Nothing | tc -> Nothing
                    _ -> Just $ map (untype . unregion) [x,y,z]
            return (result,(Just (unregion x) , Just (unregion y), Just (unregion z)))
        (x:y:z:rs) -> do
            tooManyArgs rs
            r <- argVal x y z
            let result =case r of
                    Nothing -> Nothing
                    Just _ -> Just $ map (untype . unregion) [x,y,z]
            return (result,(Just (unregion x) , Just (unregion y), Just (unregion z)))
    let res = maybe (fallback "Arg Fail Tri") f v
    return $ Typed (fromMaybe TypeAny $ uncurry3 t ts) res
    where uncurry3 f (a,b,c) = f a b c --todo export from prelude
tooFewArgs :: Int -> Int -> ValidatorS ()
tooFewArgs n i = do
    void . contextError $ MissingArgsError n i

tooManyArgs :: [RegionTagged a] -> ValidatorS ()
tooManyArgs = mapM_ (\x ->do raiseError $ x <!> UnexpectedArg)

