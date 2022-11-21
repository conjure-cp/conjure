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
      OpIndexing(OpIndexing), OpType (..), OpAttributeAsConstraint (OpAttributeAsConstraint),
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
                            else contextError (TypeError r t) >> return a

typeAs :: Type -> Maybe a -> Maybe (Typed a)
typeAs t (Just a) = Just $ Typed t a
typeAs t Nothing = Nothing

(?=>) :: ValidatorS (Typed a) -> Type -> ValidatorS a
v ?=> t = v >>= untypeAs t

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
    | SemanticError Text
    | CustomError Text
    | TypeError Type Type -- Expected, got
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
    rDeclaration :: DeclarationType
} deriving Show

mkDeclaration :: DiagnosticRegion -> Typed a -> RegionInfo
mkDeclaration r (Typed t _) = RegionInfo r t Definition

mkLiteral :: DiagnosticRegion -> Typed a -> RegionInfo
mkLiteral r (Typed t _) = RegionInfo r t LiteralDecl


data ValidatorState = ValidatorState {
    typeChecking :: Bool,
    regionInfo :: [RegionInfo],
    symbolTable :: SymbolTable,
    currentContext :: DiagnosticRegion
}
    deriving Show
instance Default ValidatorState where
    def = ValidatorState {
        typeChecking = True,
        regionInfo=[],
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
isValidLanguageName t = Data.Text.toLower t `elem` ["essence","essence'"]

validateLanguageVersion :: Maybe LangVersionNode -> Validator LanguageVersion
validateLanguageVersion Nothing = return $ pure $ LanguageVersion "Essence" [1,3]
validateLanguageVersion (Just lv@(LangVersionNode l1 n v)) = do
    setContextFrom lv
    let NameNode nt = n
    checkSymbols [l1]
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
validateStatement (UnexpectedToken lt) = return [] <* (invalid $ lt <!> CustomError "Unexpected") --TODO address as part of skip token refactor

validateHeuristicStatement :: LToken -> ExpressionNode -> ValidatorS [Statement]
validateHeuristicStatement lt exp = do
    let validHeuristics = ["static"]
    checkSymbols [lt]
    _ <- validateExpression exp
    case exp of
      IdentifierNode nn -> do
                    x <- validateName nn
                    return  $ [SearchHeuristic  x]
      _ -> return [] <* (invalid $ symbolRegion exp <!> SemanticError "Only identifiers are allowed as heuristics")


validateWhereStatement :: WhereStatementNode -> ValidatorS [Statement]
validateWhereStatement (WhereStatementNode l1 exprs) = do
    checkSymbols [l1]
    ws <-  Where <$> validateSequence_ (\x -> validateExpression x ?=> TypeBool) exprs
    return [ws]

validateObjectiveStatement :: ObjectiveStatementNode -> ValidatorS [Statement]
validateObjectiveStatement (ObjectiveMin lt en) = do
    checkSymbols [lt]
    exp <- validateExpression en
    return [Objective Minimising $ untype exp]
validateObjectiveStatement (ObjectiveMax lt en) =do
    checkSymbols [lt]
    exp <- validateExpression en
    return  [Objective Maximising $ untype exp]

validateSuchThatStatement :: SuchThatStatementNode -> ValidatorS [Statement]
validateSuchThatStatement (SuchThatStatementNode l1 l2 exprs) = do
    checkSymbols [l1, l2]
    exprs' <- validateSequence (validateExpression) exprs
    bools <- mapM (\(a,b)->do setContext a; untypeAs TypeBool b) exprs'
    let bool_exprs = bools
    return [SuchThat  bool_exprs]

validateBranchingStatement :: BranchingStatementNode -> ValidatorS [Statement]
validateBranchingStatement (BranchingStatementNode l1 l2 sts) = do
    checkSymbols [l1, l2]
    branchings <-catMaybes <$> validateList_ (f2n validateBranchingParts) sts
    return $ [SearchOrder branchings]
    where
        validateBranchingParts :: ExpressionNode -> ValidatorS SearchOrder
        validateBranchingParts (IdentifierNode nn) =  do
            n <- validateName nn
            return $ BranchingOn n
        validateBranchingParts exp = do
            x <- validateExpression exp ?=> TypeAny
            return $ Cut x

validateDeclarationStatement :: DeclarationStatementNode -> ValidatorS [Statement]
validateDeclarationStatement stmt = do
    stmt' <- case stmt of
        FindStatement l1 fs -> checkSymbols [l1] >> validateStatementSeq validateFind fs
        GivenStatement l1 gs -> checkSymbols [l1] >> validateStatementSeq validateGiven gs
        LettingStatement l1 ls -> checkSymbols [l1] >> validateStatementSeq validateLetting ls
    return  $ Declaration <$> stmt'
    where
        validateStatementSeq v l= do
            decls <- validateSequence_ v l
            return $ concat decls

validateGiven :: GivenStatementNode -> ValidatorS [Declaration]
validateGiven (GivenStatementNode idents l1 domain) =
    do
        checkSymbols [l1]
        names <-  validateNameList idents
        (dType, dom) <- typeSplit <$> validateDomain domain
        let declarations = map (flip mkDeclaration (Typed TypeAny 1) . fst) names
        mapM_ addRegion declarations
        mapM_ (\(r,x) -> putSymbol (x,(r,False,dType)) ) names
        return  $ [ FindOrGiven Given nm dom|(_,nm) <- names ]
validateGiven (GivenEnumNode se l1 l2 l3) =
    do
        checkSymbols [l1, l2, l3]
        names <-  validateNameList se
        let eType = TypeAny --TODO fixme
        mapM_ (\(r,x) -> putSymbol (x,(r,True,eType) )) names
        return  $  [GivenDomainDefnEnum n | (_,n) <- names]

validateFind :: FindStatementNode -> ValidatorS [Declaration]
validateFind (FindStatementNode names colon domain) = do
    checkSymbols [colon]
    names' <- validateNameList names
    (dType, dom) <- typeSplit <$> validateDomain domain
    mapM_ (\(r,x) -> putSymbol (x,(r,False,dType) )) names'
    return  $ [ FindOrGiven Find nm dom|(_,nm) <- names']

validateLetting :: LettingStatementNode -> ValidatorS [Declaration]
-- Letting [names] be
validateLetting (LettingStatementNode names l1 assign) = do
    checkSymbols [l1]
    names' <-  validateNameList names
    validateLettingAssignment names' assign

validateLettingAssignment :: [RegionTagged Name] -> LettingAssignmentNode -> ValidatorS [Declaration]
validateLettingAssignment names (LettingExpr en)  = do
    expr <- validateExpression en
    setContextFrom en
    let (t,e) = typeSplit expr
    let declarations = map (\(r,_)->mkDeclaration r expr) names
    mapM_ addRegion declarations
    mapM_ (\(r,x) -> putSymbol (x, (r,False,t) )) names --TODO need to add type info here
    return  $ [Letting n e | (_,n) <- names]
validateLettingAssignment names (LettingDomain lt dn) = do
    checkSymbols [lt]
    (tDomain,domain) <- typeSplit <$> validateDomain dn
    let declarations = map (\(r,_)->mkDeclaration r (Typed tDomain ())) names
    mapM_ addRegion declarations
    mapM_ (\(r,x) -> putSymbol (x, (r,False,tDomain))) names
    return $ [Letting n  (Domain domain)| (_,n) <- names]
validateLettingAssignment names (LettingEnum l1 l2 l3 enames) = do
    checkSymbols [l1, l2, l3]
    members <- validateList_ validateName enames
    mapM_
        (\(r,n) -> do
            let nameMap = zip members ([1..] :: [Int])
            let Name n' = n --TODO fix me
            let tVal = TypeInt $ TagEnum n'
            addRegion $ RegionInfo {rRegion=r, rType=tVal, rDeclaration=Ref r}
            void $ putSymbol (n,(r,True,tVal))
            mapM_ (
                \(x,i) -> putSymbol (x,(r,False,tVal))
                ) nameMap
        ) names
    return $ [LettingDomainDefnEnum n members| (_,n) <- names]
validateLettingAssignment names (LettingAnon l1 l2 l3 l4 szExp) = do
    checkSymbols [l1, l2, l3, l4]
    size <- do
                    setContextFrom szExp
                    validateExpression szExp ?=> tInt
    let d = TypeUnnamed
    --TODO delcs
    mapM_ (\(r,x) -> putSymbol (x,(r,False,d x))) names
    return  $ [LettingDomainDefnUnnamed n size| (_,n) <- names]


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
    BoolDomainNode lt -> (validateSymbol lt >> (return . Typed TypeBool) DomainBool)
    RangedIntDomainNode l1 rs -> checkSymbols [l1] >> validateRangedInt rs
    RangedEnumNode nn ranges -> validateEnumRange nn ranges
    ShortTupleDomainNode lst -> validateTupleDomain lst
    TupleDomainNode l1 doms -> checkSymbols [l1] >> validateTupleDomain doms
    RecordDomainNode l1 ndom -> checkSymbols [l1] >> validateRecordDomain ndom
    VariantDomainNode l1 ndom -> checkSymbols [l1] >> validateVariantDomain ndom
    MatrixDomainNode l1 m_ib idoms l2 dom -> checkSymbols [l1, l2] >> validateIndexedByNode m_ib >> validateMatrixDomain idoms dom
    SetDomainNode l1 attrs l2 dom -> checkSymbols [l1, l2] >> validateSetDomain attrs dom
    MSetDomainNode l1 attrs l2 dom -> checkSymbols [l1, l2] >> validateMSetDomain attrs dom
    FunctionDomainNode l1 attrs dom1 l2 dom2 -> checkSymbols [l1, l2] >> validateFunctionDomain attrs dom1 dom2
    SequenceDomainNode l1 attrs l2 dom -> checkSymbols [l1, l2] >> validateSequenceDomain attrs dom
    RelationDomainNode l1 attrs l2 doms -> checkSymbols [l1, l2] >> validateRelationDomain attrs doms
    PartitionDomainNode l1 attrs l2 dom -> checkSymbols [l1, l2] >> validatePartitionDomain attrs dom
    MissingDomainNode lt -> do raiseError $ lt <!> TokenError lt; return $ fallback "Missing Domain"
  where
    validateRangedInt :: Maybe (ListNode RangeNode) -> ValidatorS TypedDomain
    validateRangedInt (Just ranges) = do
        ranges' <-  catMaybes <$> validateList_ (f2n (validateRange tInt)) ranges
        return . Typed tInt $ DomainInt TagInt ranges'
    validateRangedInt Nothing = return . Typed tInt $ DomainInt TagInt []
    validateEnumRange :: NameNode -> Maybe (ListNode RangeNode) -> ValidatorS TypedDomain
    validateEnumRange name ranges = do
        Just name' <- validateIdentifier name
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
                lst <- catMaybes <$> validateList_  (f2n validateNamedDomainInVariant) namedDoms
                let (ts,ds) = unzip $ map (\(x,typeSplit->(t,d))->((x,t),(x,d))) lst
                return $ Typed (TypeVariant ts)  (DomainVariant ds)
    validateVariantDomain :: ListNode NamedDomainNode -> ValidatorS TypedDomain
    validateVariantDomain namedDoms = do
                lst <- catMaybes <$> validateList_ (f2n validateNamedDomainInVariant) namedDoms
                let (ts,ds) = unzip $ map (\(x,typeSplit->(t,d))->((x,t),(x,d))) lst
                return $ Typed (TypeVariant ts)  (DomainVariant ds)
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
            Nothing -> return $ def
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
validateIndexedByNode (Just (IndexedByNode a b)) = checkSymbols [a,b]

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
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
      Just  True -> invalid $ t <!> CustomError "Argument required"
      Just False ->  return . pure $ (name , Nothing)

validateAttributeNode vs (NamedAttributeNode t (Just e)) = do
    setContextFrom e
    expr <- validateExpression e ?=> tInt
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
      Just False -> invalid $ t <!> SemanticError "attribute %name% does not take an argument"
      Just True -> return . pure $(\x -> (name,Just x)) expr


validateNamedDomainInVariant :: NamedDomainNode -> ValidatorS (Name, TypedDomain)
validateNamedDomainInVariant (NameDomainNode name m_dom) = do
    name' <-  validateName name
    domain' <-case m_dom of
      Nothing ->  do return . Typed tInt $ DomainInt TagInt [RangeSingle 0]
      Just (l,d) -> do checkSymbols [l]; validateDomain d
    return $ (name' ,  domain')

validateNamedDomainInRecord :: NamedDomainNode -> ValidatorS (Name, TypedDomain)
validateNamedDomainInRecord (NameDomainNode name m_dom) = do
    name' <-  validateName name
    domain' <-case m_dom of
      Just (l,d) -> checkSymbols [l] >> validateDomain d
      Nothing -> do
        raiseError $ symbolRegion name <!> SemanticError "Dataless not allowed in record"
        (return (fallback "Dataless RecordMemeber"))
    return $  (name', domain')

validateRange ::Type -> RangeNode -> ValidatorS ((Range Expression))
validateRange t range = case range of
    SingleRangeNode en -> do setContextFrom en; ex <- validateExpression en ?=> t; return $ RangeSingle ex
    OpenRangeNode dots -> do checkSymbols [dots] ; return  RangeOpen
    RightUnboundedRangeNode e1 dots -> do checkSymbols [dots] ;setContextFrom e1; ex <- validateExpression e1 ?=> t  ; return $ RangeLowerBounded ex
    LeftUnboundedRangeNode dots e1 -> do checkSymbols [dots] ; setContextFrom e1; ex <- validateExpression e1 ?=> t  ; return $ RangeUpperBounded ex
    BoundedRangeNode e1 dots e2 -> do
        _ <- checkSymbols [dots]
        setContextFrom e1
        e1' <- validateExpression e1 ?=> t
        setContextFrom e2
        e2' <-  validateExpression e2 ?=> t
        return $  RangeBounded e1' e2'

validateArrowPair :: ArrowPairNode -> Validator (RegionTagged (Typed Expression), RegionTagged (Typed Expression))
validateArrowPair (ArrowPairNode e1 s e2) = do
    checkSymbols [s]
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
        checkSymbols [l1,l2]
        setContextFrom exp
        exp' <- validateExpression exp ?=> TypeAny
        return . Typed tInt $ mkOp TwoBarOp  [exp']
    FunctionalApplicationNode lt ln -> validateFunctionApplication  lt ln
    AttributeAsConstriant lt exprs -> validateAttributeAsConstraint lt exprs
    SpecialCase  scn ->  validateSpecialCase scn
    MissingExpressionNode lt -> do raiseError (lt <!> TokenError lt) ; return (fallback "Missing expression")


validateAttributeAsConstraint :: LToken -> ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateAttributeAsConstraint l1 exprs = do
    checkSymbols [l1]
    es <- map untype <$> validateList_ validateExpression exprs
    do
        Just lx <- validateSymbol l1
        let n = lookup (Name (lexemeText lx)) allSupportedAttributes
        r <- case (n,es) of
          (Just 1 , [e,v]) -> return . pure . Typed TypeBool  $ aacBuilder e lx (Just v)
          (Just 1 , _) -> invalid $  l1 <!> (SemanticError $ pack $ "Expected 2 args to " ++ (show lx)  ++ "got" ++ (show $ length es))
          (Just 0 , [e]) -> return .todoTypeAny . pure $ aacBuilder e lx Nothing
          (Just 0 , _) -> invalid $ l1 <!> (SemanticError $ pack $ "Expected 1 arg to " ++ (show lx)  ++ "got" ++ (show $ length es))
          (_,_) -> invalid $ l1 <!> InternalErrorS "Bad AAC"
        return $ fromMaybe (fallback "bad AAC") r
    where
        aacBuilder e lx y= Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e (fromString (lexemeFace lx)) y

validateSpecialCase :: SpecialCaseNode -> ValidatorS (Typed Expression)
validateSpecialCase (ExprWithDecls l1 ex l2 sts l3) = do
    checkSymbols [l1,l2,l3]
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
    expr <- validateExpression ex ?=> TypeAny --TODO : do this properly
    return . Typed TypeAny $ (WithLocals  (expr)  locals)

translateQnName :: Lexeme -> OpType
translateQnName qnName = case qnName of
    L_ForAll -> FunctionOp L_fAnd
    L_Exists -> FunctionOp L_fOr
    _        -> FunctionOp qnName

validateQuantificationExpression :: QuantificationExpressionNode -> ValidatorS (Typed Expression)
validateQuantificationExpression (QuantificationExpressionNode name pats over m_guard dot expr) =
    do
        checkSymbols [dot]
        scoped $ do
            name' <-  validateSymbol name
            over' <-  validateQuantificationOver pats over
            -- patterns <-  validateSequence_ validateAbstractPattern pats
            g' <- validateQuantificationGuard m_guard
            let guard' = fromMaybe [] g'
            setContextFrom expr
            body <-  validateExpression expr ?=> TypeAny
            let qBody =  Comprehension body  (over'++guard')
            let result = Typed TypeAny <$> (mkOp <$> (translateQnName <$> name') <*> pure  [qBody])
            return $ fromMaybe (fallback "Quantification error") result
    where
        validateQuantificationGuard :: Maybe QuanticationGuard -> Validator [GeneratorOrCondition]
        validateQuantificationGuard Nothing = return $ pure []
        validateQuantificationGuard (Just (QuanticationGuard l1 exp) ) = do
            checkSymbols [l1]
            setContextFrom exp
            expr' <- validateExpression exp ?=> TypeBool
            return . pure $ [Condition expr']
        validateQuantificationOver :: Sequence AbstractPatternNode -> QuantificationOverNode -> ValidatorS [GeneratorOrCondition]
        validateQuantificationOver pats ( QuantifiedSubsetOfNode lt en ) = do
            checkSymbols [lt]
            ps <- sequenceElems pats
            exp <- validateExpression en
            let (t,e) = typeSplit exp
            apats <- unifyPatterns t ps
            return [Generator $ GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet (untype exp)) | pat <- apats]
        validateQuantificationOver pats ( QuantifiedMemberOfNode lt en ) = do
            checkSymbols [lt]
            ps <- sequenceElems pats
            exp <- validateExpression en
            let (t,e) = typeSplit exp
            apats <- unifyPatterns t ps
            return [Generator $ GenInExpr pat e|pat <- apats]
        validateQuantificationOver pats ( QuantifiedDomainNode (OverDomainNode l1 dom) ) = do
            checkSymbols [l1]
            ps <- sequenceElems pats
            (dType,dom') <- typeSplit <$> validateDomain dom
            apats <- unifyPatterns dType ps
            return [ Generator $ GenDomainNoRepr pat dom'| pat <- apats]



validateMetaVar :: LToken -> ValidatorS String
validateMetaVar tok = do
    Just lx <- validateSymbol tok
    case lx of
        LMetaVar s -> return $ unpack s
        _ -> error $ "Bad MetaVar" ++ show lx

validateDomainExpression :: DomainExpressionNode -> ValidatorS (Typed Expression)
validateDomainExpression (DomainExpressionNode  l1 dom l2) = do
    checkSymbols [l1,l2]
    (tdom,dom') <- typeSplit <$> validateDomain dom
    return . Typed tdom $ Domain  dom'

--TODO fix function types inc lookups etc
validateFunctionApplication :: LToken -> ListNode ExpressionNode -> ValidatorS (Typed Expression)
validateFunctionApplication name args = do
    args' <- map (untype.snd) <$> validateList validateExpression args
    Just name' <-  validateSymbol name
    return . Typed TypeAny $ do
        let n = name'
        let a = args'
        case (n,a) of
            (L_image,[y,z]) -> Op $  MkOpImage $ OpImage y z
            _ ->  mkOp (FunctionOp n) a


validateIdentifierExpr :: NameNode -> ValidatorS (Typed Expression)
validateIdentifierExpr name = do
    Just n <- validateIdentifier name
    t <- resolveReference (symbolRegion name,Name n)
    return . Typed t $ Reference (Name n) Nothing

--TODO Adress the major hole in the type system current
validateOperatorExpression :: OperatorExpressionNode -> ValidatorS (Typed Expression)
validateOperatorExpression (PrefixOpNode lt expr) = do
    expr <-  validateExpression expr
    Just op <-  validateSymbol lt
    return . Typed TypeAny $ (\(untype->x) -> mkOp (PrefixOp op) [x]) (expr)
    --lookup symbol
validateOperatorExpression (BinaryOpNode lexp op rexp) = do
    (lType,lExpr) <- typeSplit <$> validateExpression lexp
    (rType,rExpr) <- typeSplit <$> validateExpression rexp
    Just op' <-  validateSymbol op
    let resultValidator = binOpType op'
    let resultType = resultValidator lType rType
    return . Typed resultType  $ mkBinOp ( pack $ lexemeFace op') (lExpr) (rExpr)
validateOperatorExpression (PostfixOpNode expr pon) = do
    postFixOp <-  validatePostfixOp pon
    postFixOp expr

validatePostfixOp :: PostfixOpNode -> ValidatorS (ExpressionNode -> ValidatorS (Typed Expression))
validatePostfixOp (OpFactorial lt) = do
        checkSymbols [lt]
        return  $ \exp -> do
            v <- validateExpression exp ?=> tInt
            return $ Typed tInt $ mkOp FactorialOp [v]
validatePostfixOp (ApplicationNode args) = do
        return $ \exp -> do
            (t,e) <- typeSplit <$> validateExpression exp
            args' <- validateList validateExpression args
            let underscore = Reference "_" Nothing
            let ys = [if underscore == v then Nothing else Just x| x@(_,Typed _ v) <- args']
            iType <- case t of
                TypeRelation ts -> checkProjectionArgs ts ys
                _ -> do
                        raiseTypeError $ symbolRegion exp <!> TypeError (TypeRelation []) t
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
    checkSymbols [l1,l2,l3]
    (_,dom') <- typeSplit <$> validateDomain dom
    t <- case getDType dom' of
        Just t -> return t
        Nothing -> return TypeAny <* (raiseError $ symbolRegion  dom <!> InternalErrorS (pack ("Some type bug with:" ++ show dom')))
    return $ \exp -> do
        e <- validateExpression exp ?=> t
        return . Typed t $ D.Typed e t
    where
        getDType :: Domain () Expression -> Maybe Type
        getDType d = let ?typeCheckerMode = StronglyTyped in typeOfDomain d


validateIndexingOrSlicing :: Typed Expression -> RangeNode -> ValidatorS (Typed Expression)
validateIndexingOrSlicing (Typed t exp) (SingleRangeNode r) = do
    setContextFrom r
    iType <- getIndexingType t
    i <- validateExpression r ?=> iType
    setContextFrom r
    vType <- getIndexedType t (Typed iType i)
    return . Typed vType $ Op $ MkOpIndexing (OpIndexing exp i)

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

getSlicingType :: Type -> ValidatorS Type
getSlicingType (TypeMatrix i _) = return i
getSlicingType (TypeSequence _) = return tInt
getSlicingType t = do
    contextTypeError (CustomError . pack $ "Type " ++ (show $ pretty t) ++ " does not support slicing")
    return TypeAny

getIndexingType :: Type -> ValidatorS Type
getIndexingType (TypeMatrix i _) = return i
getIndexingType (TypeSequence _) = return tInt
getIndexingType (TypeList _) = return tInt
getIndexingType (TypeTuple _) = return tInt
getIndexingType t@(TypeRecord _) = return t
getIndexingType t@(TypeVariant _) = return t
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
            (contextTypeError $ CustomError . pack $ "Tuple index "++ show v ++ "out of bounds" )
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
        checkSymbols [lt]
        validateLiteral (TupleLiteralNodeShort (ShortTuple xs))
    TupleLiteralNodeShort (ShortTuple xs) -> do
        es <- validateExprList_ xs
        makeTupleLiteral es
    RecordLiteral lt ln -> checkSymbols [lt] >> validateRecordLiteral ln
    VariantLiteral lt ln -> checkSymbols [lt] >> validateVariantLiteral ln
    SetLiteral ls -> validateSetLiteral ls
    MSetLiteral lt ls -> checkSymbols [lt] >> validateMSetLiteral ls
    FunctionLiteral lt ln -> checkSymbols [lt] >> validateFunctionLiteral ln
    SequenceLiteral lt ln -> checkSymbols [lt] >> validateSequenceLiteral ln
    RelationLiteral lt ln -> checkSymbols [lt] >> validateRelationLiteral ln
    PartitionLiteral lt ln -> checkSymbols [lt] >> validatePartitionLiteral ln

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
          RelationElemNodeLabeled (LongTuple lt xs) ->  checkSymbols [lt] >> validateExprList_ xs
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
    members <- catMaybes <$> validateList_ validateRecordMember ln
    let members' = map (\(x,y) -> (x,untype y)) members
    -- let eType = TypeRecord TypeAny
    return . Typed TypeAny $ mkAbstractLiteral $ AbsLitRecord members'

validateVariantLiteral :: ListNode RecordMemberNode -> ValidatorS (Typed Expression)
validateVariantLiteral ln = do
    members <- catMaybes <$> validateList_ validateRecordMember ln
    res <- case members of
      [] -> invalid $ symbolRegion ln <!> SemanticError "Variants must contain exactly one member"
      [(n,x)]-> return . todoTypeAny . pure $ mkAbstractLiteral $ AbsLitVariant Nothing n (untype x)
      _:_ -> invalid $ symbolRegion ln <!> SyntaxError "Variants must contain exactly one member" --tag subsequent members as unexpected 
    return $ fromMaybe (fallback "bad variant") res


validateRecordMember :: RecordMemberNode -> Validator (Name,Typed Expression)
validateRecordMember (RecordMemberNode name lEq expr) = do
    checkSymbols [lEq]
    name' <-  validateName name
    expr' <-  validateExpression expr
    return . pure $ ( name' , expr')

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
    checkSymbols [l1,l2]
    elems <-  validateSequence validateExpression se
    (t,es) <- typeSplit <$> sameType elems
    let defaultDomain :: TypedDomain = Typed tInt (mkDomainIntB 1 (fromInt $ genericLength elems))
    dom <- fromMaybe defaultDomain <$> validateOverDomain m_dom
    let lit = AbsLitMatrix (untype $ dom) es
    return $ Typed (TypeMatrix tInt t) $ mkAbstractLiteral lit
    where
        validateOverDomain :: Maybe OverDomainNode -> Validator (TypedDomain)
        validateOverDomain Nothing = return Nothing
        validateOverDomain (Just (OverDomainNode l3 dom)) = do checkSymbols [l3]; pure <$> validateDomain dom


-- Matrix as comprehension
validateMatrixLiteral (MatrixLiteralNode l1 se m_dom (Just comp) l2) = do
    checkSymbols [l1,l2]
    case m_dom of
        Nothing -> return ()
        Just p@(OverDomainNode l3 dom) -> do
            checkSymbols [l3]
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
        checkSymbols [l1]
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
    checkSymbols [l1]
    (td,domain) <- typeSplit <$> validateDomain dom
    pats <- validateSequence_ (flip unifyPattern td . Just) apn
    return  $ [Generator  (GenDomainNoRepr pat domain) | pat <- pats]

-- x <- expr
validateComprehensionBody (CompBodyGenExpr apn lt en) = do
    checkSymbols [lt]
    e <- validateExpression en
    let (t,exp) = typeSplit e
    pats <- validateSequence_ (flip unifyPattern t . Just) (apn)
    -- pats <-  validateSequence_ validateAbstractPattern apn
    return  $ [Generator (GenInExpr pat exp)| pat <- pats]
--letting x be
validateComprehensionBody (CompBodyLettingNode l1 nn l2 en) = do
    checkSymbols [l1,l2]
    (t,expr) <- typeSplit <$> validateExpression en
    pat <- unifyPattern (Just nn) t
    return  [ComprehensionLetting pat expr]


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
checkSymbols = mapM_ validateSymbol

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
    l <- validateSymbol t
    case l of
        Just (LIntLiteral x) -> return $ ConstantInt TagInt x
        _ -> error "Bad int literal"

validateBoolLiteral :: LToken -> ValidatorS Constant
validateBoolLiteral t = do
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
    unless (not tc) $ raiseError $ ValidatorDiagnostic q $ Error e

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
      Nothing -> raiseError (r <!> (CustomError . pack $ "Symbol not found "++ show n)) >> return TypeAny
      Just (reg,_,t) -> do
        addRegion (RegionInfo {rRegion=r, rType=t, rDeclaration=Ref reg})
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

unifyTypes :: Type -> RegionTagged (Typed a) -> ValidatorS a
unifyTypes _ (r,Typed TypeAny a) = do raiseError (r /!\ UnclassifiedWarning "TypeAny used") >> return a
unifyTypes t (r,Typed t' a) = do
    let ?typeCheckerMode = StronglyTyped
    if typesUnify [t', t] then pure () else raiseError $ r <!> TypeError t t'
    return a

scoped :: ValidatorS a -> ValidatorS a
scoped m = do
    st <- gets symbolTable
    res <- m
    modifySymbolTable $ const st
    return res

unifyPatterns :: Type -> [Maybe AbstractPatternNode] -> ValidatorS [AbstractPattern]
unifyPatterns t xs =  mapM (flip unifyPattern t) xs

unifyPattern :: Maybe AbstractPatternNode -> Type -> ValidatorS AbstractPattern
unifyPattern  (Just (AbstractIdentifier nn)) t = do
    (Name n) <- validateName nn
    -- traceM $ show n ++ ":" ++ show t
    --dont put symbol if _ ?
    void $ putSymbol (Name n,(symbolRegion nn,False,t))
    addRegion (RegionInfo (symbolRegion nn) t Definition)
    return  $ Single $  Name n

unifyPattern (Just(AbstractMetaVar lt)) _ = do
    s <- validateMetaVar lt
    return $ AbstractPatternMetaVar s

unifyPattern (Just(AbstractPatternTuple m_lt ln)) t = do
    sps <-listToSeq ln
    ps <-sequenceElems sps
    case m_lt of
        Nothing -> void $ return ()
        Just lt -> checkSymbols [lt]
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