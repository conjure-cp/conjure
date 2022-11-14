{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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


data Typed a = Typed Type a


untype :: Typed a -> a
untype (Typed _ a) = a

untypeAs :: Type -> Maybe (Typed a) -> Validator a
untypeAs _ Nothing = return Nothing
untypeAs r (Just (Typed t a)) = if let ?typeCheckerMode=RelaxedIntegerTags in typeUnify r t
                            then return $ Just a
                            else contextError $ CustomError "Type error"

typeAs :: Type -> Maybe a -> Maybe (Typed a)
typeAs t (Just a) = Just $ Typed t a
typeAs t Nothing = Nothing

(?=>) :: Validator (Typed a) -> Type -> Validator a
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


data ValidatorState = ValidatorState {
    typeChecking :: Bool,
    symbolTable :: SymbolTable,
    currentContext :: DiagnosticRegion
}
    deriving Show
instance Default ValidatorState where
    def = ValidatorState {
        typeChecking = False,
        symbolTable=M.empty,
        currentContext=GlobalRegion
        }
type SymbolTable = (Map Text SymbolTableValue)
type SymbolTableValue = ReferenceTo
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

setContextFrom :: Flattenable ETok a => a -> ValidatorS ()
setContextFrom = setContext.getRegion

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
    unless (maybe False isValidLanguageName name) (raiseError $ n <!> SyntaxError "Not a valid language name")
    nums <- validateSequence_ getNum v
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


validateStatement :: StatementNode -> Validator [Statement]
validateStatement (DeclarationStatement dsn) = validateDeclarationStatement dsn
validateStatement (BranchingStatement bsn) = validateBranchingStatement bsn
validateStatement (SuchThatStatement stsn) = validateSuchThatStatement stsn
validateStatement (WhereStatement wsn) = validateWhereStatement wsn
validateStatement (ObjectiveStatement osn) = validateObjectiveStatement osn
validateStatement (HeuristicStatement lt exp) = validateHeuristicStatement lt exp
validateStatement (UnexpectedToken lt) = invalid $ lt <!> CustomError "Unexpected" --TODO address as part of skip token refactor

validateHeuristicStatement :: LToken -> ExpressionNode -> Validator [Statement]
validateHeuristicStatement lt exp = do
    checkSymbols [lt]
    _ <- validateExpression exp
    case exp of
      IdentifierNode nn -> do
                    x <- validateName nn
                    return  $ sequence [SearchHeuristic <$> x]
      _ -> invalid $ exp <!> SemanticError "Only identifiers are allowed as heuristics"

validateWhereStatement :: WhereStatementNode -> Validator [Statement]
validateWhereStatement (WhereStatementNode l1 exprs) = do
    checkSymbols [l1]
    ws <-  Where <$> validateSequence_ (\x -> do
        e <- validateExpression x
        untypeAs TypeBool e) exprs
    return . pure $ [ws]

validateObjectiveStatement :: ObjectiveStatementNode -> Validator [Statement]
validateObjectiveStatement (ObjectiveMin lt en) = do
    checkSymbols [lt]
    Just exp <- validateExpression en
    return . pure $ [Objective Minimising $ untype exp]
validateObjectiveStatement (ObjectiveMax lt en) =do
    checkSymbols [lt]
    Just exp <- validateExpression en
    return . pure $ [Objective Maximising $ untype exp]

validateSuchThatStatement :: SuchThatStatementNode -> Validator [Statement]
validateSuchThatStatement (SuchThatStatementNode l1 l2 exprs) = do
    checkSymbols [l1, l2]
    exprs' <- validateSequence_ validateExpression exprs
    bools <- mapM (untypeAs TypeBool . pure) exprs'
    let bool_exprs = catMaybes bools
    return . pure $ [SuchThat  bool_exprs]

validateBranchingStatement :: BranchingStatementNode -> Validator [Statement]
validateBranchingStatement (BranchingStatementNode l1 l2 sts) = do
    checkSymbols [l1, l2]
    branchings <- validateList_ validateBranchingParts sts
    return . pure $ [SearchOrder branchings]
    where
        validateBranchingParts :: ExpressionNode -> Validator SearchOrder
        validateBranchingParts (IdentifierNode nn) =  do
            n <- validateName nn
            return $ BranchingOn <$> n
        validateBranchingParts exp = do
            x <- validateExpression exp
            return $ Cut . untype <$> x

validateDeclarationStatement :: DeclarationStatementNode -> Validator [Statement]
validateDeclarationStatement stmt = do
    Just stmt' <- case stmt of
        FindStatement l1 fs -> checkSymbols [l1] >> validateStatementSeq validateFind fs
        GivenStatement l1 gs -> checkSymbols [l1] >> validateStatementSeq validateGiven gs
        LettingStatement l1 ls -> checkSymbols [l1] >> validateStatementSeq validateLetting ls
    return . pure $ Declaration <$> stmt'
    where
        validateStatementSeq v l= do
            decls <- validateSequence_ v l
            return $ pure $ concat decls

validateGiven :: GivenStatementNode -> Validator [Declaration]
validateGiven (GivenStatementNode idents l1 domain) =
    do
        checkSymbols [l1]
        names <-  validateNameList idents
        Just dom <-  validateDomain domain
        mapM_ (\x -> putSymbol (x,DeclNoRepr Given x dom NoRegion) ) names
        return . pure $ [ FindOrGiven Given nm dom|nm <- names ]
validateGiven (GivenEnumNode se l1 l2 l3) =
    do
        checkSymbols [l1, l2, l3]
        names <-  validateNameList se
        mapM_ (\x -> putSymbol (x,Alias (Domain (DomainEnum x Nothing Nothing))) ) names
        return . pure $  [GivenDomainDefnEnum n | n <- names]

validateFind :: FindStatementNode -> Validator [Declaration]
validateFind (FindStatementNode names colon domain) = do
    checkSymbols [colon]
    names' <- validateNameList names
    Just dom <- validateDomain domain
    mapM_ (\x -> putSymbol (x,DeclNoRepr Find x (dom) NoRegion) ) names'
    return . pure $ [ FindOrGiven Given nm dom|nm <- names']

validateLetting :: LettingStatementNode -> Validator [Declaration]
-- Letting [names] be
validateLetting (LettingStatementNode names l1 assign) = do
    checkSymbols [l1]
    names' <-  validateNameList names
    validateLettingAssignment names' assign

validateLettingAssignment :: [Name] -> LettingAssignmentNode -> Validator [Declaration]
validateLettingAssignment names (LettingExpr en)  = do
    Just expr <- validateExpression en
    setContextFrom en
    let expr' = untype expr
    mapM_ (\x -> putSymbol (x, Alias $ expr' )) names --TODO need to add type info here
    return . pure $ [Letting n expr' | n <- names]
validateLettingAssignment names (LettingDomain lt dn) = do
    checkSymbols [lt]
    Just domain <- validateDomain dn
    mapM_ (\x -> putSymbol (x, Alias (Domain domain))) names
    return . pure $ [Letting n  (Domain domain)| n <- names]
validateLettingAssignment names (LettingEnum l1 l2 l3 enames) = do
    checkSymbols [l1, l2, l3]
    members <- validateList_ validateName enames
    mapM_
        (\n -> do
            let nameMap = zip members [1..]
            let Name n' = n --TODO fix me
            void $ putSymbol (n,Alias (Domain (DomainEnum n Nothing (Just nameMap))))
            mapM_ (\(x,i) -> putSymbol (x,Alias (Constant (ConstantInt (TagEnum n') i))) ) $ nameMap
        ) names
    return . pure $ [LettingDomainDefnEnum n members| n <- names]
validateLettingAssignment names (LettingAnon l1 l2 l3 l4 szExp) = do
    checkSymbols [l1, l2, l3, l4]
    Just size <- do
                    s <- validateExpression szExp
                    untypeAs tInt s
    mapM_ (\x -> putSymbol (x,Alias (Domain (DomainUnnamed x size))) ) names
    return . pure $ [LettingDomainDefnUnnamed n size| n <- names]


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




type DomainValidator = Validator (Domain () Expression)

validateDomainWithRepr :: DomainNode -> Validator (Domain HasRepresentation Expression)
validateDomainWithRepr dom = do
    dom' <- validateDomain dom
    return $ changeRepr NoRepresentation <$> dom'

validateDomain :: DomainNode -> DomainValidator
validateDomain dm = case dm of
    MetaVarDomain lt ->  do mv <- validateMetaVar lt ; return $ DomainMetaVar <$> mv
    BoolDomainNode lt -> pure <$> (validateSymbol lt >> return DomainBool)
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
    MissingDomainNode lt -> invalid $ lt <!> TokenError lt
  where
    validateRangedInt :: Maybe (ListNode RangeNode) -> DomainValidator
    validateRangedInt (Just ranges) = do
        ranges' <- validateList_ validateRange ranges 
        return . pure $ DomainInt TagInt ranges'
    validateRangedInt Nothing = return . pure $ DomainInt TagInt []
    validateEnumRange :: NameNode -> Maybe (ListNode RangeNode) -> DomainValidator
    validateEnumRange name ranges = do
        ranges' <- case ranges of
            Just r -> pure <$> validateList_ validateRange r
            Nothing -> pure Nothing
        Just name' <- validateIdentifier name
        a <- getSymbol name'
        case a of
            Just (Alias (Domain (DomainEnum {}))) ->return . pure $ DomainEnum (Name name') ranges' Nothing
            Just t -> invalid $ name <!> InternalError -- $ "Unknown type :" ++ t
            Nothing -> case ranges' of
              Nothing -> return . pure $  DomainReference (Name name') Nothing
              Just _ -> do
                raiseError ( name <!> SemanticError "range not supported on non enum ranges")
                return . pure $  DomainReference (Name name') Nothing

    validateTupleDomain :: ListNode DomainNode -> DomainValidator
    validateTupleDomain doms = pure . DomainTuple <$> validateList_ validateDomain doms
    validateRecordDomain :: ListNode NamedDomainNode -> DomainValidator
    validateRecordDomain namedDoms = pure . DomainRecord <$> validateList_ validateNamedDomainInRecord namedDoms
    validateVariantDomain :: ListNode NamedDomainNode -> DomainValidator
    validateVariantDomain namedDoms = do
                lst <- validateList_ validateNamedDomainInVariant namedDoms
                return . pure $ DomainVariant lst
    validateMatrixDomain :: ListNode DomainNode -> DomainNode -> DomainValidator
    validateMatrixDomain indexes dom = do
        idoms <-  validateList_ validateDomain indexes
        dom' <-  validateDomain dom
        return $ foldr DomainMatrix <$> dom' <*> pure idoms
    validateSetDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> DomainValidator
    validateSetDomain attrs dom = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a ->  validateSetAttributes a
            Nothing -> return $ Just def
        dom' <-  validateDomain dom
        return $ DomainSet <$> repr <*> attrs' <*> dom'

    validateMSetDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> DomainValidator
    validateMSetDomain attrs dom = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a ->  validateMSetAttributes a
            Nothing -> return $ Just def
        dom' <-  validateDomain dom
        return $ DomainMSet <$> repr <*> attrs' <*> dom'
    validateFunctionDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> DomainNode -> DomainValidator
    validateFunctionDomain attrs dom1 dom2 = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a ->  validateFuncAttributes a
            Nothing -> return $ Just def
        dom1' <-  validateDomain dom1
        dom2' <-  validateDomain dom2
        return $ DomainFunction <$> repr <*> attrs' <*> dom1' <*> dom2'

    -- attrs <- validateAttributes
    validateSequenceDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> DomainValidator
    validateSequenceDomain attrs dom = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a ->  validateSeqAttributes a
            Nothing -> return $ Just def
        dom' <-  validateDomain dom
        return $ DomainSequence <$> repr <*> attrs' <*> dom'
    validateRelationDomain ::Maybe (ListNode AttributeNode)-> ListNode DomainNode -> DomainValidator
    validateRelationDomain attrs doms = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a ->  validateRelationAttributes a
            Nothing -> return $ Just def
        doms' <-  validateList_ validateDomain doms
        return $ DomainRelation <$> repr <*> attrs' <*> pure doms'
    validatePartitionDomain :: Maybe (ListNode AttributeNode)-> DomainNode -> DomainValidator
    validatePartitionDomain attrs dom = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a ->  validatePartitionAttributes a
            Nothing -> return $ Just def
        dom' <-  validateDomain dom
        return $ DomainPartition <$> repr <*> attrs' <*> dom'

validateIndexedByNode :: Maybe IndexedByNode -> ValidatorS ()
validateIndexedByNode Nothing = return ()
validateIndexedByNode (Just (IndexedByNode a b)) = checkSymbols [a,b]

todo :: Text -> Validator a
todo s = invalid $ ValidatorDiagnostic GlobalRegion $ Error $ InternalErrorS (append "Not Implemented: " s)

validateSizeAttributes :: [(Lexeme,Maybe Expression)] -> Validator (SizeAttr Expression)
validateSizeAttributes attrs = do
    let sizeAttrs = [L_size,L_minSize,L_maxSize]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $ Just SizeAttr_None
      [(L_size,Just a)] -> return $ Just (SizeAttr_Size a)
      [(L_minSize, Just a)] -> return $ Just (SizeAttr_MinSize a)
      [(L_maxSize, Just a)] -> return $ Just (SizeAttr_MaxSize a)
      [(L_minSize, Just a),(L_maxSize, Just b)] -> return $ Just (SizeAttr_MinMaxSize a b)
      as -> contextError $ SemanticError $ pack $ "Incompatible attributes size:" ++ show as

validatePartSizeAttributes :: [(Lexeme,Maybe Expression)] -> Validator (SizeAttr Expression)
validatePartSizeAttributes attrs = do
    let sizeAttrs = [L_partSize,L_minPartSize,L_maxPartSize]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $ Just SizeAttr_None
      [(L_partSize,Just a)] -> return $ Just (SizeAttr_Size a)
      [(L_minPartSize, Just a)] -> return $ Just (SizeAttr_MinSize a)
      [(L_maxPartSize, Just a)] -> return $ Just (SizeAttr_MaxSize a)
      [(L_minPartSize, Just a),(L_maxPartSize, Just b)] -> return $ Just (SizeAttr_MinMaxSize a b)
      as -> contextError $ SemanticError $ pack $ "Incompatible attributes partitionSize :" ++ show as

validateNumPartAttributes :: [(Lexeme,Maybe Expression)] -> Validator (SizeAttr Expression)
validateNumPartAttributes attrs = do
    let sizeAttrs = [L_numParts,L_maxNumParts,L_minNumParts]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $ Just SizeAttr_None
      [(L_numParts,Just a)] -> return $ Just (SizeAttr_Size a)
      [(L_minNumParts, Just a)] -> return $ Just (SizeAttr_MinSize a)
      [(L_maxNumParts, Just a)] -> return $ Just (SizeAttr_MaxSize a)
      [(L_minNumParts, Just a),(L_maxNumParts, Just b)] -> return $ Just (SizeAttr_MinMaxSize a b)
      as -> contextError $ SemanticError $ pack $ "Incompatible attributes partitionSize :" ++ show as


validateJectivityAttributes :: [(Lexeme,Maybe Expression)] -> Validator JectivityAttr
validateJectivityAttributes attrs = do
    let sizeAttrs = [L_injective,L_surjective,L_bijective]
    let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
    case filtered of
      [] -> return $ Just JectivityAttr_None
      [(L_injective,_)] -> return $ Just JectivityAttr_Injective
      [(L_surjective, _)] -> return $ Just JectivityAttr_Surjective
      [(L_bijective, _)] -> return $ Just JectivityAttr_Bijective
      [(L_injective, _),(L_surjective, _)] -> do
        contextInfo $ UnclassifiedInfo "Inj and Sur can be combined to bijective"
        return $ Just JectivityAttr_Bijective
      as -> contextError $ SemanticError $ pack $ "Incompatible attributes jectivity" ++ show as


validateSetAttributes :: ListNode AttributeNode -> Validator (SetAttr Expression)
validateSetAttributes atts = do
    setContextFrom atts
    attrs <- validateList_ (validateAttributeNode setValidAttrs) atts
    size <- validateSizeAttributes attrs
    return $ SetAttr <$> size


validateMSetAttributes :: ListNode AttributeNode -> Validator (MSetAttr Expression)
validateMSetAttributes atts = do
    setContextFrom atts
    attrs <- validateList_ (validateAttributeNode msetValidAttrs) atts
    size <- validateSizeAttributes attrs
    occurs <- validateOccursAttrs attrs
    return $ MSetAttr <$> size <*> occurs
        where
            validateOccursAttrs attrs = do
                let sizeAttrs = [L_minOccur,L_maxOccur]
                let filtered = sort $ filter (\x -> fst x `elem` sizeAttrs) attrs
                case filtered of
                    [] -> return $ Just OccurAttr_None
                    [(L_minOccur,Just a)] -> return $ Just (OccurAttr_MinOccur a)
                    [(L_maxOccur, Just a)] -> return $ Just (OccurAttr_MaxOccur a)
                    [(L_minOccur, Just a),(L_maxOccur, Just b)] -> return $ Just (OccurAttr_MinMaxOccur a b)
                    as -> contextError $ SemanticError $ pack $ "Bad args to occurs" ++ show as


validateFuncAttributes :: ListNode AttributeNode -> Validator (FunctionAttr Expression)
validateFuncAttributes atts = do
    attrs <- validateList_ (validateAttributeNode funAttrs) atts
    size <- validateSizeAttributes attrs
    parts <- return . Just $ if L_total `elem` map fst attrs then PartialityAttr_Total else PartialityAttr_Partial
    jectivity <- validateJectivityAttributes attrs
    return $ FunctionAttr <$> size <*> parts <*> jectivity

validateSeqAttributes :: ListNode AttributeNode -> Validator (SequenceAttr Expression)
validateSeqAttributes atts = do
    attrs <- validateList_ (validateAttributeNode seqAttrs) atts
    size <- validateSizeAttributes attrs
    jectivity <- validateJectivityAttributes attrs
    return $ SequenceAttr <$> size <*> jectivity


validateRelationAttributes :: ListNode AttributeNode -> Validator (RelationAttr Expression)
validateRelationAttributes atts = do
    setContextFrom atts
    attrs <- validateList_ (validateAttributeNode relAttrs) atts
    size <- validateSizeAttributes attrs
    others <- validateArray validateBinaryRel (filter (\x -> fst x `elem` map fst binRelAttrs) attrs)
    return $ RelationAttr <$>  size <*> pure (BinaryRelationAttrs $ S.fromList others )
        where
            validateBinaryRel :: (Lexeme , Maybe Expression) -> Validator BinaryRelationAttr
            validateBinaryRel (l,_) = do
                case lexemeToBinRel l of
                    Just b -> return . pure $ b
                    Nothing ->contextError $ InternalErrorS $ pack $ "Not found (bin rel) " ++ show l

validatePartitionAttributes :: ListNode AttributeNode -> Validator (PartitionAttr Expression)
validatePartitionAttributes atts = do
    attrs <- validateList_ (validateAttributeNode partitionAttrs) atts
    --guard size attrs and complete as this is default
    size <- validateNumPartAttributes attrs
    partSize <- validatePartSizeAttributes attrs
    regular <- return . Just $ L_regular `elem` map fst attrs
    return $ PartitionAttr <$> size <*> partSize <*> regular

validateAttributeNode :: Map Lexeme Bool -> AttributeNode -> Validator (Lexeme,Maybe Expression)
validateAttributeNode vs (NamedAttributeNode t Nothing) = do
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
      Just  True -> invalid $ t <!> CustomError "Argument required"
      Just False ->  return . pure $ (name , Nothing)

validateAttributeNode vs (NamedAttributeNode t (Just e)) = do
    expr <- validateExpression e ?=> tInt
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ t <!> CustomError "Not a valid attribute in this context"
      Just False -> invalid $ t <!> SemanticError "attribute %name% does not take an argument"
      Just True -> return $(\x -> (name,Just x)) <$> expr


validateNamedDomainInVariant :: NamedDomainNode -> Validator (Name, Domain () Expression)
validateNamedDomainInVariant (NameDomainNode name m_dom) = do
    name' <-  validateName name
    domain' <-case m_dom of
      Nothing -> return . pure $ DomainInt TagInt [RangeSingle 0]
      Just (l,d) -> checkSymbols [l] >> validateDomain d
    return $  (,) <$> name' <*> domain'

validateNamedDomainInRecord :: NamedDomainNode -> Validator (Name, Domain () Expression)
validateNamedDomainInRecord (NameDomainNode name m_dom) = do
    name' <-  validateName name
    domain' <-case m_dom of
      Nothing -> invalid $ name <!> SemanticError "Dataless not allowed in record"
      Just (l,d) -> checkSymbols [l] >> validateDomain d
    return $  (,) <$> name' <*> domain'

validateRange :: RangeNode -> Validator (Range Expression)
validateRange range = case range of
    SingleRangeNode en -> do ex <- untypeAs tInt =<< validateExpression en ; return  $ RangeSingle <$> ex
    OpenRangeNode dots -> do checkSymbols [dots] ; return . pure $ RangeOpen
    RightUnboundedRangeNode e1 dots -> do checkSymbols [dots] ; ex <- untypeAs tInt =<<  validateExpression e1 ; return $ RangeLowerBounded <$>ex
    LeftUnboundedRangeNode dots e1 -> do checkSymbols [dots] ;  ex <- untypeAs tInt =<<  validateExpression e1 ; return $ RangeUpperBounded <$> ex
    BoundedRangeNode e1 dots e2 -> do
        _ <- checkSymbols [dots]
        e1' <- untypeAs tInt =<< validateExpression e1
        e2' <-  untypeAs tInt =<< validateExpression e2
        return $  RangeBounded <$> e1' <*> e2'

validateArrowPair :: ArrowPairNode -> Validator (Typed Expression, Typed Expression)
validateArrowPair (ArrowPairNode e1 s e2) = do
    checkSymbols [s]
    e1' <-  validateExpression e1
    e2' <-  validateExpression e2
    return $ (,) <$> e1' <*> e2'

validateExpression :: ExpressionNode -> Validator (Typed Expression)
validateExpression expr = case expr of
    Literal ln -> validateLiteral ln
    IdentifierNode nn -> validateIdentifierExpr nn
    MetaVarExpr tok -> do 
        Just x <- validateMetaVar tok ;
        return $ typeAs TypeAny $ pure $ ExpressionMetaVar x
    QuantificationExpr qen -> validateQuantificationExpression qen
    OperatorExpressionNode oen -> validateOperatorExpression oen
    DomainExpression dex -> castAny $ validateDomainExpression dex 
    ParenExpression (ParenExpressionNode l1 exp l2) -> checkSymbols [l1,l2] >> validateExpression exp
    AbsExpression (ParenExpressionNode l1 exp l2) -> do
        checkSymbols [l1,l2]
        Just exp' <- validateExpression exp ?=> TypeAny
        return . typeAs tInt . pure $ mkOp TwoBarOp  [exp']
    FunctionalApplicationNode lt ln -> validateFunctionApplication  lt ln
    AttributeAsConstriant lt exprs -> validateAttributeAsConstraint lt exprs
    SpecialCase  scn ->  castAny $ validateSpecialCase scn
    MissingExpressionNode lt -> invalid $  lt <!> TokenError lt

validateAttributeAsConstraint :: LToken -> ListNode ExpressionNode -> Validator (Typed Expression)
validateAttributeAsConstraint l1 exprs = do
    checkSymbols [l1]
    es <- map untype <$> validateList_ validateExpression exprs
    do
        Just lx <- validateSymbol l1
        let n = lookup (Name (lexemeText lx)) allSupportedAttributes
        case (n,es) of
          (Just 1 , [e,v]) -> return . todoTypeAny . pure  $ aacBuilder e lx (Just v)
          (Just 1 , _) -> invalid $  l1 <!> (SemanticError $ pack $ "Expected 2 args to " ++ (show lx)  ++ "got" ++ (show $ length es))
          (Just 0 , [e]) -> return .todoTypeAny . pure $ aacBuilder e lx Nothing
          (Just 0 , _) -> invalid $ l1 <!> (SemanticError $ pack $ "Expected 1 arg to " ++ (show lx)  ++ "got" ++ (show $ length es))
          (_,_) -> invalid $ l1 <!> InternalErrorS "Bad AAC"
    where
        aacBuilder e lx y= Op $ MkOpAttributeAsConstraint $ OpAttributeAsConstraint e (fromString (lexemeFace lx)) y

validateSpecialCase :: SpecialCaseNode -> Validator Expression
validateSpecialCase (ExprWithDecls l1 ex l2 sts l3) = do
    checkSymbols [l1,l2,l3]
    expr <- validateExpression ex
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
    return (WithLocals <$> (untype <$> expr) <*> pure locals)

translateQnName :: Lexeme -> OpType
translateQnName qnName = case qnName of
    L_ForAll -> FunctionOp L_fAnd
    L_Exists -> FunctionOp L_fOr
    _        -> FunctionOp qnName

validateQuantificationExpression :: QuantificationExpressionNode -> Validator (Typed Expression)
validateQuantificationExpression (QuantificationExpressionNode name pats over m_guard dot expr) =
    do
        checkSymbols [dot]
        name' <-  validateSymbol name
        patterns <-  validateSequence_ validateAbstractPattern pats
        over' <-  validateQuantificationOver over
        guard' <-  validateQuantificationGuard m_guard
        body <-  validateExpression expr ?=> TypeAny
        let gens = map  <$>  over' <*> pure patterns
        let qBody =  Comprehension <$> body  <*> ((++) <$>  gens <*> guard')
        return . todoTypeAny  $ mkOp <$> (translateQnName <$> name') <*> ((:[]) <$> qBody)
    where
        validateQuantificationGuard :: Maybe QuanticationGuard -> Validator [GeneratorOrCondition]
        validateQuantificationGuard Nothing = return $ pure []
        validateQuantificationGuard (Just (QuanticationGuard l1 exp) ) = do
            checkSymbols [l1]
            setContextFrom exp
            Just expr' <- validateExpression exp ?=> TypeBool
            return . pure $ [Condition expr']
        validateQuantificationOver :: QuantificationOverNode -> Validator (AbstractPattern -> GeneratorOrCondition)
        validateQuantificationOver ( QuantifiedSubsetOfNode lt en ) = do
            checkSymbols [lt]
            Just exp <- validateExpression en
            return . pure $ (\pat -> Generator $ GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet (untype exp)))
        validateQuantificationOver ( QuantifiedMemberOfNode lt en ) = do
            checkSymbols [lt]
            Just exp <- validateExpression en ?=> TypeAny
            q <- getType exp
            return . pure $ (\pat -> Generator $ GenInExpr pat exp)
        validateQuantificationOver ( QuantifiedDomainNode (OverDomainNode l1 dom) ) = do
            checkSymbols [l1]
            Just dom' <- validateDomain dom
            return . pure $ (\pat -> Generator $ GenDomainNoRepr pat dom')



validateAbstractPattern :: AbstractPatternNode -> Validator AbstractPattern
validateAbstractPattern (AbstractIdentifier nn) = validateName nn >>= \x -> return $ Single <$> x
validateAbstractPattern (AbstractMetaVar lt) =  validateMetaVar lt >>= \x -> return $ AbstractPatternMetaVar <$> x
validateAbstractPattern (AbstractPatternMatrix ln) = pure . AbsPatMatrix <$> validateList_ validateAbstractPattern ln
validateAbstractPattern (AbstractPatternSet ln) = pure . AbsPatSet <$> validateList_ validateAbstractPattern ln
validateAbstractPattern (AbstractPatternTuple m_lt ln) = do
    maybe (pure ()) (\n ->checkSymbols [n]) m_lt
    pure . AbsPatTuple <$> validateList_ validateAbstractPattern ln

validateMetaVar :: LToken -> Validator String
validateMetaVar tok = do
    Just lx <- validateSymbol tok
    case lx of
        LMetaVar s -> return .pure  $ unpack s
        _ -> invalid $ tok <!> InternalError

validateDomainExpression :: DomainExpressionNode -> Validator Expression
validateDomainExpression (DomainExpressionNode  l1 dom l2) = do
    checkSymbols [l1,l2]
    dom' <- validateDomain dom
    return $ Domain <$> dom'

validateFunctionApplication :: LToken -> ListNode ExpressionNode -> Validator (Typed Expression)
validateFunctionApplication name args = do
    name' <-  validateSymbol name
    args' <- map (untype.snd) <$> validateList validateExpression args
    return . todoTypeAny $ do
        n <- name'
        let a = args'
        case (n,a) of
            (L_image,[y,z]) -> return $ Op $  MkOpImage $ OpImage y z
            _ -> return $ mkOp (FunctionOp n) a


validateIdentifierExpr :: NameNode -> Validator (Typed Expression)
validateIdentifierExpr name = do
    Just n <- validateIdentifier name
    ref <- setContextFrom name >> (tryResolveReference $ Name n)
    return . todoTypeAny . pure $ Reference (Name n) ref

validateOperatorExpression :: OperatorExpressionNode -> Validator (Typed Expression)
validateOperatorExpression (PrefixOpNode lt expr) = do
    expr <-  validateExpression expr
    Just op <-  validateSymbol lt
    return . todoTypeAny $ (\(untype->x) -> mkOp (PrefixOp op) [x]) <$> (expr)
    --lookup symbol
validateOperatorExpression (BinaryOpNode lexp op rexp) = do
    lExpr <-  validateExpression lexp
    rExpr <-  validateExpression rexp
    op' <-  validateSymbol op
    return . todoTypeAny  $ mkBinOp  <$> ( pack . lexemeFace <$> op') <*> (untype <$> lExpr) <*> (untype <$> rExpr)
validateOperatorExpression (PostfixOpNode expr pon) = do
    expr' <-  validateExpression expr
    postFixOp <-  validatePostfixOp pon
    return $ postFixOp <*> expr'

validatePostfixOp :: PostfixOpNode -> Validator (Typed Expression -> Typed Expression)
validatePostfixOp (OpFactorial lt) = do
        checkSymbols [lt]
        return . pure $ (\(untype->x) -> Typed tInt $ mkOp FactorialOp [x])
validatePostfixOp (ApplicationNode args) = do
        args' <- validateList_ validateExpression args
        let underscore = Reference "_" Nothing
        let ys = [if underscore == v then Nothing else Just v | x@(Typed _ v) <- args']
        return . pure $ \ (Typed t x) -> Typed TypeAny $ Op $ MkOpRelationProj $ OpRelationProj x ys
validatePostfixOp (IndexedNode ln) = do
        ranges <-validateList_ validateRange ln
        let indices = map interpretRange ranges
        return . pure  $ \x -> (foldl (\m f -> f m)) x indices
        where
            interpretRange :: Range Expression -> (Typed Expression-> Typed Expression)
            interpretRange x =
                let a = case x of
                            RangeOpen -> Right (Nothing,Nothing)
                            RangeSingle ex -> Left ex
                            RangeLowerBounded ex -> Right (Just ex,Nothing)
                            RangeUpperBounded ex -> Right (Nothing,Just ex)
                            RangeBounded exl exr -> Right (Just exl,Just exr)
                in case a of
                  Left ex -> \(untype->m) -> Typed TypeAny $ Op $ MkOpIndexing (OpIndexing m ex)
                  Right (i,j) -> \(untype->m) -> Typed TypeAny $ Op $ MkOpSlicing (OpSlicing m i j)
validatePostfixOp (ExplicitDomain l1 l2 dom l3) = do
    checkSymbols [l1,l2,l3]
    Just dom' <- validateDomain dom
    let t =  getType dom'
    case t of
      Nothing -> invalid $ dom <!> InternalErrorS (pack ("Some type bug with:" ++ show dom'))
      Just ty -> return . pure $ (\(Typed t ex) -> Typed ty $ D.Typed ex ty)
    where
        getType :: Domain () Expression -> Maybe Type
        getType d = let ?typeCheckerMode = StronglyTyped in typeOfDomain d



validateLiteral :: LiteralNode -> Validator (Typed Expression)
validateLiteral litNode = case litNode of
    IntLiteral lt -> validateIntLiteral lt >>= \x -> return $ typeAs tInt $ Constant <$> x
    BoolLiteral lt -> validateBoolLiteral lt >>= \x -> return $ typeAs TypeBool $ Constant <$> x
    MatrixLiteral mln -> validateMatrixLiteral mln
    TupleLiteralNode (LongTuple lt xs) ->  do
        checkSymbols [lt]
        validateLiteral (TupleLiteralNodeShort (ShortTuple xs))
    TupleLiteralNodeShort (ShortTuple xs) -> do
        es <- validateExprList_ xs
        t <- makeTupleLiteral es
        return $ pure t
    RecordLiteral lt ln -> checkSymbols [lt] >> validateRecordLiteral ln
    VariantLiteral lt ln -> checkSymbols [lt] >> validateVariantLiteral ln
    SetLiteral ls -> validateSetLiteral ls
    MSetLiteral lt ls -> checkSymbols [lt] >> validateMSetLiteral ls
    FunctionLiteral lt ln -> checkSymbols [lt] >> validateFunctionLiteral ln
    SequenceLiteral lt ln -> checkSymbols [lt] >> validateSequenceLiteral ln
    RelationLiteral lt ln -> checkSymbols [lt] >> validateRelationLiteral ln
    PartitionLiteral lt ln -> checkSymbols [lt] >> validatePartitionLiteral ln

validateSequenceLiteral :: ListNode ExpressionNode -> Validator (Typed Expression)
validateSequenceLiteral x = do
    l <-  map untype <$> validateExprList_ x
    let lType = TypeSequence TypeAny
    return . (typeAs lType). pure $  mkAbstractLiteral  $ AbsLitSequence l


validateRelationLiteral :: ListNode RelationElemNode -> Validator (Typed Expression)
validateRelationLiteral ln = do
    members <- map (map untype) <$> validateList_ validateRelationMember ln
    return . todoTypeAny . pure $ mkAbstractLiteral $ AbsLitRelation members
    where
        validateRelationMember :: RelationElemNode -> Validator [Typed Expression]
        validateRelationMember x = case x of
          RelationElemNodeLabeled (LongTuple lt xs) -> Just <$> (checkSymbols [lt] >> validateExprList_ xs)
          RelationElemNodeShort (ShortTuple xs) -> Just <$> validateExprList_ xs


validatePartitionLiteral :: ListNode PartitionElemNode -> Validator (Typed Expression)
validatePartitionLiteral ln = do
    members <- map (map untype) <$> validateList_ (\(PartitionElemNode exprs) -> Just <$> validateExprList_ exprs) ln
    return . todoTypeAny . pure . mkAbstractLiteral $ AbsLitPartition members




validateRecordLiteral :: ListNode RecordMemberNode -> Validator (Typed Expression)
validateRecordLiteral ln = do
    members <- validateList_ validateRecordMember ln
    let members' = map (\(x,y) -> (x,untype y)) members
    -- let eType = TypeRecord TypeAny
    return . todoTypeAny . pure $ mkAbstractLiteral $ AbsLitRecord members'

validateVariantLiteral :: ListNode RecordMemberNode -> Validator (Typed Expression)
validateVariantLiteral ln = do
    members <- validateList_ validateRecordMember ln
    case members of
      [] -> invalid $ ln <!> SemanticError "Variants must contain exactly one member"
      [(n,x)]-> return . todoTypeAny . pure $ mkAbstractLiteral $ AbsLitVariant Nothing n (untype x)
      _:_ -> invalid $ ln <!> SyntaxError "Variants must contain exactly one member" --tag subsequent members as unexpected 



validateRecordMember :: RecordMemberNode -> Validator (Name,Typed Expression)
validateRecordMember (RecordMemberNode name lEq expr) = do
    checkSymbols [lEq]
    name' <-  validateName name
    expr' <-  validateExpression expr
    return $ (,) <$> name' <*> expr'

validateFunctionLiteral :: ListNode ArrowPairNode -> Validator (Typed Expression)
validateFunctionLiteral ln = do
    pairs <- validateList_ validateArrowPair ln
    let pairs' = map (\(x,y)->(untype x,untype y)) pairs
    return . todoTypeAny . pure $ mkAbstractLiteral $ AbsLitFunction pairs'

validateSetLiteral :: ListNode ExpressionNode -> Validator (Typed Expression)
validateSetLiteral ls = do
    xs <- validateList_ validateExpression ls
    let eType = TypeMSet tInt
    return . todoTypeAny . pure  $ mkAbstractLiteral $ AbsLitSet (map untype xs)

validateMSetLiteral :: ListNode ExpressionNode -> Validator (Typed Expression)
validateMSetLiteral ls = do
    xs <- validateList validateExpression ls
    let eType = TypeMSet tInt
    let result = mkAbstractLiteral . AbsLitMSet . map (untype . snd) $ xs
    return .pure $ Typed eType result
validateMatrixLiteral :: MatrixLiteralNode -> Validator (Typed Expression)
validateMatrixLiteral (MatrixLiteralNode l1 se m_dom Nothing l2) = do
    checkSymbols [l1,l2]
    elems <-  validateSequence validateExpression se
    dom <-  validateOverDomain m_dom
    let lit = do
            let xs = map (untype . snd) elems
            case dom of
              Just (Just d) -> return $ AbsLitMatrix d xs
              _ -> return $ AbsLitMatrix (mkDomainIntB 1 (fromInt $ genericLength xs)) xs
    pure . typeAs TypeAny $mkAbstractLiteral <$> lit
    where
        validateOverDomain :: Maybe OverDomainNode -> Validator (Maybe (Domain () Expression))
        validateOverDomain Nothing = pure Nothing
        validateOverDomain (Just (OverDomainNode l3 dom)) = checkSymbols [l3] >> Just<$> validateDomain dom



validateMatrixLiteral (MatrixLiteralNode l1 se m_dom (Just comp) l2) = do
    checkSymbols [l1,l2]
    elems <- Just <$> validateSequence_ validateExpression se
    gens <-  validateComprehension comp
    enforceConstraint ((\x -> length x == 1 )<$> elems) "List comprehension must contain exactly one expression before |"
    return $ do
        ms <- elems
        gs <- gens
        case ms of
            [x] -> return  . Typed TypeAny $ Comprehension (untype x) gs
            _ -> Nothing



validateComprehension :: ComprehensionNode -> Validator [GeneratorOrCondition]
validateComprehension (ComprehensionNode l1 body) = do
        checkSymbols [l1]
        pure . concat <$> validateSequence_ validateComprehensionBody body

validateComprehensionBody :: ComprehensionBodyNode -> Validator [GeneratorOrCondition]
validateComprehensionBody (CompBodyCondition en) = do
    Just e <- validateExpression en
    assertType e TypeBool "Guards must be bools"
    return . pure $ [Condition $ untype e]
validateComprehensionBody (CompBodyDomain apn l1 dom) = do
    checkSymbols [l1]
    pats <- validateSequence_ validateAbstractPattern apn
    Just domain <-  validateDomain dom
    return . pure $ [Generator  (GenDomainNoRepr pat domain) | pat <- pats]

validateComprehensionBody (CompBodyGenExpr apn lt en) = do
    checkSymbols [lt]
    pats <-  validateSequence_ validateAbstractPattern apn
    Just exp <-  validateExpression en
    return . pure $ [Generator (GenInExpr pat (untype exp))| pat <- pats]
validateComprehensionBody (CompBodyLettingNode l1 nn l2 en) = do
    checkSymbols [l1,l2]
    pat <-  validateAbstractPattern nn
    expr <-  validateExpression en
    let gen = ComprehensionLetting <$> pat <*> (untype <$> expr)
    return  (( : []) <$> gen)


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

makeTupleLiteral :: [Typed Expression] -> ValidatorS (Typed Expression)
makeTupleLiteral members = do
    let memberTypes = unzip $ map typeSplit members
    let eType = TypeTuple (fst memberTypes)
    return . Typed eType . mkAbstractLiteral . AbsLitTuple $ snd memberTypes


validateIntLiteral :: LToken -> Validator Constant
validateIntLiteral t = do
    l <- validateSymbol t
    case l of
        Just (LIntLiteral x) -> return . pure $ ConstantInt TagInt x
        _ -> invalid $ t <!> InternalError

validateBoolLiteral :: LToken -> Validator Constant
validateBoolLiteral t = do
    Just l <- validateSymbol t
    case l of
        L_true -> return . pure $ ConstantBool True
        L_false -> return . pure $ ConstantBool False
        _ -> invalid $  t <!> InternalError
validateNameList :: Sequence NameNode -> ValidatorS [Name]
validateNameList = validateSequence_ validateName

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

validateName :: NameNode -> Validator Name
validateName name = do
        n <- validateIdentifier name
        return $ (Name <$> n)

validateArray :: (a -> Validator b) -> [a] -> ValidatorS [b]
validateArray f l = catMaybes <$> mapM f l

validateList :: (a -> Validator b) -> ListNode a -> ValidatorS [(a,b)]
validateList validator (ListNode st seq end) = do
    _ <- validateSymbol st
    _ <- validateSymbol end
    validateSequence validator seq

validateList_ :: (a -> Validator b) -> ListNode a -> ValidatorS [b]
validateList_ validator (ListNode st seq end) = do
    _ <- validateSymbol st
    _ <- validateSymbol end
    validateSequence_ validator seq

-- mapPrefixToOp :: Lexeme -> Text
-- mapPrefixToOp x = case x of
--     L_Minus -> "negate"
--     L_ExclamationMark -> "not"
--     _ -> pack $ lexemeFace x
validateSequence :: (a -> Validator b) -> Sequence a -> ValidatorS [(a,b)]
validateSequence f (Seq vals) = validateArray (validateSequenceElem f) vals
validateSequence_ :: (a -> Validator b) -> Sequence a -> ValidatorS [b]
validateSequence_ f s = do
    q <- validateSequence f s
    return . map snd $ q

validateSequenceElem :: (a -> Validator b) -> SeqElem a -> Validator (a,b)
validateSequenceElem f (SeqElem i s) = do
                            case s of 
                              Nothing -> pure ()
                              Just lt -> void $ validateSymbol lt
                            v <- f i
                            return (case v of
                               Nothing -> Nothing
                               Just b -> Just (i,b))
validateSequenceElem f (MissingSeqElem plc sep) = checkSymbols [sep] >> invalid ( plc <!> TokenError plc)

validateExprList :: ListNode ExpressionNode -> ValidatorS [(ExpressionNode,Typed Expression)]
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

getRegion :: Flattenable ETok a => a -> DiagnosticRegion
getRegion a = case range of
  (h :<| rst)  -> do
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


(<!>) :: Flattenable ETok a => a -> ErrorType -> ValidatorDiagnostic
t <!> e = ValidatorDiagnostic (getRegion t) $ Error e

(</!\>) :: Flattenable ETok a => a -> WarningType -> ValidatorDiagnostic
t </!\> e = ValidatorDiagnostic (getRegion t) $ Warning e

(<?>) :: Flattenable ETok a => a -> InfoType -> ValidatorDiagnostic
t <?> e = ValidatorDiagnostic (getRegion t) $ Info e

(<?!>) :: Flattenable ETok a => Maybe a -> ErrorType -> ValidatorDiagnostic
Nothing <?!> e =  ValidatorDiagnostic GlobalRegion $ Error e
Just t <?!> e =  t <!> e

contextError :: ErrorType -> Validator a
contextError e = do
    q <- getContext
    invalid $ ValidatorDiagnostic q $ Error e

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

tryResolveReference :: Name -> ValidatorS (Maybe ReferenceTo)
tryResolveReference (Name n) = do
    c <- getSymbol n
    case c of
      Nothing -> contextError (CustomError . pack $ "Symbol not found "++ show n) >> return Nothing
      Just _ -> return c
tryResolveReference _ = return Nothing


-- unifyAbstractPatternOverExpression :: AbstractPatternNode -> Expression -> Validator (Name,Type)
-- unifyAbstractPatternOverExpression pat exp = do
--     t <- typeOf exp

--     empty
