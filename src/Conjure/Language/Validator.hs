{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Conjure.Language.Validator where

import Conjure.Language.AST.ASTParser
import Conjure.Language.AST.Syntax as S
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Lexemes
import Conjure.Language.NewLexer (ETok (ETok, capture, lexeme), ETokenStream (ETokenStream), eLex, sourcePos0)

import Conjure.Language.Attributes
import Conjure.Prelude

import Control.Monad.Writer.Strict (Writer)

import Conjure.Language.Type

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Text (pack, unpack, toLower)
import Text.Megaparsec (parseMaybe, runParser)

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
import Control.Applicative (empty, Alternative)
import Conjure.Language.Attributes (lexemeToBinRel)


data ValidatorError
    = TypeError String
    | StateError String
    | SyntaxError String
    | RegionError String -- Add region
    | TokenError LToken
    | TaggedTokenError String LToken
    | IllegalToken LToken -- Should not occur in practice and indicates a logical error somewhere
    | NotImplemented String
    deriving (Show)

data SymbolTable = SymbolTable [(Text,String)]
    deriving (Show)

newtype ValidatorT r w a = ValidatorT (MaybeT (StateT r (Writer [w])) a)
    deriving (Monad,Applicative ,Functor,MonadState r ,MonadWriter [w],MonadFail)


--synonym wrapped in maybe to allow errors to propagate
type Validator a = ValidatorT SymbolTable ValidatorError (Maybe a)

--Non maybe version used in outward facing applications/ lists 
type ValidatorS a = ValidatorT SymbolTable ValidatorError a

addEnumDefns ::  [Text] -> SymbolTable -> SymbolTable
addEnumDefns names (SymbolTable enums) = SymbolTable $ enums ++  map (\m -> (m,"Enum")) names

lookupSymbol :: Text -> ValidatorS (Maybe String)
lookupSymbol name = do
    SymbolTable a <- get
    return $ lookup name a

strict :: Validator a -> ValidatorS a
strict a = do Just res <- a; return res

deState :: ((a,r),n) -> (a,n)
deState ((a,_),n) = (a,n)

runValidator :: (ValidatorT r w a) -> r -> ((Maybe a),[w])
runValidator (ValidatorT r) d = deState $ runWriter (runStateT (runMaybeT r) d)
-- data Validator a = Validator
--     { value :: Maybe a
--     , errors :: [ValidatorError]
--     }
--     deriving (Show)

data Foo = Foo Int Int Int
    deriving (Show)

-- instance Functor Validator where
--     fmap :: (a -> b) -> Validator a -> Validator b
--     fmap fab (Validator m_a ves) =
--         Validator
--             { value = case m_a of
--                 Nothing -> Nothing
--                 (Just a) -> Just $ fab a
--             , errors = ves
--             }

-- instance Applicative Validator where
--     pure :: a -> Validator a
--     pure x = Validator (Just x) []
--     (<*>) :: Validator (a -> b) -> Validator a -> Validator b
--     (Validator Nothing es) <*> (Validator _ e2s) = Validator Nothing (es ++ e2s)
--     (Validator (Just f) es) <*> (Validator a e2s) = Validator val (es ++ e2s)
--       where
--         val = case a of
--             Just a' -> Just $ f a'
--             Nothing -> Nothing

-- instance Monad Validator where
--     (>>=) :: Validator a -> (a -> Validator b) -> Validator b
--     (Validator Nothing ves) >>= _ =
--         Validator{value = Nothing, errors = ves}
--     (Validator (Just n) ves) >>= f =
--         let r = f n in r{errors = ves ++ errors r}


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
validateLanguageVersion (Just (LangVersionNode l1 n v)) = do
    let NameNode nt = n
    checkSymbols [l1]
    name <- validateIdentifier n
    unless (maybe False isValidLanguageName name) (raiseError $ IllegalToken nt)
    nums <- validateSequence getNum v
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
                _ -> invalid $ TokenError c


validateStatement :: StatementNode -> Validator [Statement]
validateStatement (DeclarationStatement dsn) = validateDeclarationStatement dsn
validateStatement (BranchingStatement bsn) = validateBranchingStatement bsn
validateStatement (SuchThatStatement stsn) = validateSuchThatStatement stsn
validateStatement (WhereStatement wsn) = validateWhereStatement wsn
validateStatement (ObjectiveStatement osn) = validateObjectiveStatement osn
validateStatement (HeuristicStatement lt exp) = validateHeuristicStatement lt exp
validateStatement (UnexpectedToken lt) = invalid $ TokenError lt

validateHeuristicStatement :: LToken -> ExpressionNode -> Validator [Statement]
validateHeuristicStatement lt exp = do
    checkSymbols [lt]
    _ <- validateExpression exp
    case exp of
      IdentifierNode nn -> do
                    x <- validateName nn
                    return  $ sequence [SearchHeuristic <$> x]
      _ -> invalid $ StateError "Only identifiers are allowed as heuristics"

validateWhereStatement :: WhereStatementNode -> Validator [Statement]
validateWhereStatement (WhereStatementNode l1 exprs) = do
    checkSymbols [l1]
    ws <-  Where <$> validateSequence validateExpression exprs
    return . pure $ [ws]

validateObjectiveStatement :: ObjectiveStatementNode -> Validator [Statement]
validateObjectiveStatement (ObjectiveMin lt en) = do
    checkSymbols [lt]
    Just exp <- validateExpression en
    return . pure $ [Objective Minimising exp]
validateObjectiveStatement (ObjectiveMax lt en) =do
    checkSymbols [lt]
    Just exp <- validateExpression en
    return . pure $ [Objective Maximising exp]

validateSuchThatStatement :: SuchThatStatementNode -> Validator [Statement]
validateSuchThatStatement (SuchThatStatementNode l1 l2 exprs) = do
    checkSymbols [l1, l2]
    exprs' <- validateSequence validateExpression exprs
    return . pure $ [SuchThat  exprs']

validateBranchingStatement :: BranchingStatementNode -> Validator [Statement]
validateBranchingStatement (BranchingStatementNode l1 l2 sts) = do
    checkSymbols [l1, l2]
    branchings <- validateList validateBranchingParts sts
    return . pure $ [SearchOrder branchings]
    where
        validateBranchingParts :: ExpressionNode -> Validator SearchOrder
        validateBranchingParts (IdentifierNode nn) =  do
            n <- validateName nn
            return $ BranchingOn <$> n
        validateBranchingParts exp = do
            x <- validateExpression exp
            return $ Cut <$> x

validateDeclarationStatement :: DeclarationStatementNode -> Validator [Statement]
validateDeclarationStatement stmt = do
    Just stmt' <- case stmt of
        FindStatement l1 fs -> checkSymbols [l1] >> validateStatementSeq validateFind fs
        GivenStatement l1 gs -> checkSymbols [l1] >> validateStatementSeq validateGiven gs
        LettingStatement l1 ls -> checkSymbols [l1] >> validateStatementSeq validateLetting ls
    return . pure $ Declaration <$> stmt'
    where
        validateStatementSeq v l= do
            decls <- validateSequence v l
            return $ pure $ concat decls

validateGiven :: GivenStatementNode -> Validator [Declaration]
validateGiven (GivenStatementNode idents l1 domain) =
    do
        checkSymbols [l1]
        names <-  validateNameList idents
        Just dom <-  validateDomain domain
        return . pure $ [ FindOrGiven Given nm dom|nm <- names ]
validateGiven (GivenEnumNode se l1 l2 l3) =
    do
        checkSymbols [l1, l2, l3]
        names <-  validateNameList se
        modify $ addEnumDefns [ n | Name n <-names]
        return . pure $  [GivenDomainDefnEnum n | n <- names]

validateLetting :: LettingStatementNode -> Validator [Declaration]
-- Letting [names] be
validateLetting (LettingStatementNode names l1 assign) = do
    checkSymbols [l1]
    names' <-  validateNameList names
    validateLettingAssignment names' assign

validateLettingAssignment :: [Name] -> LettingAssignmentNode -> Validator [Declaration]
validateLettingAssignment names (LettingExpr en)  = do
    Just expr <- validateExpression en
    return . pure $ [Letting n expr | n <- names]
validateLettingAssignment names (LettingDomain lt dn) = do
    checkSymbols [lt]
    Just domain <- validateDomain dn
    return . pure $ [Letting n  (Domain domain)| n <- names]
validateLettingAssignment names (LettingEnum l1 l2 l3 enames) = do
    checkSymbols [l1, l2, l3]
    members <- validateList validateName enames
    modify $ addEnumDefns [ n | Name n <-names]
    return . pure $ [LettingDomainDefnEnum n members| n <- names]
validateLettingAssignment names (LettingAnon l1 l2 l3 l4 szExp) = do
    checkSymbols [l1, l2, l3, l4]
    Just size <- validateExpression szExp
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

invalid :: ValidatorError -> Validator a
invalid err = do
    raiseError err
    return Nothing
    --  Validator Nothing [err]

-- rg :: String
-- rg = case g of
--     (Validator x es) -> show (x, es)

-- type Checker a = State [ValidatorError] (Maybe a)

validateSymbol :: LToken -> Validator Lexeme
validateSymbol s =
    case s of
        RealToken et -> return . pure  $ lexeme et
        _ -> invalid $ TokenError s

-- [MissingTokenError ]

validateFind :: FindStatementNode -> Validator [Declaration]
validateFind (FindStatementNode names colon domain) = do
    checkSymbols [colon]
    names' <- validateNameList names
    domain' <- validateDomain domain
    return $ map <$> (makeFind <$> domain') <*> pure names'
  where
    makeFind :: Domain () Expression -> Name -> Declaration
    makeFind dom nm = FindOrGiven Find nm dom

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
    -- EnumDomainNode nn -> validateNamedEnumDomain nn
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
    MissingDomainNode lt -> invalid $ TokenError lt
  where
    validateRangedInt :: Maybe (ListNode RangeNode) -> DomainValidator
    validateRangedInt (Just ranges) = do
        ranges' <- validateList validateRange ranges
        return . pure $ DomainInt TagInt ranges'
    validateRangedInt Nothing = return . pure $ DomainInt TagInt []
    validateEnumRange :: NameNode -> Maybe (ListNode RangeNode) -> DomainValidator
    validateEnumRange name ranges = do
        ranges' <- case ranges of
            Just r -> pure <$> validateList validateRange r
            Nothing -> pure Nothing
        Just name' <- validateIdentifier name
        a <- lookupSymbol name'
        case a of
            Just "Enum" ->return . pure $ DomainEnum (Name name') ranges' Nothing
            Just t -> invalid $ StateError $ "Unknown type :" ++ t
            Nothing -> case ranges' of
              Nothing -> return . pure $  DomainReference (Name name') Nothing
              Just _ -> do
                raiseError (StateError "range not supported on non enum ranges")
                return . pure $  DomainReference (Name name') Nothing

    validateTupleDomain :: ListNode DomainNode -> DomainValidator
    validateTupleDomain doms = pure . DomainTuple <$> validateList validateDomain doms
    validateRecordDomain :: ListNode NamedDomainNode -> DomainValidator
    validateRecordDomain namedDoms = pure . DomainRecord <$> validateList validateNamedDomain namedDoms
    validateVariantDomain :: ListNode NamedDomainNode -> DomainValidator
    validateVariantDomain namedDoms = do
                lst <- validateList validateNamedDomain namedDoms
                return . pure $ DomainVariant lst
    validateMatrixDomain :: ListNode DomainNode -> DomainNode -> DomainValidator
    validateMatrixDomain indexes dom = do
        idoms <-  validateList validateDomain indexes
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
        doms' <-  validateList validateDomain doms
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

todo :: String -> Validator a
todo s = invalid $ NotImplemented s

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
      as -> do invalid $ RegionError $ "Incompatible attributes size:" ++ show as

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
      as -> do invalid $ RegionError $ "Incompatible attributes partitionSize :" ++ show as

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
      as -> do invalid $ RegionError $ "Incompatible attributes partitionSize :" ++ show as


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
        info "Inj and Sur can be combined to bijective"
        return $ Just JectivityAttr_Bijective
      as -> do invalid $ RegionError $ "Incompatible attributes jectivity" ++ show as


validateSetAttributes :: ListNode AttributeNode -> Validator (SetAttr Expression)
validateSetAttributes atts = do
    attrs <- validateList (validateAttributeNode setValidAttrs) atts
    size <- validateSizeAttributes attrs
    return $ SetAttr <$> size


validateMSetAttributes :: ListNode AttributeNode -> Validator (MSetAttr Expression)
validateMSetAttributes atts = do
    attrs <- validateList (validateAttributeNode msetValidAttrs) atts
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
                    as -> invalid $ StateError $ "Bad args to occurs" ++ show as


validateFuncAttributes :: ListNode AttributeNode -> Validator (FunctionAttr Expression)
validateFuncAttributes atts = do
    attrs <- validateList (validateAttributeNode funAttrs) atts
    size <- validateSizeAttributes attrs
    parts <- return . Just $ if L_total `elem` map fst attrs then PartialityAttr_Total else PartialityAttr_Partial
    jectivity <- validateJectivityAttributes attrs
    return $ FunctionAttr <$> size <*> parts <*> jectivity

validateSeqAttributes :: ListNode AttributeNode -> Validator (SequenceAttr Expression)
validateSeqAttributes atts = do
    attrs <- validateList (validateAttributeNode seqAttrs) atts
    size <- validateSizeAttributes attrs
    jectivity <- validateJectivityAttributes attrs
    return $ SequenceAttr <$> size <*> jectivity


validateRelationAttributes :: ListNode AttributeNode -> Validator (RelationAttr Expression)
validateRelationAttributes atts = do
    attrs <- validateList (validateAttributeNode relAttrs) atts
    size <- validateSizeAttributes attrs
    others <- validateArray validateBinaryRel (filter (\x -> fst x `elem` map fst binRelAttrs) attrs)
    return $ RelationAttr <$>  size <*> pure (BinaryRelationAttrs $ S.fromList others )
        where
            validateBinaryRel :: (Lexeme , Maybe Expression) -> Validator BinaryRelationAttr
            validateBinaryRel (l,_) = do
                case lexemeToBinRel l of
                    Just b -> return . pure $ b
                    Nothing -> invalid $ StateError  $ "Not found (bin rel) " ++ show l

validatePartitionAttributes :: ListNode AttributeNode -> Validator (PartitionAttr Expression)
validatePartitionAttributes atts = do
    attrs <- validateList (validateAttributeNode partitionAttrs) atts
    --guard size attrs and complete as this is default
    size <- validateNumPartAttributes attrs
    partSize <- validatePartSizeAttributes attrs
    regular <- return . Just $ L_regular `elem` map fst attrs
    return $ PartitionAttr <$> size <*> partSize <*> regular

validateAttributeNode :: Map Lexeme Bool -> AttributeNode -> Validator (Lexeme,Maybe Expression)
validateAttributeNode vs (NamedAttributeNode t Nothing) = do
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ TokenError t
      Just  True -> invalid $ RegionError  "Argument required"
      Just False ->  return . pure $ (name , Nothing)

validateAttributeNode vs (NamedAttributeNode t (Just e)) = do
    expr <- validateExpression e
    Just name <- validateSymbol t
    case M.lookup name vs of
      Nothing -> invalid $ TaggedTokenError "Not a valid attr " t
      Just False -> invalid $ RegionError "name: does not take an argument"
      Just True -> return $(\x -> (name,Just x)) <$> expr


validateNamedDomain :: NamedDomainNode -> Validator (Name, Domain () Expression)
validateNamedDomain (NameDomainNode name l1 domain) = do
    checkSymbols [l1]
    name' <-  validateName name
    domain' <- validateDomain domain
    return $  (,) <$> name' <*> domain'

validateRange :: RangeNode -> Validator (Range Expression)
validateRange range = case range of
    SingleRangeNode en -> do ex <- validateExpression en ; return  $ RangeSingle <$> ex
    OpenRangeNode dots -> do checkSymbols [dots] ; return . pure $ RangeOpen
    RightUnboundedRangeNode e1 dots -> do checkSymbols [dots] ; ex <- validateExpression e1 ; return $ RangeLowerBounded <$>ex
    LeftUnboundedRangeNode dots e1 -> do checkSymbols [dots] ;  ex <- validateExpression e1 ; return $ RangeUpperBounded <$> ex
    BoundedRangeNode e1 dots e2 -> do
        _ <- checkSymbols [dots]
        e1' <-  validateExpression e1
        e2' <-  validateExpression e2
        return $ RangeBounded <$> e1' <*> e2'

validateArrowPair :: ArrowPairNode -> Validator (Expression, Expression)
validateArrowPair (ArrowPairNode e1 s e2) = do
    checkSymbols [s]
    e1' <-  validateExpression e1
    e2' <-  validateExpression e2
    return $ (,) <$> e1' <*> e2'

validateExpression :: ExpressionNode -> Validator Expression
validateExpression expr = case expr of
    Literal ln -> validateLiteral ln
    IdentifierNode nn -> validateIdentifierExpr nn
    MetaVarExpr tok -> do x <- validateMetaVar tok ; return $ ExpressionMetaVar <$> x
    QuantificationExpr qen -> validateQuantificationExpression qen
    OperatorExpressionNode oen -> validateOperatorExpression oen
    DomainExpression dex -> validateDomainExpression dex
    ParenExpression (ParenExpressionNode l1 exp l2) -> checkSymbols [l1,l2] >> validateExpression exp
    AbsExpression (ParenExpressionNode l1 exp l2) -> do
        checkSymbols [l1,l2]
        Just exp' <- validateExpression exp
        return . pure $ mkOp TwoBarOp  [exp']
    FunctionalApplicationNode lt ln -> validateFunctionApplication  lt ln
    AttributeAsConstriant lt exprs -> validateAttributeAsConstraint lt exprs
    SpecialCase  scn ->  validateSpecialCase scn
    MissingExpressionNode lt -> invalid $ TokenError lt

validateAttributeAsConstraint :: LToken -> ListNode ExpressionNode -> Validator Expression
validateAttributeAsConstraint l1 exprs = do
    checkSymbols [l1]
    es <- validateList validateExpression exprs
    do
        Just lx <- validateSymbol l1
        let n = lookup (Name (lexemeText lx)) allSupportedAttributes
        case (n,es) of
          (Just 1 , [e,v]) -> return . pure  $ aacBuilder e lx (Just v)
          (Just 1 , _) -> invalid $ StateError $ "Expected 2 args to " ++ (show lx)  ++ "got" ++ (show $ length es)
          (Just 0 , [e]) -> return . pure $ aacBuilder e lx Nothing
          (Just 0 , _) -> invalid $ StateError $ "Expected 1 arg to " ++ (show lx)  ++ "got" ++ (show $ length es)
          (_,_) -> invalid (IllegalToken l1)
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
    return (WithLocals <$> expr <*> pure locals)

translateQnName :: Lexeme -> OpType
translateQnName qnName = case qnName of
    L_ForAll -> FunctionOp L_fAnd
    L_Exists -> FunctionOp L_fOr
    _        -> FunctionOp qnName

validateQuantificationExpression :: QuantificationExpressionNode -> Validator Expression
validateQuantificationExpression (QuantificationExpressionNode name pats over m_guard dot expr) =
    do
        checkSymbols [dot]
        name' <-  validateSymbol name
        patterns <-  validateSequence validateAbstractPattern pats
        over' <-  validateQuantificationOver over
        guard' <-  validateQuantificationGuard m_guard
        body <-  validateExpression expr
        let gens = map  <$>  over' <*> pure patterns
        let qBody =  Comprehension <$> body  <*> ((++) <$>  gens <*> guard')
        return  $ mkOp <$> (translateQnName <$> name') <*> ((:[]) <$> qBody)
    where
        validateQuantificationGuard :: Maybe QuanticationGuard -> Validator [GeneratorOrCondition]
        validateQuantificationGuard Nothing = return $ pure []
        validateQuantificationGuard (Just (QuanticationGuard l1 exp) ) = do
            checkSymbols [l1]
            Just expr' <- validateExpression exp
            return . pure $ [Condition expr']
        validateQuantificationOver :: QuantificationOverNode -> Validator (AbstractPattern -> GeneratorOrCondition)
        validateQuantificationOver ( QuantifiedSubsetOfNode lt en ) = do
            checkSymbols [lt]
            Just exp <- validateExpression en
            return . pure $ (\pat -> Generator $ GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet exp))
        validateQuantificationOver ( QuantifiedMemberOfNode lt en ) = do
            checkSymbols [lt]
            Just exp <- validateExpression en
            return . pure $ (\pat -> Generator $ GenInExpr pat exp)
        validateQuantificationOver ( QuantifiedDomainNode (OverDomainNode l1 dom) ) = do
            checkSymbols [l1]
            Just dom' <- validateDomain dom
            return . pure $ (\pat -> Generator $ GenDomainNoRepr pat dom')



validateAbstractPattern :: AbstractPatternNode -> Validator AbstractPattern
validateAbstractPattern (AbstractIdentifier nn) = validateName nn >>= \x -> return $ Single <$> x
validateAbstractPattern (AbstractMetaVar lt) =  validateMetaVar lt >>= \x -> return $ AbstractPatternMetaVar <$> x
validateAbstractPattern (AbstractPatternMatrix ln) = pure . AbsPatMatrix <$> validateList validateAbstractPattern ln
validateAbstractPattern (AbstractPatternSet ln) = pure . AbsPatSet <$> validateList validateAbstractPattern ln
validateAbstractPattern (AbstractPatternTuple m_lt ln) = do
    maybe (pure ()) (\n ->checkSymbols [n]) m_lt
    pure . AbsPatTuple <$> validateList validateAbstractPattern ln

validateMetaVar :: LToken -> Validator String
validateMetaVar tok = do
    Just lx <- validateSymbol tok
    case lx of
        LMetaVar s -> return .pure  $ unpack s
        _ -> invalid $ IllegalToken tok

validateDomainExpression :: DomainExpressionNode -> Validator Expression
validateDomainExpression (DomainExpressionNode  l1 dom l2) = do
    checkSymbols [l1,l2]
    dom' <- validateDomain dom
    return $ Domain <$> dom'

validateFunctionApplication :: LToken -> ListNode ExpressionNode -> Validator Expression
validateFunctionApplication name args = do
    name' <-  validateSymbol name
    args' <-  validateList validateExpression args
    return $ do
        n <- name'
        let a = args'
        case (n,a) of
            (L_image,[y,z]) -> return $ Op $  MkOpImage $ OpImage y z
            _ -> return $ mkOp (FunctionOp n) a


validateIdentifierExpr :: NameNode -> Validator Expression
validateIdentifierExpr name = do
    n <- validateIdentifier name
    return $ Reference <$> (Name <$> n) <*> pure Nothing

validateOperatorExpression :: OperatorExpressionNode -> Validator Expression
validateOperatorExpression (PrefixOpNode lt expr) = do
    expr <-  validateExpression expr
    Just op <-  validateSymbol lt
    return $ (\x -> mkOp (PrefixOp op) [x]) <$> (expr)
    --lookup symbol
validateOperatorExpression (BinaryOpNode lexp op rexp) = do
    lExpr <-  validateExpression lexp
    rExpr <-  validateExpression rexp
    op' <-  validateSymbol op
    return $ mkBinOp <$> ( pack . lexemeFace <$> op') <*> lExpr <*> rExpr
validateOperatorExpression (PostfixOpNode expr pon) = do
    expr' <-  validateExpression expr
    postFixOp <-  validatePostfixOp pon
    return $ postFixOp <*> expr'

validatePostfixOp :: PostfixOpNode -> Validator (Expression -> Expression)
validatePostfixOp (OpFactorial lt) = do
        checkSymbols [lt]
        return . pure $ (\x -> mkOp FactorialOp [x])
validatePostfixOp (ApplicationNode args) = do
        args' <- validateList validateExpression args
        let underscore = Reference "_" Nothing
        let ys = [if underscore == x then Nothing else Just x | x <- args']
        return . pure $ \ x -> Op $ MkOpRelationProj $ OpRelationProj x ys
validatePostfixOp (IndexedNode ln) = do
        ranges <-validateList validateRange ln
        let indices = map interpretRange ranges
        return . pure  $ \x -> (foldl (\m f -> f m)) x indices
        where
            interpretRange :: Range Expression -> (Expression-> Expression)
            interpretRange x =
                let a = case x of
                            RangeOpen -> Right (Nothing,Nothing)
                            RangeSingle ex -> Left ex
                            RangeLowerBounded ex -> Right (Just ex,Nothing)
                            RangeUpperBounded ex -> Right (Nothing,Just ex)
                            RangeBounded exl exr -> Right (Just exl,Just exr)
                in case a of
                  Left ex -> \m -> Op $ MkOpIndexing (OpIndexing m ex)
                  Right (i,j) -> \m -> Op $ MkOpSlicing (OpSlicing m i j)
validatePostfixOp (ExplicitDomain l1 l2 dom l3) = do
    checkSymbols [l1,l2,l3]
    Just dom' <- validateDomain dom
    let t =  getType dom'
    case t of
      Nothing -> invalid $ StateError $ "Some type bug with:" ++ show dom'
      Just ty -> return . pure $ (\ex -> Typed ex ty)
    where
        getType :: Domain () Expression -> Maybe Type
        getType d = let ?typeCheckerMode = StronglyTyped in typeOfDomain d



validateLiteral :: LiteralNode -> Validator Expression
validateLiteral litNode = case litNode of
    IntLiteral lt -> validateIntLiteral lt >>= \x -> return $ Constant <$> x
    BoolLiteral lt -> validateBoolLiteral lt >>= \x -> return $ Constant <$> x
    MatrixLiteral mln -> validateMatrixLiteral mln
    TupleLiteralNode lt ->  Just . mkAbstractLiteral . AbsLitTuple <$> validateLongTuple lt
    TupleLiteralNodeShort st -> Just . mkAbstractLiteral.AbsLitTuple <$> validateShortTuple st
    RecordLiteral lt ln -> checkSymbols [lt] >> validateRecordLiteral ln
    VariantLiteral lt ln -> checkSymbols [lt] >> validateVariantLiteral ln
    SetLiteral ls -> validateSetLiteral ls
    MSetLiteral lt ls -> checkSymbols [lt] >> validateMSetLiteral ls
    FunctionLiteral lt ln -> checkSymbols [lt] >> validateFunctionLiteral ln
    SequenceLiteral lt ln -> checkSymbols [lt] >> validateSequenceLiteral ln
    RelationLiteral lt ln -> checkSymbols [lt] >> validateRelationLiteral ln
    PartitionLiteral lt ln -> checkSymbols [lt] >> validatePartitionLiteral ln

validateSequenceLiteral :: ListNode ExpressionNode -> Validator Expression
validateSequenceLiteral x = do
    l <-  validateExprList x
    return . pure $ mkAbstractLiteral  $ AbsLitSequence l


validateRelationLiteral :: ListNode RelationElemNode -> Validator Expression
validateRelationLiteral ln = do
    members <- validateList validateRelationMember ln
    return . pure $ mkAbstractLiteral $ AbsLitRelation members
    where
        validateRelationMember :: RelationElemNode -> Validator [Expression]
        validateRelationMember x = case x of
          RelationElemNodeLabeled lt -> Just <$> validateLongTuple lt
          RelationElemNodeShort st -> Just <$> validateShortTuple st


validatePartitionLiteral :: ListNode PartitionElemNode -> Validator Expression
validatePartitionLiteral ln = do
    members <- validateList (\(PartitionElemNode exprs) -> Just <$> validateExprList exprs) ln
    return . pure . mkAbstractLiteral $ AbsLitPartition members




validateRecordLiteral :: ListNode RecordMemberNode -> Validator Expression
validateRecordLiteral ln = do
    members <- validateList validateRecordMember ln
    return . pure $ mkAbstractLiteral $ AbsLitRecord members

validateVariantLiteral :: ListNode RecordMemberNode -> Validator Expression
validateVariantLiteral ln = do
    members <- validateList validateRecordMember ln
    case members of
      [] -> invalid $ SyntaxError "Variants must contain exactly one member"
      [(n,x)]-> return . pure $ mkAbstractLiteral $ AbsLitVariant Nothing n x
      _:_ -> invalid $ SyntaxError "Variants must contain exactly one member" --tag subsequent members as unexpected 



validateRecordMember :: RecordMemberNode -> Validator (Name,Expression)
validateRecordMember (RecordMemberNode name lEq expr) = do
    checkSymbols [lEq]
    name' <-  validateName name
    expr' <-  validateExpression expr
    return $ (,) <$> name' <*> expr'

validateFunctionLiteral :: ListNode ArrowPairNode -> Validator Expression
validateFunctionLiteral ln = do
    pairs <- validateList validateArrowPair ln
    return . pure $ mkAbstractLiteral $ AbsLitFunction pairs

validateSetLiteral :: ListNode ExpressionNode -> Validator Expression
validateSetLiteral ls = do
    xs <- validateList validateExpression ls
    return . pure  $ mkAbstractLiteral $ AbsLitSet xs

validateMSetLiteral :: ListNode ExpressionNode -> Validator Expression
validateMSetLiteral ls = do
        xs <- validateList validateExpression ls
        return .pure $ mkAbstractLiteral $ AbsLitMSet xs
validateMatrixLiteral :: MatrixLiteralNode -> Validator Expression
validateMatrixLiteral (MatrixLiteralNode l1 se m_dom Nothing l2) = do
    checkSymbols [l1,l2]
    elems <-  validateSequence validateExpression se
    dom <-  validateOverDomain m_dom
    let lit = do
            let xs = elems
            case dom of
              Just (Just d) -> return $ AbsLitMatrix d xs
              _ -> return $ AbsLitMatrix (mkDomainIntB 1 (fromInt $ genericLength xs)) xs
    pure . mkAbstractLiteral <$> lit
    where
        validateOverDomain :: Maybe OverDomainNode -> Validator (Maybe (Domain () Expression))
        validateOverDomain Nothing = pure Nothing
        validateOverDomain (Just (OverDomainNode l3 dom)) = checkSymbols [l3] >> Just<$> validateDomain dom



validateMatrixLiteral (MatrixLiteralNode l1 se m_dom (Just comp) l2) = do
    checkSymbols [l1,l2]
    elems <- Just <$> validateSequence validateExpression se
    gens <-  validateComprehension comp
    enforceConstraint ((\x -> length x == 1 )<$> elems) "List comprehension must contain exactly one expression before |"
    return $ do
        ms <- elems
        gs <- gens
        case ms of
            [x] -> return $ Comprehension x gs
            _ -> Nothing



validateComprehension :: ComprehensionNode -> Validator [GeneratorOrCondition]
validateComprehension (ComprehensionNode l1 body) = do
        checkSymbols [l1]
        pure . concat <$> validateSequence validateComprehensionBody body

validateComprehensionBody :: ComprehensionBodyNode -> Validator [GeneratorOrCondition]
validateComprehensionBody (CompBodyCondition en) = do
    Just e <- validateExpression en
    return . pure $ [Condition e]
validateComprehensionBody (CompBodyDomain apn l1 dom) = do
    checkSymbols [l1]
    pats <- validateSequence validateAbstractPattern apn
    Just domain <-  validateDomain dom
    return . pure $ [Generator  (GenDomainNoRepr pat domain) | pat <- pats]

validateComprehensionBody (CompBodyGenExpr apn lt en) = do
    checkSymbols [lt]
    pats <-  validateSequence validateAbstractPattern apn
    Just exp <-  validateExpression en
    return . pure $ [Generator (GenInExpr pat exp)| pat <- pats]
validateComprehensionBody (CompBodyLettingNode l1 nn l2 en) = do
    checkSymbols [l1,l2]
    pat <-  validateAbstractPattern nn
    expr <-  validateExpression en
    let gen = ComprehensionLetting <$> pat <*> expr
    return  (( : []) <$> gen)


mkAbstractLiteral :: AbstractLiteral Expression -> Expression
mkAbstractLiteral x = case e2c (AbstractLiteral x) of
                        Nothing -> AbstractLiteral x
                        Just c -> Constant c


enforceConstraint :: Maybe Bool -> String -> ValidatorS ()
enforceConstraint p msg = do
    case p of
        Just True-> return ()
        _ -> invalid (StateError msg) >> fail ""



checkSymbols :: [LToken] -> ValidatorS ()
checkSymbols = mapM_ validateSymbol

--Raise a non structural error (i.e type error)
raiseError :: ValidatorError -> ValidatorS ()
raiseError e = tell [e]

type ValidatorInfo = String
--todo warn and info
info :: ValidatorInfo -> ValidatorS ()
info v = tell [RegionError $ "info : " ++ v]



validateShortTuple :: ShortTuple -> ValidatorS [Expression]
validateShortTuple (ShortTuple exs) = validateList validateExpression exs

validateLongTuple :: LongTuple -> ValidatorS [Expression]
validateLongTuple (LongTuple lt exs) = checkSymbols [lt] >> validateList validateExpression exs

validateIntLiteral :: LToken -> Validator Constant
validateIntLiteral t = do
    l <- validateSymbol t
    case l of
        Just (LIntLiteral x) -> return . pure $ ConstantInt TagInt x
        _ -> invalid $ IllegalToken t

validateBoolLiteral :: LToken -> Validator Constant
validateBoolLiteral t = do
    Just l <- validateSymbol t
    case l of
        L_true -> return . pure $ ConstantBool True
        L_false -> return . pure $ ConstantBool False
        _ -> invalid $ IllegalToken t

validateNameList :: Sequence NameNode -> ValidatorS [Name]
validateNameList = validateSequence validateName

validateIdentifier :: NameNode -> Validator Text
validateIdentifier (NameNode iden) = do
    Just q <-  validateSymbol iden
    case q of
        LIdentifier x -> checkName x
        _ -> return Nothing
    where
        checkName :: Text -> Validator Text
        checkName "" = invalid $ StateError "Empty names not allowed"
        checkName "\"\"" = invalid $ StateError "Empty names not allowed"
        checkName x = return . pure $ x

validateName :: NameNode -> Validator Name
validateName name = do
        n <- validateIdentifier name
        return $ (Name <$> n)

validateArray :: (a -> Validator b) -> [a] -> ValidatorS [b]
validateArray f l = catMaybes <$> mapM f l

validateList :: (a -> Validator b) -> ListNode a -> ValidatorS [b]
validateList validator (ListNode st seq end) = do
    _ <- validateSymbol st
    _ <- validateSymbol end
    validateSequence validator seq

-- mapPrefixToOp :: Lexeme -> Text
-- mapPrefixToOp x = case x of
--     L_Minus -> "negate"
--     L_ExclamationMark -> "not"
--     _ -> pack $ lexemeFace x
validateSequence :: (a -> Validator b) -> Sequence a -> ValidatorS [b]
validateSequence f (Seq vals) = validateArray (validateSequenceElem f) vals

validateSequenceElem :: (a -> Validator b) -> SeqElem a -> Validator b
validateSequenceElem f (SeqElem i (Just x)) = validateSymbol x >> f i
validateSequenceElem f (SeqElem i Nothing) = f i
validateSequenceElem f (MissingSeqElem plc sep) = checkSymbols [sep] >> invalid (TokenError plc)

validateExprList :: ListNode ExpressionNode -> ValidatorS [Expression]
validateExprList = validateList validateExpression


val :: String -> IO ()
val s = do
    let str = s
    let other = [ETok (0, 0, 0, sourcePos0) [] L_EOF ""]
    let txt = pack str
    let lexed = parseMaybe eLex txt
    let stream = ETokenStream txt $ fromMaybe other lexed
    -- parseTest parseProgram stream
    let progStruct = runParser parseProgram "TEST" stream

    case progStruct of
        Left _ -> putStrLn "error"
        Right p@(ProgramTree{}) -> let qpr = runValidator (validateModel p) (SymbolTable []) in
            putStrLn $ show qpr


valFile :: String -> IO ()
valFile p = do
    path <- readFileIfExists p
    case path of
      Nothing -> putStrLn "NO such file"
      Just s -> val s
    return ()
-- putStrLn validateFind