{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Validator where

import Conjure.Language.AST.ASTParser
import Conjure.Language.AST.Syntax as S
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Lexemes
import Conjure.Language.NewLexer (ETok (ETok, capture, lexeme), ETokenStream (ETokenStream), eLex, sourcePos0)
import Conjure.Prelude
import Control.Applicative

import Conjure.Language.Type
import Data.Text (pack, unpack)
import Text.Megaparsec (parseMaybe, runParser)
import Conjure.Language (mkOp, mkBinOp, Op (MkOpRelationProj, MkOpSlicing, MkOpIndexing), OpRelationProj (OpRelationProj), OpIndexing (OpIndexing), opPowerSet)
import Conjure.Language.Expression.Op (OpSlicing(..))
import Conjure.Language (Op(..))
import Conjure.Language (OpPowerSet(..), OpImage (OpImage))

validateModel :: ProgramTree -> Either Doc Model
validateModel model = Left "TODO"

validateProgramTree :: ProgramTree -> Validator [Statement]
validateProgramTree (ProgramTree sts _) = do
    q <- validateArray validateStatement sts
    return $ concat q
    -- todo "all"

validateStatement :: StatementNode -> Validator [Statement]
validateStatement (DeclarationStatement dsn) = validateDeclarationStatement dsn
validateStatement (BranchingStatement bsn) = validateBranchingStatement bsn
validateStatement (SuchThatStatement stsn) = validateSuchThatStatement stsn
validateStatement (WhereStatement wsn) = validateWhereStatement wsn
validateStatement (ObjectiveStatement osn) = validateObjectiveStatement osn
validateStatement (UnexpectedToken lt) = invalid $ TokenError lt

validateWhereStatement :: WhereStatementNode -> Validator [Statement]
validateWhereStatement (WhereStatementNode l1 exprs) = do
    checkSymbols [l1] >> sequence [Where <$> validateSequence validateExpression exprs]

validateObjectiveStatement :: ObjectiveStatementNode -> Validator [Statement]
validateObjectiveStatement (ObjectiveMin lt en) =
    checkSymbols [lt] >> sequence [Objective Minimising <$> validateExpression en]
validateObjectiveStatement (ObjectiveMax lt en) =
    checkSymbols [lt] >> sequence [Objective Maximising <$> validateExpression en]

validateSuchThatStatement :: SuchThatStatementNode -> Validator [Statement]
validateSuchThatStatement (SuchThatStatementNode l1 l2 exprs) = do
    checkSymbols [l1, l2]
    exprs' <- validateSequence validateExpression exprs
    return [SuchThat exprs']

validateBranchingStatement :: BranchingStatementNode -> Validator [Statement]
validateBranchingStatement (BranchingStatementNode l1 l2 statements) = do
    checkSymbols [l1, l2]
    statements <- todo "branching"
    todo "branching"

validateDeclarationStatement :: DeclarationStatementNode -> Validator [Statement]
validateDeclarationStatement stmt =
    fmap Declaration <$> case stmt of
        FindStatement fsn -> validateFind fsn
        GivenStatement gsn -> validateGiven gsn
        LettingStatement lsn -> validateLetting lsn

validateGiven :: GivenStatementNode -> Validator [Declaration]
validateGiven (GivenStatementNode l1 idents l2 domain) =
    do
        checkSymbols [l1, l2]
        names <- validate $ validateNameList idents
        dom <- validate $ validateDomain domain
        verify $ zipWith (FindOrGiven Given) <$> names <*> (repeat <$> dom)
validateGiven (GivenEnumNode l1 se l2 l3 l4) =
    do
        checkSymbols [l1, l2, l3, l4]
        names <- validate $ validateNameList se
        verify $ fmap GivenDomainDefnEnum <$> names

validateLetting :: LettingStatementNode -> Validator [Declaration]
-- Letting [names] be
validateLetting (LettingStatementNode l1 names l2 assign) = do
    checkSymbols [l1, l2]
    names' <- validate $ validateNameList names
    assignment <- validateLettingAssignment assign
    verify $ fmap assignment <$> names'

validateLettingAssignment :: LettingAssignmentNode -> Validator (Name -> Declaration)
validateLettingAssignment (LettingExpr en) = do
    expr <- validateExpression en
    return (`Letting` expr)
validateLettingAssignment (LettingDomain lt dn) = do
    checkSymbols [lt]
    domain <- validateDomain dn
    return (`Letting` Domain domain)
validateLettingAssignment (LettingEnum l1 l2 l3 names) = do
    checkSymbols [l1, l2, l3]
    members <- validateList validateName names
    return (`LettingDomainDefnEnum` members)
validateLettingAssignment (LettingAnon l1 l2 l3 l4 szExp) = do
    checkSymbols [l1, l2, l3, l4]
    size <- validateExpression szExp
    return (`LettingDomainDefnUnnamed` size)

data ValidatorError
    = TypeError String
    | TokenError LToken
    | IllegalToken LToken -- Should not occur in practice and indicates a logical error somewhere
    | NotImplemented String
    deriving (Show)

data Validator a = Validator
    { value :: Maybe a
    , errors :: [ValidatorError]
    }
    deriving (Show)

-- data Validated a = Just a | Nothing

-- instance Functor Validated where
--     fmap :: (a -> b) -> Validated a -> Validated b
--     fmap fab (Just a) = Just (fab a)
--     fmap _ Nothing = Nothing

-- instance Applicative Validated where
--     pure :: a -> Validated a
--     pure = Valid
--     (<*>) :: Validated (a -> b) -> Validated a -> Validated b
--     (Just fab) <*> (Just a) = Just (fab a)
--     Nothing <*> (Just _) = Nothing
--     _ <*> Nothing = Nothing

data Foo = Foo Int Int Int
    deriving (Show)

instance Functor Validator where
    fmap :: (a -> b) -> Validator a -> Validator b
    fmap fab (Validator m_a ves) =
        Validator
            { value = case m_a of
                Nothing -> Nothing
                (Just a) -> Just $ fab a
            , errors = ves
            }

instance Applicative Validator where
    pure :: a -> Validator a
    pure x = Validator (Just x) []
    (<*>) :: Validator (a -> b) -> Validator a -> Validator b
    (Validator Nothing es) <*> (Validator _ e2s) = Validator Nothing (es ++ e2s)
    (Validator (Just f) es) <*> (Validator a e2s) = Validator val (es ++ e2s)
      where
        val = case a of
            Just a' -> Just $ f a'
            Nothing -> Nothing

instance Monad Validator where
    (>>=) :: Validator a -> (a -> Validator b) -> Validator b
    (Validator Nothing ves) >>= _ =
        Validator{value = Nothing, errors = ves}
    (Validator (Just n) ves) >>= f =
        let r = f n
         in r{errors = ves ++ errors r}

validate :: Validator a -> Validator (Maybe a)
validate n = do
    case n of
        Validator Nothing ves -> Validator (Just Nothing) ves
        Validator (Just a) ves -> Validator (Just $ Just a) ves

getPrefix :: Validator Int
getPrefix = Validator Nothing [TypeError "ERR"]

g :: Validator Foo
g = do
    _ <- validate getPrefix
    a <- validate $ do return 1
    b <- validate $ do return 1 :: Validator Int
    c <- validate $ do return 1
    verify $ Foo <$> a <*> b <*> c

verify :: Maybe a -> Validator a
verify (Just a) = Validator{value = Just a, errors = []}
verify Nothing = Validator{value = Nothing, errors = []}

invalid :: ValidatorError -> Validator a
invalid err = Validator Nothing [err]

rg :: String
rg = case g of
    (Validator x es) -> show (x, es)

-- type Checker a = State [ValidatorError] (Maybe a)

validateSymbol :: LToken -> Validator Lexeme
validateSymbol s =
    case s of
        RealToken et -> return $ lexeme et
        _ -> invalid $ TokenError s

-- [MissingTokenError ]

validateFind :: FindStatementNode -> Validator [Declaration]
validateFind (FindStatementNode find names colon domain) = do
    checkSymbols [find, colon]
    names' <- validate $ validateNameList names
    domain' <- validate $ validateDomain domain
    verify $ map <$> (makeFind <$> domain') <*> names'
  where
    makeFind :: Domain () Expression -> Name -> Declaration
    makeFind dom nm = FindOrGiven Find nm dom

type DomainValidator = Validator (Domain () Expression)

validateDomain :: DomainNode -> DomainValidator
validateDomain dm = case dm of
    BoolDomainNode lt -> validateSymbol lt >> return DomainBool
    RangedIntDomainNode l1 rs -> checkSymbols [l1] >> validateRangedInt rs
    RangedEnumNode nn ranges -> validateEnumRange nn ranges
    EnumDomainNode nn -> validateNamedEnumDomain nn
    ShortTupleDomainNode lst -> validateTupleDomain lst
    TupleDomainNode l1 doms -> checkSymbols [l1] >> validateTupleDomain doms
    RecordDomainNode l1 ndom -> checkSymbols [l1] >> validateRecordDomain ndom
    VariantDomainNode l1 ndom -> checkSymbols [l1] >> validateVariantDomain ndom
    MatrixDomainNode l1 l2 l3 idoms l4 dom -> checkSymbols [l1, l2, l3, l4] >> validateMatrixDomain idoms dom
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
        return $ DomainInt TagInt ranges'
    validateRangedInt Nothing = return $ DomainInt TagInt []
    validateEnumRange :: NameNode -> ListNode RangeNode -> DomainValidator
    validateEnumRange name ranges = do
        name' <- validate $ validateIdentifier name
        ranges' <- validateList validateRange ranges
        -- scopecheck (see parser:313)
        verify $ (\n -> DomainEnum (Name n) (Just ranges') Nothing) <$> name'
    validateNamedEnumDomain :: NameNode -> DomainValidator
    validateNamedEnumDomain name = do
        name' <- validateName name
        return $ DomainReference name' Nothing
    validateTupleDomain :: ListNode DomainNode -> DomainValidator
    validateTupleDomain doms = DomainTuple <$> validateList validateDomain doms
    validateRecordDomain :: ListNode NamedDomainNode -> DomainValidator
    validateRecordDomain namedDoms = DomainRecord <$> validateList validateNamedDomain namedDoms
    validateVariantDomain :: ListNode NamedDomainNode -> DomainValidator
    validateVariantDomain namedDoms = DomainRecord <$> validateList validateNamedDomain namedDoms
    validateMatrixDomain :: ListNode DomainNode -> DomainNode -> DomainValidator
    validateMatrixDomain indexes dom = do
        idoms <- validate $ validateList validateDomain indexes
        dom' <- validate $ validateDomain dom
        verify $ foldr DomainMatrix <$> dom' <*> idoms
    validateSetDomain :: ListNode AttributeNode -> DomainNode -> DomainValidator
    validateSetDomain attrs dom = do
        let repr = Just ()
        attrs' <- validate $ validateSetAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainSet <$> repr <*> attrs' <*> dom'

    validateMSetDomain :: ListNode AttributeNode -> DomainNode -> DomainValidator
    validateMSetDomain attrs dom = do
        let repr = Just ()
        attrs' <- validate $ validateMSetAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainMSet <$> repr <*> attrs' <*> dom'
    validateFunctionDomain :: Maybe (ListNode AttributeNode) -> DomainNode -> DomainNode -> DomainValidator
    validateFunctionDomain attrs dom1 dom2 = do
        let repr = Just ()
        attrs' <- case attrs of
            Just a -> validate $ validateFuncAttributes a
            Nothing -> return $ Just def
        dom1' <- validate $ validateDomain dom1
        dom2' <- validate $ validateDomain dom2
        verify $ DomainFunction <$> repr <*> attrs' <*> dom1' <*> dom2'

    -- attrs <- validateAttributes
    validateSequenceDomain :: ListNode AttributeNode -> DomainNode -> DomainValidator
    validateSequenceDomain attrs dom = do
        let repr = Just ()
        attrs' <- validate $ validateSeqAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainSequence <$> repr <*> attrs' <*> dom'
    validateRelationDomain :: ListNode AttributeNode -> ListNode DomainNode -> DomainValidator
    validateRelationDomain attrs doms = do
        let repr = Just ()
        attrs' <- validate $ validateRelationAttributes attrs
        doms' <- validate $ validateList validateDomain doms
        verify $ DomainRelation <$> repr <*> attrs' <*> doms'
    validatePartitionDomain :: ListNode AttributeNode -> DomainNode -> DomainValidator
    validatePartitionDomain attrs dom = do
        let repr = Just ()
        attrs' <- validate $ validatePartitionAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainPartition <$> repr <*> attrs' <*> dom'

todo :: String -> Validator a
todo s = invalid $ NotImplemented s

-- TODO:THIS IS NOT DONE
validateSetAttributes :: ListNode AttributeNode -> Validator (SetAttr Expression)
validateSetAttributes a = do verify $ Nothing

validateMSetAttributes :: ListNode AttributeNode -> Validator (MSetAttr Expression)
validateMSetAttributes a = do verify $ Nothing

validateFuncAttributes :: ListNode AttributeNode -> Validator (FunctionAttr Expression)
validateFuncAttributes a = do verify $ Nothing

validateSeqAttributes :: ListNode AttributeNode -> Validator (SequenceAttr Expression)
validateSeqAttributes a = do verify $ Nothing

validateRelationAttributes :: ListNode AttributeNode -> Validator (RelationAttr Expression)
validateRelationAttributes a = do verify $ Nothing

validatePartitionAttributes :: ListNode AttributeNode -> Validator (PartitionAttr Expression)
validatePartitionAttributes a = do verify $ Nothing

validateNamedDomain :: NamedDomainNode -> Validator (Name, Domain () Expression)
validateNamedDomain (NameDomainNode name l1 domain) = do
    checkSymbols [l1]
    name' <- validate $ validateName name
    domain' <- validate $ validateDomain domain
    verify $ (,) <$> name' <*> domain'

validateRange :: RangeNode -> Validator (Range Expression)
validateRange range = case range of
    SingleRangeNode en -> RangeSingle <$> validateExpression en
    OpenRangeNode dots -> checkSymbols [dots] >> return RangeOpen
    RightUnboundedRangeNode e1 dots -> checkSymbols [dots] >> RangeLowerBounded <$> validateExpression e1
    LeftUnboundedRangeNode dots e1 -> checkSymbols [dots] >> RangeUpperBounded <$> validateExpression e1
    BoundedRangeNode e1 dots e2 -> do
        _ <- checkSymbols [dots]
        e1' <- validate $ validateExpression e1
        e2' <- validate $ validateExpression e2
        verify $ RangeBounded <$> e1' <*> e2'

validateArrowPair :: ArrowPairNode -> Validator (Expression, Expression)
validateArrowPair (ArrowPairNode e1 s e2) = do
    checkSymbols [s]
    e1' <- validate $ validateExpression e1
    e2' <- validate $ validateExpression e2
    verify $ (,) <$> e1' <*> e2'

validateExpression :: ExpressionNode -> Validator Expression
validateExpression expr = case expr of
    Literal ln -> validateLiteral ln
    IdentifierNode nn -> validateIdentifierExpr nn
    QuantificationExpr qen -> validateQuantificationExpression qen
    OperatorExpressionNode oen -> validateOperatorExpression oen
    DomainExpression dex -> validateDomainExpression dex
    ParenExpression (ParenExpressionNode l1 exp l2) -> checkSymbols [l1,l2] >> validateExpression exp
    AbsExpression (ParenExpressionNode l1 exp l2) -> do
        checkSymbols [l1,l2]
        exp' <- validateExpression exp
        return $ mkOp "twoBars"  [exp']
    FunctionalApplicationNode lt ln -> validateFunctionApplication  lt ln
    MissingExpressionNode lt -> invalid $ TokenError lt

translateQnName :: Lexeme -> Text
translateQnName qnName = case qnName of
    L_ForAll -> "and"
    L_Exists -> "or"
    _        -> lexemeText qnName

validateQuantificationExpression :: QuantificationExpressionNode -> Validator Expression
validateQuantificationExpression (QuantificationExpressionNode name pats over m_guard dot expr) =
    do
        checkSymbols [dot]
        name' <- validate $ validateSymbol name
        patterns <- validate $ validateSequence validateAbstractPattern pats
        over' <- validate $ validateQuantificationOver over
        guard' <- validate $ validateQuantificationGuard m_guard
        body <- validate $ validateExpression expr
        let gens = map  <$>  over' <*> patterns
        let qBody =  Comprehension <$> body  <*> ((++) <$> guard' <*> gens)
        verify $ mkOp <$> (translateQnName <$> name') <*> ((:[]) <$> qBody)
    where
        validateQuantificationGuard :: Maybe QuanticationGuard -> Validator [GeneratorOrCondition]
        validateQuantificationGuard Nothing = pure []
        validateQuantificationGuard (Just (QuanticationGuard l1 exp) ) = do
            checkSymbols [l1]
            expr' <- validateExpression exp
            return [Condition expr']
        validateQuantificationOver :: QuantificationOverNode -> Validator (AbstractPattern -> GeneratorOrCondition)
        validateQuantificationOver ( QuantifiedSubsetOfNode lt en ) = do
            checkSymbols [lt]
            exp <- validateExpression en
            return (\pat -> Generator $ GenInExpr pat (Op $ MkOpPowerSet $ OpPowerSet exp))
        validateQuantificationOver ( QuantifiedMemberOfNode lt en ) = do
            checkSymbols [lt]
            exp <- validateExpression en
            return (\pat -> Generator $ GenInExpr pat exp)
        validateQuantificationOver ( QuantifiedDomainNode (OverDomainNode l1 dom) ) = do
            checkSymbols [l1]
            dom' <- validateDomain dom
            return (\pat -> Generator $ GenDomainNoRepr pat dom')



validateAbstractPattern :: AbstractPatternNode -> Validator AbstractPattern
validateAbstractPattern (AbstractIdentifier nn) = Single <$> validateName nn
validateAbstractPattern (AbstractMetaVar lt) = AbstractPatternMetaVar <$>  todo "absMetaVar"
validateAbstractPattern (AbstractPatternMatrix ln) = AbsPatMatrix <$> validateList validateAbstractPattern ln
validateAbstractPattern (AbstractPatternSet ln) = AbsPatSet <$> validateList validateAbstractPattern ln
validateAbstractPattern (AbstractPatternTuple m_lt ln) = do
    maybe (pure ()) (\n ->checkSymbols [n]) m_lt
    AbsPatTuple <$> validateList validateAbstractPattern ln

validateDomainExpression :: DomainExpressionNode -> Validator Expression
validateDomainExpression (DomainExpressionNode  l1 dom l2) = do
    checkSymbols [l1,l2]
    Domain <$> validateDomain dom

validateFunctionApplication :: LToken -> ListNode ExpressionNode -> Validator Expression
validateFunctionApplication name args = do
    name' <- validate $ validateSymbol name
    args' <- validate $ validateList validateExpression args
    verify $ do 
                n <- name' 
                a <- args'
                return $ case (n,a) of
                    (L_image,[y,z]) -> Op $  MkOpImage $ OpImage y z
                    _ -> mkOp (pack . lexemeFace $ n) a

validateIdentifierExpr :: NameNode -> Validator Expression
validateIdentifierExpr name = Reference <$> ( Name <$> validateIdentifier name) <*> pure Nothing

validateOperatorExpression :: OperatorExpressionNode -> Validator Expression

validateOperatorExpression (PrefixOpNode lt expr) = do
    checkSymbols [lt]
    --lookup symbol
    todo "operator expression prefix"
validateOperatorExpression (BinaryOpNode lexp op rexp) = do
    lExpr <- validate $ validateExpression lexp
    rExpr <- validate $ validateExpression rexp
    op' <- validate $ validateSymbol op
    verify $ mkBinOp <$> ( pack . lexemeFace <$> op') <*> lExpr <*> rExpr
validateOperatorExpression (PostfixOpNode expr pon) = do
    expr' <- validate $ validateExpression expr
    postFixOp <- validate $ validatePostfixOp pon
    verify $ postFixOp <*> expr'

validatePostfixOp :: PostfixOpNode -> Validator (Expression -> Expression)
validatePostfixOp (OpFactorial lt) = do
        checkSymbols [lt]
        return (\x -> mkOp "factorial" [x])
validatePostfixOp (ApplicationNode args) = do
        args' <- validateList validateExpression args
        let underscore = Reference "_" Nothing
        let ys = [if underscore == x then Nothing else Just x | x <- args']
        return $ \ x -> Op $ MkOpRelationProj $ OpRelationProj x ys
validatePostfixOp (IndexedNode ln) = do
        ranges <-validateList validateRange ln
        let indices = map interpretRange ranges
        return $ flip (foldl (\m f -> f m)) indices
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

validateLiteral :: LiteralNode -> Validator Expression
validateLiteral ln = case ln of
    IntLiteral lt -> Constant <$> validateIntLiteral lt
    BoolLiteral lt -> Constant <$> validateBoolLiteral lt
    MatrixLiteral mln -> validateMatrixLiteral mln
    TupleLiteralNode lt -> todo "Tuple literal"
    TupleLiteralNodeShort st -> todo "Short tuple literal"
    RecordLiteral lt ln' -> todo "Record literal"
    VariantLiteral lt ln' -> todo "Variant literal"
    SetLiteral ln' -> todo "Set literal"
    MSetLiteral lt ln' -> todo "MSet Literal"
    FunctionLiteral lt ln' -> todo "Function Literal"
    SequenceLiteral lt ln' -> todo "Sequence Literal"
    RelationLiteral lt ln' -> todo "Relation literal"
    PartitionLiteral lt ln' -> todo "Partition literal"

validateMatrixLiteral :: MatrixLiteralNode -> Validator Expression
validateMatrixLiteral (MatrixLiteralNode l1 se m_dom m_comp l2) = do
    checkSymbols [l1,l2]
    todo "matrix literal"



checkSymbols :: [LToken] -> Validator ()
checkSymbols = mapM_ (validate . validateSymbol)

validateShortTuple :: ShortTuple -> Validator [Expression]
validateShortTuple (ShortTuple exs) = validateList validateExpression exs

validateLongTuple :: LongTuple -> Validator [Expression]
validateLongTuple (LongTuple lt exs) = checkSymbols [lt] >> validateList validateExpression exs

validateIntLiteral :: LToken -> Validator Constant
validateIntLiteral t = do
    l <- validateSymbol t
    case l of
        LIntLiteral x -> return $ ConstantInt TagInt x
        _ -> invalid $ IllegalToken t

validateBoolLiteral :: LToken -> Validator Constant
validateBoolLiteral t = do
    l <- validateSymbol t
    case l of
        L_true -> return $ ConstantBool True
        L_false -> return $ ConstantBool False
        _ -> invalid $ IllegalToken t

validateNameList :: Sequence NameNode -> Validator [Name]
validateNameList = validateSequence validateName

validateIdentifier :: NameNode -> Validator Text
validateIdentifier (NameNode iden) = do
    q <- validate $ validateSymbol iden
    verify $ case q of
        Just (LIdentifier x) -> Just x
        _ -> Nothing

validateName :: NameNode -> Validator Name
validateName name = Name <$> validateIdentifier name

validateArray :: (a -> Validator b) -> [a] -> Validator [b]
validateArray f l = catMaybes <$> mapM (validate . f) l

validateList :: (a -> Validator b) -> ListNode a -> Validator [b]
validateList validator (ListNode st seq end) = do
    _ <- validateSymbol st
    _ <- validateSymbol end
    validateSequence validator seq

validateSequence :: (a -> Validator b) -> Sequence a -> Validator [b]
validateSequence f (Seq vals) = validateArray (validateSequenceElem f) vals

validateSequenceElem :: (a -> Validator b) -> SeqElem a -> Validator b
validateSequenceElem f (SeqElem (Just x) i) = validate (validateSymbol x) >> f i
validateSequenceElem f (SeqElem Nothing i) = f i

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
        Left a -> putStrLn "error"
        Right p@(ProgramTree{}) -> print (validateProgramTree p)

-- putStrLn validateFind