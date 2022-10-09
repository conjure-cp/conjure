{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Validator where

import Conjure.Language.AST.ASTParser
import Conjure.Language.AST.Syntax as S
import Conjure.Language.Definition
import Conjure.Language.Lexemes
import Conjure.Prelude
import Control.Applicative
import Conjure.Language.NewLexer (ETok (lexeme, capture, ETok), ETokenStream (ETokenStream), eLex, sourcePos0)
import Conjure.Language.Domain

import Data.Text (unpack, pack)
import Conjure.Language.Type
import Shelly (ls)
import Text.Megaparsec (parseMaybe)

validateModel :: ProgramTree -> Either Doc Model
validateModel model = Left "TODO"


validateProgramTree :: ProgramTree -> Validator [Declaration]
validateProgramTree (ProgramTree (x:xs) l) = validateStatement x
validateProgramTree _ = todo


validateStatement :: StatementNode -> Validator [Declaration]
validateStatement a = case a of
    DeclarationStatement (FindStatement f) -> validateFind f
    _ -> todo
data ValidatorError
    = TypeError String
    | TokenError LToken
    | IllegalToken LToken -- Should not occur in practice and indicates a logical error somewhere
    | NotImplemented
    deriving (Show)

-- --validateFind :: ValidatorState -> FindStatementNode -> (ValidatorState,FindOrGiven)
-- validateFind :: FindStatementNode -> Validator [Declaration]
-- validateFind (FindStatementNode l names b domain) =
--     do
--         validateSymbol l
--         names' <- validateSeq $ validateName names
--         validateSymbol b
--         domain' <- validateDomain domain
--         return $ FindOrGiven FindOrGiven Name (Domain () Expression)

-- validateList :: ListNode a ->  (a -> Validator b) -> Validator [b]
-- validateList xs f = map q xs
--                     where q e= do
--                                 a <- f e
--                                 return e

-- validateSymbol :: LToken -> Validator ()
-- validateSymbol a = do put $ ValidatorState []

-- validateName :: NameNode -> Validator String
-- validateName (NameNode (LIdentifier s)) = s
-- validateName (NameNode (_)) = do
--                                     raise "missing synbol"
--                                     return ""

data Validator a = Validator
    { value :: Maybe a
    , errors :: [ValidatorError]
    }
    deriving (Show)

data Validated a = Valid a | Invalid

instance Functor Validated where
  fmap :: (a -> b) -> Validated a -> Validated b
  fmap fab (Valid a) = Valid (fab a)
  fmap _ Invalid = Invalid

instance Applicative Validated where
  pure :: a -> Validated a
  pure = Valid
  (<*>) :: Validated (a -> b) -> Validated a -> Validated b
  (Valid fab) <*> (Valid a) = Valid (fab a)
  Invalid <*> (Valid _) = Invalid
  _ <*> Invalid = Invalid

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
    (Validator Nothing ves) >>= _
      = Validator {value = Nothing, errors = ves}
    (Validator (Just n) ves) >>= f = let r = f n in
                                        r {errors = ves ++ errors r}


validate ::  Validator a -> Validator (Validated a)
validate n = do
                case n of
                    Validator Nothing ves -> Validator (Just Invalid) ves
                    Validator (Just a) ves -> Validator (Just $ Valid a) ves


getPrefix :: Validator Int
getPrefix = Validator Nothing [TypeError "ERR"]

g :: Validator Foo
g = do
        _ <- validate getPrefix
        a <- validate $ do return 1
        b <- validate $ do return 1:: Validator Int
        c <- validate $ do return 1
        verify $ Foo <$> a  <*> b <*> c

verify :: Validated a -> Validator a
verify (Valid a) = Validator {value = Just a, errors = []}
verify Invalid = Validator {value = Nothing, errors = []}

invalid :: ValidatorError -> Validator a
invalid err = Validator Nothing [err]

-- qq :: Checker Foo
-- qq = do
--         getPrefix
--         q <- get
--         return $ Foo <$> q a b

rg :: String
rg = case g of
    (Validator x es) -> show (x,es)

-- type Checker a = State [ValidatorError] (Maybe a)

validateSymbol :: LToken -> Validator Lexeme
validateSymbol s =
    case s of
        RealToken et -> return $  lexeme et
        _ -> invalid $ TokenError s

                        -- [MissingTokenError ] 


validateFind :: FindStatementNode -> Validator [Declaration]
validateFind (FindStatementNode find names colon domain ) = do
    checkSymbols [find,colon]
    names' <- validate $ validateNameList names
    domain' <- validate $ validateDomain domain
    verify $ map <$> (makeFind <$> domain') <*> names'
    where
        makeFind :: Domain () Expression -> Name -> Declaration
        makeFind dom nm = FindOrGiven  Find  nm  dom





type DomainValidator = Validator (Domain () Expression)

validateDomain :: DomainNode -> DomainValidator
validateDomain dm = case dm of
  BoolDomainNode lt -> validateSymbol lt >> return DomainBool
  RangedIntDomainNode l1 rs -> checkSymbols [l1] >> validateRangedInt rs
  RangedEnumNode nn ranges -> validateEnumRange nn ranges
  EnumDomainNode nn -> validateNamedEnumDomain nn
  TupleDomainNode l1 doms -> checkSymbols [l1] >> validateTupleDomain doms
  RecordDomainNode l1 ndom -> checkSymbols [l1] >> validateRecordDomain ndom
  VariantDomainNode l1 ndom -> checkSymbols [l1] >> validateVariantDomain ndom
  MatrixDomainNode l1 l2 l3 idoms l4 dom -> checkSymbols [l1,l2,l3,l4] >> validateMatrixDomain idoms dom
  SetDomainNode l1 attrs l2 dom -> checkSymbols [l1,l2] >> validateSetDomain attrs dom
  MSetDomainNode l1 attrs l2 dom -> checkSymbols [l1,l2] >> validateMSetDomain attrs dom
  FunctionDomainNode l1 attrs dom1 l2 dom2 -> checkSymbols [l1,l2] >> validateFunctionDomain attrs dom1 dom2
  SequenceDomainNode l1 attrs l2 dom -> checkSymbols [l1,l2] >> validateSequenceDomain attrs dom
  RelationDomainNode l1 attrs l2 doms -> checkSymbols [l1,l2] >> validateRelationDomain attrs doms
  PartitionDomainNode l1 attrs l2 dom -> checkSymbols [l1,l2] >> validatePartitionDomain attrs dom
  MissingDomainNode lt -> invalid $ TokenError lt
  where
    validateRangedInt :: ListNode RangeNode ->DomainValidator
    validateRangedInt ranges = do
        ranges' <- validateList validateRange ranges
        return $ DomainInt TagInt ranges'
    validateEnumRange :: NameNode -> ListNode RangeNode -> DomainValidator
    validateEnumRange name ranges = do
        name' <- validate $ validateIdentifier name
        ranges' <- validateList validateRange ranges
        --scopecheck (see parser:313)
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
    validateMatrixDomain ::ListNode DomainNode -> DomainNode -> DomainValidator
    validateMatrixDomain indexes dom = do
        idoms <- validate $ validateList validateDomain indexes
        dom' <- validate $ validateDomain dom
        verify $ foldr DomainMatrix <$> dom' <*> idoms
    validateSetDomain :: ListNode AttributeNode-> DomainNode -> DomainValidator
    validateSetDomain attrs dom = do
        let repr = Valid ()
        attrs' <- validate $ validateSetAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainSet <$> repr <*> attrs' <*> dom'

    validateMSetDomain :: ListNode AttributeNode-> DomainNode -> DomainValidator
    validateMSetDomain attrs dom =  do
        let repr = Valid ()
        attrs' <- validate $ validateMSetAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainMSet <$> repr <*> attrs' <*> dom'
    validateFunctionDomain :: ListNode AttributeNode-> DomainNode -> DomainNode -> DomainValidator
    validateFunctionDomain attrs dom1 dom2 = do
        let repr = Valid ()
        attrs' <- validate $ validateFuncAttributes attrs
        dom1' <- validate $ validateDomain dom1
        dom2' <- validate $ validateDomain dom2
        verify $ DomainFunction <$> repr <*> attrs' <*> dom1' <*> dom2'
        -- attrs <- validateAttributes
    validateSequenceDomain :: ListNode AttributeNode-> DomainNode -> DomainValidator
    validateSequenceDomain attrs dom = do
        let repr = Valid ()
        attrs' <- validate $ validateSeqAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainSequence <$> repr <*> attrs' <*> dom'
    validateRelationDomain :: ListNode AttributeNode-> ListNode DomainNode -> DomainValidator
    validateRelationDomain attrs doms = do
        let repr = Valid ()
        attrs' <- validate $ validateRelationAttributes attrs
        doms' <- validate $ validateList validateDomain doms
        verify $ DomainRelation <$> repr <*> attrs' <*> doms'
    validatePartitionDomain :: ListNode AttributeNode-> DomainNode -> DomainValidator
    validatePartitionDomain attrs dom = do
        let repr = Valid ()
        attrs' <- validate $ validatePartitionAttributes attrs
        dom' <- validate $ validateDomain dom
        verify $ DomainPartition <$> repr <*> attrs' <*> dom'

todo:: Validator a
todo = invalid NotImplemented



--TODO:THIS IS NOT DONE
validateSetAttributes :: ListNode AttributeNode -> Validator (SetAttr Expression)
validateSetAttributes a = do verify $ Invalid

validateMSetAttributes :: ListNode AttributeNode -> Validator (MSetAttr Expression)
validateMSetAttributes a = do verify $ Invalid

validateFuncAttributes :: ListNode AttributeNode -> Validator (FunctionAttr Expression)
validateFuncAttributes a = do verify $ Invalid

validateSeqAttributes :: ListNode AttributeNode -> Validator (SequenceAttr Expression)
validateSeqAttributes a = do verify $ Invalid

validateRelationAttributes :: ListNode AttributeNode -> Validator (RelationAttr Expression)
validateRelationAttributes a = do verify $ Invalid

validatePartitionAttributes :: ListNode AttributeNode -> Validator (PartitionAttr Expression)
validatePartitionAttributes a = do verify $ Invalid

validateNamedDomain :: NamedDomainNode -> Validator (Name,Domain () Expression)
validateNamedDomain (NameDomainNode name l1 domain) = do
    checkSymbols [l1]
    name' <- validate $ validateName name
    domain' <- validate $ validateDomain domain
    verify $ (,) <$> name' <*> domain'

validateRange :: RangeNode -> Validator (Range Expression)
validateRange range = case range of
  SingleRangeNode en -> RangeSingle <$> validateExpression en
  OpenRangeNode (DoubleDotNode a b) -> checkSymbols [a,b] >> return RangeOpen
  RightUnboundedRangeNode e1 (DoubleDotNode a b) -> checkSymbols [a,b] >> RangeLowerBounded <$> validateExpression e1
  LeftUnboundedRangeNode (DoubleDotNode a b) e1 -> checkSymbols [a,b] >> RangeUpperBounded <$> validateExpression e1
  BoundedRangeNode e1 (DoubleDotNode a b) e2 -> do
    _ <- checkSymbols [a,b]
    e1' <- validate $ validateExpression e1
    e2' <- validate $ validateExpression e2
    verify $ RangeBounded <$> e1' <*> e2'


validateArrowPair :: ArrowPairNode -> Validator (Expression,Expression)
validateArrowPair (ArrowPairNode e1 s e2) = do
    checkSymbols [s]
    e1' <- validate $ validateExpression e1
    e2' <- validate $ validateExpression e2
    verify $ (,) <$> e1' <*> e2'


validateExpression :: ExpressionNode -> Validator Expression
validateExpression expr = case expr of
  Literal ln -> validateLiteral ln
  IdentifierNode nn -> todo
  QuantificationExpr qen -> todo
  ComprehensionExpr cen -> todo
  OperatorExpressionNode oen -> todo
  ParenExpression pen -> todo
  AbsExpression pen -> todo
  FunctionalApplicationNode lt ln -> todo
  MissingExpressionNode lt -> invalid $ TokenError lt

validateLiteral :: LiteralNode -> Validator Expression
validateLiteral ln = case ln of
  IntLiteral lt ->  Constant <$> validateIntLiteral lt
  BoolLiteral lt -> Constant <$> validateBoolLiteral lt
  MatrixLiteral mln -> todo
  TupleLiteralNode lt -> todo
  TupleLiteralNodeShort st -> todo
  RecordLiteral lt ln' -> todo
  VariantLiteral lt ln' -> todo
  SetLiteral ln' -> todo
  MSetLiteral lt ln' -> todo
  FunctionLiteral lt ln' -> todo
  SequenceLiteral lt ln' -> todo
  RelationLiteral lt ln' -> todo
  PartitionLiteral lt ln' -> todo

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
        Valid (LIdentifier x) ->Valid  x
        _ -> Invalid

validateName :: NameNode -> Validator Name
validateName name = Name <$> validateIdentifier name

validateList :: (a -> Validator b) -> ListNode a ->  Validator [b]
validateList validator (ListNode st seq end)  = do
    _ <- validateSymbol st
    _ <- validateSymbol end
    validateSequence validator seq

validateSequence :: (a -> Validator b) ->  Sequence a -> Validator [b]
validateSequence f (Seq vals) = mapM (validateSequenceElem f) vals

validateSequenceElem :: (a -> Validator b) -> SeqElem a -> Validator b
validateSequenceElem f (SeqElem i (Just x)) = validate (validateSymbol x) >> f i
validateSequenceElem f (SeqElem i Nothing) = f i


val :: String -> IO ()
val s = do
    let str = s
    let other = [ETok (0, 0, 0, sourcePos0) [] L_EOF ""]
    let txt  = pack str
    let lexed = parseMaybe eLex  txt
    let stream = ETokenStream txt $ fromMaybe other lexed
    -- parseTest parseProgram stream
    let progStruct = parseMaybe parseProgram stream
    case progStruct of
        Nothing -> putStrLn "error"
        Just p@(ProgramTree{}) -> print (validateProgramTree p)
    -- putStrLn validateFind