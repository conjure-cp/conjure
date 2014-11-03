{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Ops where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Language.AdHoc
import Conjure.Language.Lexer ( Lexeme(..), textToLexeme, lexemeFace )

-- aeson
import qualified Data.Aeson as JSON


class OperatorContainer x where
    injectOp :: Ops x -> x
    projectOp :: MonadFail m => x -> m (Ops x)

instance OperatorContainer Constant where
    injectOp x = bug ("OperatorContainer{Constant} -- injectOp --" <+> pretty x)
    projectOp x = bug ("OperatorContainer{Constant} -- projectOp --" <+> pretty x)


data Ops x
    = MkOpPlus            (OpPlus x)
    | MkOpMinus           (OpMinus x)
    | MkOpTimes           (OpTimes x)
    | MkOpDiv             (OpDiv x)
    | MkOpMod             (OpMod x)
    | MkOpPow             (OpPow x)
    | MkOpAbs             (OpAbs x)
    | MkOpNegate          (OpNegate x)

    | MkOpEq              (OpEq x)
    | MkOpNeq             (OpNeq x)
    | MkOpLt              (OpLt x)
    | MkOpLeq             (OpLeq x)
    | MkOpGt              (OpGt x)
    | MkOpGeq             (OpGeq x)

    | MkOpAnd             (OpAnd x)
    | MkOpOr              (OpOr x)
    | MkOpImply           (OpImply x)
    | MkOpNot             (OpNot x)

    | MkOpIndexing        (OpIndexing x)
    | MkOpSlicing         (OpSlicing x)
    | MkOpFlatten         (OpFlatten x)

    | MkOpFilter          (OpFilter x)
    | MkOpMapOverDomain   (OpMapOverDomain x)
    | MkOpMapInExpr       (OpMapInExpr x)
    | MkOpMapSubsetExpr   (OpMapSubsetExpr x)
    | MkOpMapSubsetEqExpr (OpMapSubsetEqExpr x)

    | MkOpTrue            (OpTrue x)
    | MkOpToInt           (OpToInt x)
    | MkOpDontCare        (OpDontCare x)

    | MkOpIn              (OpIn x)
    | MkOpSubset          (OpSubset x)
    | MkOpSubsetEq        (OpSubsetEq x)
    | MkOpSupset          (OpSupset x)
    | MkOpSupsetEq        (OpSupsetEq x)
    | MkOpIntersect       (OpIntersect x)
    | MkOpUnion           (OpUnion x)

    | MkOpFunctionImage   (OpFunctionImage x)
    | MkOpDefined         (OpDefined x)
    | MkOpRange           (OpRange x)

    | MkOpAllDiff         (OpAllDiff x)

    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (Ops x)
instance Hashable  x => Hashable  (Ops x)
instance ToJSON    x => ToJSON    (Ops x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (Ops x) where parseJSON = JSON.genericParseJSON jsonOptions

instance (TypeOf x, Show x, Pretty x, IntContainer x) => TypeOf (Ops x) where
    typeOf (MkOpPlus                x) = typeOf x
    typeOf (MkOpMinus               x) = typeOf x
    typeOf (MkOpTimes               x) = typeOf x
    typeOf (MkOpDiv                 x) = typeOf x
    typeOf (MkOpMod                 x) = typeOf x
    typeOf (MkOpPow                 x) = typeOf x
    typeOf (MkOpAbs                 x) = typeOf x
    typeOf (MkOpNegate              x) = typeOf x
    typeOf (MkOpEq                  x) = typeOf x
    typeOf (MkOpNeq                 x) = typeOf x
    typeOf (MkOpLt                  x) = typeOf x
    typeOf (MkOpLeq                 x) = typeOf x
    typeOf (MkOpGt                  x) = typeOf x
    typeOf (MkOpGeq                 x) = typeOf x
    typeOf (MkOpAnd                 x) = typeOf x
    typeOf (MkOpOr                  x) = typeOf x
    typeOf (MkOpImply               x) = typeOf x
    typeOf (MkOpNot                 x) = typeOf x
    typeOf (MkOpIndexing            x) = typeOf x
    typeOf (MkOpSlicing             x) = typeOf x
    typeOf (MkOpFlatten             x) = typeOf x
    typeOf (MkOpFilter              x) = typeOf x
    typeOf (MkOpMapOverDomain       x) = typeOf x
    typeOf (MkOpMapInExpr           x) = typeOf x
    typeOf (MkOpMapSubsetExpr       x) = typeOf x
    typeOf (MkOpMapSubsetEqExpr     x) = typeOf x
    typeOf (MkOpTrue                x) = typeOf x
    typeOf (MkOpToInt               x) = typeOf x
    typeOf (MkOpDontCare            x) = typeOf x
    typeOf (MkOpIn                  x) = typeOf x
    typeOf (MkOpSubset              x) = typeOf x
    typeOf (MkOpSubsetEq            x) = typeOf x
    typeOf (MkOpSupset              x) = typeOf x
    typeOf (MkOpSupsetEq            x) = typeOf x
    typeOf (MkOpIntersect           x) = typeOf x
    typeOf (MkOpUnion               x) = typeOf x
    typeOf (MkOpFunctionImage       x) = typeOf x
    typeOf (MkOpDefined             x) = typeOf x
    typeOf (MkOpRange               x) = typeOf x
    typeOf (MkOpAllDiff             x) = typeOf x


class BinaryOperator op where
    opLexeme :: proxy op -> Lexeme

-- | just the operator not the arguments
opPretty :: BinaryOperator op => proxy op -> Doc
opPretty = lexemeFace . opLexeme

opFixityPrec :: BinaryOperator op => proxy op -> (Fixity, Int)
opFixityPrec op =
    case [ (f,p) | (l,f,p) <- operators, l == opLexeme op ] of
        [x] -> x
        _ -> bug "opFixityPrec"

instance Pretty x => Pretty (Ops x) where
    prettyPrec prec (MkOpPlus  op@(OpPlus  [a,b])) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpPlus     (OpPlus   xs  )) = "sum" <> prettyList prParens "," xs
    prettyPrec prec (MkOpMinus op@(OpMinus  a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpTimes op@(OpTimes [a,b])) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpTimes    (OpTimes  xs  )) = "product" <> prettyList prParens "," xs
    prettyPrec prec (MkOpDiv   op@(OpDiv    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpMod   op@(OpMod    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpPow   op@(OpPow    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpAbs      (OpAbs    a   )) = "|" <> pretty a <> "|"
    prettyPrec _    (MkOpNegate   (OpNegate a   )) = "-" <> prettyPrec 10000 a
    prettyPrec prec (MkOpEq    op@(OpEq     a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpNeq   op@(OpNeq    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpLt    op@(OpLt     a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpLeq   op@(OpLeq    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpGt    op@(OpGt     a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpGeq   op@(OpGeq    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpAnd   op@(OpAnd   [a,b])) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpAnd      (OpAnd   xs   )) = "and" <> prettyList prParens "," xs
    prettyPrec prec (MkOpOr    op@(OpOr    [a,b])) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpOr       (OpOr    xs   )) = "or"  <> prettyList prParens "," xs
    prettyPrec prec (MkOpImply op@(OpImply  a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpNot      (OpNot    a   )) = "!" <> prettyPrec 10000 a
    prettyPrec _ (MkOpIndexing (OpIndexing  a b )) = pretty a <> prBrackets (pretty b)
    prettyPrec _ (MkOpSlicing  (OpSlicing m a b )) = pretty m <> prBrackets (pretty a <> ".." <> pretty b)
    prettyPrec _ (MkOpFlatten  (OpFlatten m     )) = "flatten" <> prParens (pretty m)
    prettyPrec _ (MkOpFilter          (OpFilter          a b)) = "filter"            <> prettyList prParens "," [a,b]
    prettyPrec _ (MkOpMapOverDomain   (OpMapOverDomain   a b)) = "map_domain"        <> prettyList prParens "," [a,b]
    prettyPrec _ (MkOpMapInExpr       (OpMapInExpr       a b)) = "map_in_expr"       <> prettyList prParens "," [a,b]
    prettyPrec _ (MkOpMapSubsetExpr   (OpMapSubsetExpr   a b)) = "map_subset_expr"   <> prettyList prParens "," [a,b]
    prettyPrec _ (MkOpMapSubsetEqExpr (OpMapSubsetEqExpr a b)) = "map_subsetEq_expr" <> prettyList prParens "," [a,b]
    prettyPrec _ (MkOpTrue     (OpTrue     a)) = "true"     <> prParens (pretty a)
    prettyPrec _ (MkOpToInt    (OpToInt    a)) = "toInt"    <> prParens (pretty a)
    prettyPrec _ (MkOpDontCare (OpDontCare a)) = "dontCare" <> prParens (pretty a)
    prettyPrec prec (MkOpIn        op@(OpIn        a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpSubset    op@(OpSubset    a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpSubsetEq  op@(OpSubsetEq  a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpSupset    op@(OpSupset    a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpSupsetEq  op@(OpSupsetEq  a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpIntersect op@(OpIntersect a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpUnion     op@(OpUnion     a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec _ (MkOpFunctionImage (OpFunctionImage a b)) = "image" <> prettyList prParens "," (a:b)
    prettyPrec _ (MkOpDefined  (OpDefined  a)) = "defined"  <> prParens (pretty a)
    prettyPrec _ (MkOpRange    (OpRange    a)) = "range"    <> prParens (pretty a)
    prettyPrec _ (MkOpAllDiff  (OpAllDiff  a)) = "allDiff"  <> prParens (pretty a)


prettyPrecBinOp :: (BinaryOperator op, Pretty x) => Int -> proxy op -> x -> x -> Doc
prettyPrecBinOp envPrec op a b =
    let
        (fixity, prec) = opFixityPrec op
    in
        case fixity of
            FLeft  -> parensIf (envPrec > prec) $ fsep [ prettyPrec  prec    a
                                                       , opPretty op
                                                       , prettyPrec (prec+1) b
                                                       ]
            FNone  -> parensIf (envPrec > prec) $ fsep [ prettyPrec (prec+1) a
                                                       , opPretty op
                                                       , prettyPrec (prec+1) b
                                                       ]
            FRight -> parensIf (envPrec > prec) $ fsep [ prettyPrec  prec    a
                                                       , opPretty op
                                                       , prettyPrec (prec+1) b
                                                       ]


data OpPlus x = OpPlus [x]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpPlus x)
instance Hashable  x => Hashable  (OpPlus x)
instance ToJSON    x => ToJSON    (OpPlus x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPlus x) where parseJSON = JSON.genericParseJSON jsonOptions
opPlus :: OperatorContainer x => x -> x -> x
opPlus x y = injectOp (MkOpPlus (OpPlus [x,y]))
instance BinaryOperator (OpPlus x) where
    opLexeme _ = L_Plus
instance TypeOf x => TypeOf (OpPlus x) where
    typeOf (OpPlus [a,b]) = intToIntToInt a b
    typeOf (OpPlus xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeInt:tys)
            then return TypeInt
            else bug "Type error in OpPlus"


data OpMinus x = OpMinus x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpMinus x)
instance Hashable  x => Hashable  (OpMinus x)
instance ToJSON    x => ToJSON    (OpMinus x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMinus x) where parseJSON = JSON.genericParseJSON jsonOptions
opMinus :: OperatorContainer x => x -> x -> x
opMinus x y = injectOp (MkOpMinus (OpMinus x y))
instance BinaryOperator (OpMinus x) where
    opLexeme _ = L_Minus
instance TypeOf x => TypeOf (OpMinus x) where
    typeOf (OpMinus a b) = intToIntToInt a b


data OpTimes x = OpTimes [x]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpTimes x)
instance Hashable  x => Hashable  (OpTimes x)
instance ToJSON    x => ToJSON    (OpTimes x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTimes x) where parseJSON = JSON.genericParseJSON jsonOptions
opTimes :: OperatorContainer x => x -> x -> x
opTimes x y = injectOp (MkOpTimes (OpTimes [x,y]))
instance BinaryOperator (OpTimes x) where
    opLexeme _ = L_Times
instance TypeOf x => TypeOf (OpTimes x) where
    typeOf (OpTimes [a,b]) = intToIntToInt a b
    typeOf (OpTimes xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeInt:tys)
            then return TypeInt
            else bug "Type error in OpTimes"


data OpDiv x = OpDiv x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpDiv x)
instance Hashable  x => Hashable  (OpDiv x)
instance ToJSON    x => ToJSON    (OpDiv x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDiv x) where parseJSON = JSON.genericParseJSON jsonOptions
opDiv :: OperatorContainer x => x -> x -> x
opDiv x y = injectOp (MkOpDiv (OpDiv x y))
instance BinaryOperator (OpDiv x) where
    opLexeme _ = L_Div
instance TypeOf x => TypeOf (OpDiv x) where
    typeOf (OpDiv a b) = intToIntToInt a b


data OpMod x = OpMod x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpMod x)
instance Hashable  x => Hashable  (OpMod x)
instance ToJSON    x => ToJSON    (OpMod x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMod x) where parseJSON = JSON.genericParseJSON jsonOptions
opMod :: OperatorContainer x => x -> x -> x
opMod x y = injectOp (MkOpMod (OpMod x y))
instance BinaryOperator (OpMod x) where
    opLexeme _ = L_Mod
instance TypeOf x => TypeOf (OpMod x) where
    typeOf (OpMod a b) = intToIntToInt a b


data OpPow x = OpPow x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpPow x)
instance Hashable  x => Hashable  (OpPow x)
instance ToJSON    x => ToJSON    (OpPow x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPow x) where parseJSON = JSON.genericParseJSON jsonOptions
opPow :: OperatorContainer x => x -> x -> x
opPow x y = injectOp (MkOpPow (OpPow x y))
instance BinaryOperator (OpPow x) where
    opLexeme _ = L_Pow
instance TypeOf x => TypeOf (OpPow x) where
    typeOf (OpPow a b) = intToIntToInt a b


data OpAbs x = OpAbs x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpAbs x)
instance Hashable  x => Hashable  (OpAbs x)
instance ToJSON    x => ToJSON    (OpAbs x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAbs x) where parseJSON = JSON.genericParseJSON jsonOptions
opAbs :: OperatorContainer x => x -> x
opAbs x = injectOp (MkOpAbs (OpAbs x))
instance TypeOf x => TypeOf (OpAbs x) where
    typeOf (OpAbs a) = intToInt a


data OpNegate x = OpNegate x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpNegate x)
instance Hashable  x => Hashable  (OpNegate x)
instance ToJSON    x => ToJSON    (OpNegate x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNegate x) where parseJSON = JSON.genericParseJSON jsonOptions
opNegate :: OperatorContainer x => x -> x
opNegate x = injectOp (MkOpNegate (OpNegate x))
instance TypeOf x => TypeOf (OpNegate x) where
    typeOf (OpNegate a) = do TypeInt <- typeOf a ; return TypeInt


data OpEq x = OpEq x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpEq x)
instance Hashable  x => Hashable  (OpEq x)
instance ToJSON    x => ToJSON    (OpEq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpEq x) where parseJSON = JSON.genericParseJSON jsonOptions
opEq :: OperatorContainer x => x -> x -> x
opEq x y = injectOp (MkOpEq (OpEq x y))
instance BinaryOperator (OpEq x) where
    opLexeme _ = L_Eq
instance TypeOf x => TypeOf (OpEq x) where
    typeOf (OpEq a b) = sameToSameToBool a b


data OpNeq x = OpNeq x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpNeq x)
instance Hashable  x => Hashable  (OpNeq x)
instance ToJSON    x => ToJSON    (OpNeq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNeq x) where parseJSON = JSON.genericParseJSON jsonOptions
opNeq :: OperatorContainer x => x -> x -> x
opNeq x y = injectOp (MkOpNeq (OpNeq x y))
instance BinaryOperator (OpNeq x) where
    opLexeme _ = L_Neq
instance TypeOf x => TypeOf (OpNeq x) where
    typeOf (OpNeq a b) = sameToSameToBool a b


data OpLt x = OpLt x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpLt x)
instance Hashable  x => Hashable  (OpLt x)
instance ToJSON    x => ToJSON    (OpLt x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLt x) where parseJSON = JSON.genericParseJSON jsonOptions
opLt :: OperatorContainer x => x -> x -> x
opLt x y = injectOp (MkOpLt (OpLt x y))
instance BinaryOperator (OpLt x) where
    opLexeme _ = L_Lt
instance TypeOf x => TypeOf (OpLt x) where
    typeOf (OpLt a b) = sameToSameToBool a b


data OpLeq x = OpLeq x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpLeq x)
instance Hashable  x => Hashable  (OpLeq x)
instance ToJSON    x => ToJSON    (OpLeq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLeq x) where parseJSON = JSON.genericParseJSON jsonOptions
opLeq :: OperatorContainer x => x -> x -> x
opLeq x y = injectOp (MkOpLeq (OpLeq x y))
instance BinaryOperator (OpLeq x) where
    opLexeme _ = L_Leq
instance TypeOf x => TypeOf (OpLeq x) where
    typeOf (OpLeq a b) = sameToSameToBool a b


data OpGt x = OpGt x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpGt x)
instance Hashable  x => Hashable  (OpGt x)
instance ToJSON    x => ToJSON    (OpGt x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGt x) where parseJSON = JSON.genericParseJSON jsonOptions
opGt :: OperatorContainer x => x -> x -> x
opGt x y = injectOp (MkOpGt (OpGt x y))
instance BinaryOperator (OpGt x) where
    opLexeme _ = L_Gt
instance TypeOf x => TypeOf (OpGt x) where
    typeOf (OpGt a b) = sameToSameToBool a b


data OpGeq x = OpGeq x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpGeq x)
instance Hashable  x => Hashable  (OpGeq x)
instance ToJSON    x => ToJSON    (OpGeq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGeq x) where parseJSON = JSON.genericParseJSON jsonOptions
opGeq :: OperatorContainer x => x -> x -> x
opGeq x y = injectOp (MkOpGeq (OpGeq x y))
instance BinaryOperator (OpGeq x) where
    opLexeme _ = L_Geq
instance TypeOf x => TypeOf (OpGeq x) where
    typeOf (OpGeq a b) = sameToSameToBool a b


data OpAnd x = OpAnd [x]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpAnd x)
instance Hashable  x => Hashable  (OpAnd x)
instance ToJSON    x => ToJSON    (OpAnd x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAnd x) where parseJSON = JSON.genericParseJSON jsonOptions
opAnd :: OperatorContainer x => x -> x -> x
opAnd x y = injectOp (MkOpAnd (OpAnd [x,y]))
instance BinaryOperator (OpAnd x) where
    opLexeme _ = L_And
instance TypeOf x => TypeOf (OpAnd x) where
    typeOf (OpAnd [a,b]) = boolToBoolToBool a b
    typeOf (OpAnd xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeBool:tys)
            then return TypeBool
            else bug "Type error in OpAnd"


data OpOr x = OpOr [x]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpOr x)
instance Hashable  x => Hashable  (OpOr x)
instance ToJSON    x => ToJSON    (OpOr x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpOr x) where parseJSON = JSON.genericParseJSON jsonOptions
opOr :: OperatorContainer x => x -> x -> x
opOr x y = injectOp (MkOpOr (OpOr [x,y]))
instance BinaryOperator (OpOr x) where
    opLexeme _ = L_Or
instance TypeOf x => TypeOf (OpOr x) where
    typeOf (OpOr [a,b]) = boolToBoolToBool a b
    typeOf (OpOr xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeBool:tys)
            then return TypeBool
            else bug "Type error in OpOr"


data OpImply x = OpImply x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpImply x)
instance Hashable  x => Hashable  (OpImply x)
instance ToJSON    x => ToJSON    (OpImply x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpImply x) where parseJSON = JSON.genericParseJSON jsonOptions
opImply :: OperatorContainer x => x -> x -> x
opImply x y = injectOp (MkOpImply (OpImply x y))
instance BinaryOperator (OpImply x) where
    opLexeme _ = L_Imply
instance TypeOf x => TypeOf (OpImply x) where
    typeOf (OpImply a b) = boolToBoolToBool a b


data OpNot x = OpNot x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpNot x)
instance Hashable  x => Hashable  (OpNot x)
instance ToJSON    x => ToJSON    (OpNot x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNot x) where parseJSON = JSON.genericParseJSON jsonOptions
opNot :: OperatorContainer x => x -> x
opNot x = injectOp (MkOpNot (OpNot x))
instance TypeOf x => TypeOf (OpNot x) where
    typeOf (OpNot a) = do TypeBool <- typeOf a ; return TypeBool


data OpIndexing x = OpIndexing x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpIndexing x)
instance Hashable  x => Hashable  (OpIndexing x)
instance ToJSON    x => ToJSON    (OpIndexing x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIndexing x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Show x, Pretty x, IntContainer x) => TypeOf (OpIndexing x) where
    typeOf (OpIndexing m i) = do
        tyM <- typeOf m
        case tyM of
            TypeMatrix _ inn -> return inn
            TypeTuple inns   -> do
                TypeInt{} <- typeOf i
                iInt <- intOut i
                return (at inns (iInt-1))
            _ -> bug ("Indexing something other than a matrix or a tuple:" <++> vcat [pretty m, pretty tyM])


data OpSlicing x = OpSlicing x (Maybe x) (Maybe x)
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpSlicing x)
instance Hashable  x => Hashable  (OpSlicing x)
instance ToJSON    x => ToJSON    (OpSlicing x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSlicing x) where parseJSON = JSON.genericParseJSON jsonOptions
opSlicing :: OperatorContainer x => x -> Maybe x -> Maybe x -> x
opSlicing m a b = injectOp (MkOpSlicing (OpSlicing m a b))
instance TypeOf x => TypeOf (OpSlicing x) where
    typeOf (OpSlicing m _ _) = do
        t@TypeMatrix{} <- typeOf m
        return t


data OpFlatten x = OpFlatten x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpFlatten x)
instance Hashable  x => Hashable  (OpFlatten x)
instance ToJSON    x => ToJSON    (OpFlatten x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFlatten x) where parseJSON = JSON.genericParseJSON jsonOptions
opFlatten :: OperatorContainer x => x -> x
opFlatten m = injectOp (MkOpFlatten (OpFlatten m))
instance TypeOf x => TypeOf (OpFlatten x) where
    typeOf (OpFlatten m) = do
        let flattenType (TypeMatrix _ inner) = flattenType inner
            flattenType ty = ty
        TypeMatrix index n <- typeOf m
        return (TypeMatrix index (flattenType n))


data OpFilter x = OpFilter x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpFilter x)
instance Hashable  x => Hashable  (OpFilter x)
instance ToJSON    x => ToJSON    (OpFilter x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFilter x) where parseJSON = JSON.genericParseJSON jsonOptions
opFilter :: OperatorContainer x => x -> x -> x
opFilter x y = injectOp (MkOpFilter (OpFilter x y))
instance TypeOf x => TypeOf (OpFilter x) where
    typeOf (OpFilter _ _) = return TypeAny


data OpMapOverDomain x = OpMapOverDomain x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpMapOverDomain x)
instance Hashable  x => Hashable  (OpMapOverDomain x)
instance ToJSON    x => ToJSON    (OpMapOverDomain x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMapOverDomain x) where parseJSON = JSON.genericParseJSON jsonOptions
opMapOverDomain :: OperatorContainer x => x -> x -> x
opMapOverDomain x y = injectOp (MkOpMapOverDomain (OpMapOverDomain x y))
instance TypeOf x => TypeOf (OpMapOverDomain x) where
    typeOf (OpMapOverDomain _ _) = return TypeAny


data OpMapInExpr x = OpMapInExpr x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpMapInExpr x)
instance Hashable  x => Hashable  (OpMapInExpr x)
instance ToJSON    x => ToJSON    (OpMapInExpr x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMapInExpr x) where parseJSON = JSON.genericParseJSON jsonOptions
opMapInExpr :: OperatorContainer x => x -> x -> x
opMapInExpr x y = injectOp (MkOpMapInExpr (OpMapInExpr x y))
instance TypeOf x => TypeOf (OpMapInExpr x) where
    typeOf (OpMapInExpr _ _) = return TypeAny


data OpMapSubsetExpr x = OpMapSubsetExpr x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpMapSubsetExpr x)
instance Hashable  x => Hashable  (OpMapSubsetExpr x)
instance ToJSON    x => ToJSON    (OpMapSubsetExpr x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMapSubsetExpr x) where parseJSON = JSON.genericParseJSON jsonOptions
opMapSubsetExpr :: OperatorContainer x => x -> x -> x
opMapSubsetExpr x y = injectOp (MkOpMapSubsetExpr (OpMapSubsetExpr x y))
instance TypeOf x => TypeOf (OpMapSubsetExpr x) where
    typeOf (OpMapSubsetExpr _ _) = return TypeAny


data OpMapSubsetEqExpr x = OpMapSubsetEqExpr x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpMapSubsetEqExpr x)
instance Hashable  x => Hashable  (OpMapSubsetEqExpr x)
instance ToJSON    x => ToJSON    (OpMapSubsetEqExpr x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMapSubsetEqExpr x) where parseJSON = JSON.genericParseJSON jsonOptions
opMapSubsetEqExpr :: OperatorContainer x => x -> x -> x
opMapSubsetEqExpr x y = injectOp (MkOpMapSubsetEqExpr (OpMapSubsetEqExpr x y))
instance TypeOf x => TypeOf (OpMapSubsetEqExpr x) where
    typeOf (OpMapSubsetEqExpr _ _) = return TypeAny


data OpTrue x = OpTrue x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpTrue x)
instance Hashable  x => Hashable  (OpTrue x)
instance ToJSON    x => ToJSON    (OpTrue x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTrue x) where parseJSON = JSON.genericParseJSON jsonOptions
opTrue :: OperatorContainer x => x -> x
opTrue x = injectOp (MkOpTrue (OpTrue x))
instance TypeOf x => TypeOf (OpTrue x) where
    typeOf (OpTrue _) = return TypeBool


data OpToInt x = OpToInt x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpToInt x)
instance Hashable  x => Hashable  (OpToInt x)
instance ToJSON    x => ToJSON    (OpToInt x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToInt x) where parseJSON = JSON.genericParseJSON jsonOptions
opToInt :: OperatorContainer x => x -> x
opToInt x = injectOp (MkOpToInt (OpToInt x))
instance TypeOf x => TypeOf (OpToInt x) where
    typeOf (OpToInt x) = do
        TypeBool{} <- typeOf x
        return TypeInt


data OpDontCare x = OpDontCare x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpDontCare x)
instance Hashable  x => Hashable  (OpDontCare x)
instance ToJSON    x => ToJSON    (OpDontCare x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDontCare x) where parseJSON = JSON.genericParseJSON jsonOptions
opDontCare :: OperatorContainer x => x -> x
opDontCare x = injectOp (MkOpDontCare (OpDontCare x))
instance TypeOf x => TypeOf (OpDontCare x) where
    typeOf (OpDontCare _) = return TypeBool


data OpIn x = OpIn x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpIn x)
instance Hashable  x => Hashable  (OpIn x)
instance ToJSON    x => ToJSON    (OpIn x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIn x) where parseJSON = JSON.genericParseJSON jsonOptions
opIn :: OperatorContainer x => x -> x -> x
opIn x y = injectOp (MkOpIn (OpIn x y))
instance BinaryOperator (OpIn x) where
    opLexeme _ = L_in
instance TypeOf x => TypeOf (OpIn x) where
    typeOf (OpIn a b) = do
        tyA <- typeOf a
        TypeSet tyB <- typeOf b
        if tyA `typeUnify` tyB
            then return TypeBool
            else userErr "Type error"


data OpSubset x = OpSubset x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpSubset x)
instance Hashable  x => Hashable  (OpSubset x)
instance ToJSON    x => ToJSON    (OpSubset x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubset x) where parseJSON = JSON.genericParseJSON jsonOptions
opSubset :: OperatorContainer x => x -> x -> x
opSubset x y = injectOp (MkOpSubset (OpSubset x y))
instance BinaryOperator (OpSubset x) where
    opLexeme _ = L_subset
instance TypeOf x => TypeOf (OpSubset x) where
    typeOf (OpSubset a b) = sameToSameToBool a b


data OpSubsetEq x = OpSubsetEq x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpSubsetEq x)
instance Hashable  x => Hashable  (OpSubsetEq x)
instance ToJSON    x => ToJSON    (OpSubsetEq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubsetEq x) where parseJSON = JSON.genericParseJSON jsonOptions
opSubsetEq :: OperatorContainer x => x -> x -> x
opSubsetEq x y = injectOp (MkOpSubsetEq (OpSubsetEq x y))
instance BinaryOperator (OpSubsetEq x) where
    opLexeme _ = L_subsetEq
instance TypeOf x => TypeOf (OpSubsetEq x) where
    typeOf (OpSubsetEq a b) = sameToSameToBool a b


data OpSupset x = OpSupset x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpSupset x)
instance Hashable  x => Hashable  (OpSupset x)
instance ToJSON    x => ToJSON    (OpSupset x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSupset x) where parseJSON = JSON.genericParseJSON jsonOptions
opSupset :: OperatorContainer x => x -> x -> x
opSupset x y = injectOp (MkOpSupset (OpSupset x y))
instance BinaryOperator (OpSupset x) where
    opLexeme _ = L_supset
instance TypeOf x => TypeOf (OpSupset x) where
    typeOf (OpSupset a b) = sameToSameToBool a b


data OpSupsetEq x = OpSupsetEq x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpSupsetEq x)
instance Hashable  x => Hashable  (OpSupsetEq x)
instance ToJSON    x => ToJSON    (OpSupsetEq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSupsetEq x) where parseJSON = JSON.genericParseJSON jsonOptions
opSupsetEq :: OperatorContainer x => x -> x -> x
opSupsetEq x y = injectOp (MkOpSupsetEq (OpSupsetEq x y))
instance BinaryOperator (OpSupsetEq x) where
    opLexeme _ = L_supsetEq
instance TypeOf x => TypeOf (OpSupsetEq x) where
    typeOf (OpSupsetEq a b) = sameToSameToBool a b


data OpIntersect x = OpIntersect x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpIntersect x)
instance Hashable  x => Hashable  (OpIntersect x)
instance ToJSON    x => ToJSON    (OpIntersect x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIntersect x) where parseJSON = JSON.genericParseJSON jsonOptions
opIntersect :: OperatorContainer x => x -> x -> x
opIntersect x y = injectOp (MkOpIntersect (OpIntersect x y))
instance BinaryOperator (OpIntersect x) where
    opLexeme _ = L_intersect
instance TypeOf x => TypeOf (OpIntersect x) where
    typeOf (OpIntersect a b) = sameToSameToSame a b


data OpUnion x = OpUnion x x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpUnion x)
instance Hashable  x => Hashable  (OpUnion x)
instance ToJSON    x => ToJSON    (OpUnion x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpUnion x) where parseJSON = JSON.genericParseJSON jsonOptions
opUnion :: OperatorContainer x => x -> x -> x
opUnion x y = injectOp (MkOpUnion (OpUnion x y))
instance BinaryOperator (OpUnion x) where
    opLexeme _ = L_union
instance TypeOf x => TypeOf (OpUnion x) where
    typeOf (OpUnion a b) = sameToSameToSame a b


data OpFunctionImage x = OpFunctionImage x [x]
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpFunctionImage x)
instance Hashable  x => Hashable  (OpFunctionImage x)
instance ToJSON    x => ToJSON    (OpFunctionImage x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFunctionImage x) where parseJSON = JSON.genericParseJSON jsonOptions
opFunctionImage :: OperatorContainer x => x -> [x] -> x
opFunctionImage x y = injectOp (MkOpFunctionImage (OpFunctionImage x y))
instance TypeOf x => TypeOf (OpFunctionImage x) where
    typeOf (OpFunctionImage _ _) = return TypeAny


data OpDefined x = OpDefined x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpDefined x)
instance Hashable  x => Hashable  (OpDefined x)
instance ToJSON    x => ToJSON    (OpDefined x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDefined x) where parseJSON = JSON.genericParseJSON jsonOptions
opDefined :: OperatorContainer x => x -> x
opDefined x = injectOp (MkOpDefined (OpDefined x))
instance TypeOf x => TypeOf (OpDefined x) where
    typeOf (OpDefined x) = do
        TypeFunction a _ <- typeOf x
        return (TypeSet a)


data OpRange x = OpRange x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpRange x)
instance Hashable  x => Hashable  (OpRange x)
instance ToJSON    x => ToJSON    (OpRange x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRange x) where parseJSON = JSON.genericParseJSON jsonOptions
opRange :: OperatorContainer x => x -> x
opRange x = injectOp (MkOpRange (OpRange x))
instance TypeOf x => TypeOf (OpRange x) where
    typeOf (OpRange x) = do
        TypeFunction _ a <- typeOf x
        return (TypeSet a)


data OpAllDiff x = OpAllDiff x
    deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance Serialize x => Serialize (OpAllDiff x)
instance Hashable  x => Hashable  (OpAllDiff x)
instance ToJSON    x => ToJSON    (OpAllDiff x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAllDiff x) where parseJSON = JSON.genericParseJSON jsonOptions
opAllDiff :: OperatorContainer x => x -> x
opAllDiff x = injectOp (MkOpAllDiff (OpAllDiff x))
instance TypeOf x => TypeOf (OpAllDiff x) where
    typeOf (OpAllDiff x) = do
        TypeMatrix TypeInt TypeInt <- typeOf x
        return TypeBool


intToInt :: (MonadFail m, TypeOf a) => a -> m Type
intToInt a = do
    TypeInt{} <- typeOf a
    return TypeInt

intToIntToInt :: (MonadFail m, TypeOf a) => a -> a -> m Type
intToIntToInt a b = do
    TypeInt{} <- typeOf a
    TypeInt{} <- typeOf b
    return TypeInt

boolToBoolToBool :: (MonadFail m, TypeOf a) => a -> a -> m Type
boolToBoolToBool a b = do
    TypeBool{} <- typeOf a
    TypeBool{} <- typeOf b
    return TypeBool

sameToSameToBool :: (MonadFail m, TypeOf a) => a -> a -> m Type
sameToSameToBool a b = do
    tyA <- typeOf a
    tyB <- typeOf b
    if tyA `typeUnify` tyB
        then return TypeBool
        else bug "sameToSameToBool"

sameToSameToSame :: (MonadFail m, TypeOf a) => a -> a -> m Type
sameToSameToSame a b = do
    tyA <- typeOf a
    tyB <- typeOf b
    if tyA `typeUnify` tyB
        then return (mostDefined [tyA,tyB])
        else bug "sameToSameToSame"


mkBinOp :: OperatorContainer x => Text -> x -> x -> x
mkBinOp op a b =
    case textToLexeme op of
        Nothing -> bug ("Unknown binary operator:" <+> pretty op)
        Just l  ->
            let
                f = case l of
                    L_Plus  -> opPlus
                    L_Minus -> opMinus
                    L_Times -> opTimes
                    L_Div   -> opDiv
                    L_Mod   -> opMod
                    L_Pow   -> opPow
                    L_Eq    -> opEq
                    L_Neq   -> opNeq
                    L_Lt    -> opLt
                    L_Leq   -> opLeq
                    L_Gt    -> opGt
                    L_Geq   -> opGeq
                    L_in    -> opIn
                    L_And   -> opAnd
                    L_Or    -> opOr
                    L_Imply -> opImply
                    L_Iff   -> opEq
                    L_subset    -> opSubset
                    L_subsetEq  -> opSubsetEq
                    L_supset    -> opSupset
                    L_supsetEq  -> opSupsetEq
                    L_intersect -> opIntersect
                    L_union     -> opUnion
                    _ -> bug ("Unknown lexeme for binary operator:" <+> pretty (show l))
            in
                f a b

mkOp :: (OperatorContainer x, ReferenceContainer x) => Text -> [x] -> x
mkOp op xs =
    case textToLexeme op of
        Nothing -> case op of
            "and"    -> injectOp (MkOpAnd    (OpAnd    xs))
            "or"     -> injectOp (MkOpOr     (OpOr     xs))
            "sum"    -> injectOp (MkOpPlus   (OpPlus   xs))
            "not"    -> injectOp (MkOpNot    (OpNot    (headNote "not takes a single argument"    xs)))
            "negate" -> injectOp (MkOpNegate (OpNegate (headNote "negate takes a single argument" xs)))
            _     -> bug ("Unknown operator:" <+> vcat [pretty op, pretty $ show $ textToLexeme op])
            -- _     -> opFunctionImage (fromName (Name op)) xs
        Just l -> case l of
            L_toInt    -> opToInt    (headNote "toInt takes a single argument."    xs)
            L_defined  -> opDefined  (headNote "defined takes a single argument."  xs)
            L_range    -> opDefined  (headNote "range takes a single argument."    xs)
            L_allDiff  -> opAllDiff  (headNote "allDiff takes a single argument."  xs)
            L_dontCare -> opDontCare (headNote "dontCare takes a single argument." xs)
            L_flatten  -> opFlatten  (headNote "flatten takes a single argument."  xs)
            _ -> bug ("Unknown lexeme for operator:" <+> pretty (show l))



data Fixity = FNone | FLeft | FRight
    deriving Show

operators :: [(Lexeme,Fixity,Int)]
operators =
    [ ( L_Plus      , FLeft  ,  600 )
    , ( L_Minus     , FLeft  ,  600 )
    , ( L_Times     , FLeft  ,  700 )
    , ( L_Div       , FLeft  ,  700 )
    , ( L_Mod       , FLeft  ,  700 )
    , ( L_Pow       , FRight ,  800 )
    , ( L_Lt        , FNone  ,  400 )
    , ( L_Leq       , FNone  ,  400 )
    , ( L_Gt        , FNone  ,  400 )
    , ( L_Geq       , FNone  ,  400 )
    , ( L_Neq       , FNone  ,  400 )
    , ( L_Eq        , FNone  ,  400 )
    , ( L_Or        , FLeft  ,  110 )
    , ( L_And       , FLeft  ,  120 )
    , ( L_Imply     , FNone  ,   50 )
    , ( L_Iff       , FNone  ,   50 )
    , ( L_union     , FLeft  ,  600 )
    , ( L_intersect , FLeft  ,  700 )
    , ( L_subset    , FNone  ,  400 )
    , ( L_subsetEq  , FNone  ,  400 )
    , ( L_supset    , FNone  ,  400 )
    , ( L_supsetEq  , FNone  ,  400 )
    , ( L_in        , FNone  ,  550 )
    -- , ( L_Colon     , FNone  ,   10 )
    , ( L_HasRepr   , FNone  ,   10 )
    , ( L_HasType   , FNone  ,   10 )
    , ( L_HasDomain , FNone  ,   10 )
    , ( L_LexLt     , FNone  ,  400 )
    , ( L_LexLeq    , FNone  ,  400 )
    , ( L_LexGt     , FNone  ,  400 )
    , ( L_LexGeq    , FNone  ,  400 )
    , ( L_DotLt     , FNone  ,  400 )
    , ( L_DotLeq    , FNone  ,  400 )
    ]

functionals :: [Lexeme]
functionals =
    [ L_toInt
    , L_min
    , L_max
    , L_allDiff
    , L_dontCare
    , L_hist

    , L_toSet
    , L_toMSet
    , L_toRelation
    , L_defined
    , L_range
    , L_image
    , L_preImage
    , L_inverse
    , L_together
    , L_apart
    , L_party
    , L_participants
    , L_parts
    , L_freq
    , L_toInt
    , L_flatten
    , L_normIndices
    , L_indices
    ]
