{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Ops where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.Domain
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

class EvaluateOp op where
    evaluateOp :: MonadFail m => op Constant -> m Constant

data Ops x
    = MkOpPlus            (OpPlus x)
    | MkOpMinus           (OpMinus x)
    | MkOpTimes           (OpTimes x)
    | MkOpDiv             (OpDiv x)
    | MkOpMod             (OpMod x)
    | MkOpPow             (OpPow x)
    | MkOpTwoBars         (OpTwoBars x)
    | MkOpNegate          (OpNegate x)
    | MkOpFactorial       (OpFactorial x)

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

    | MkOpAllDiff         (OpAllDiff x)
    | MkOpIndexing        (OpIndexing x)
    | MkOpSlicing         (OpSlicing x)
    | MkOpFlatten         (OpFlatten x)
    | MkOpLexLt           (OpLexLt x)
    | MkOpLexLeq          (OpLexLeq x)

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
    | MkOpToSet           (OpToSet x)
    | MkOpToMSet          (OpToMSet x)
    | MkOpMax             (OpMax x)
    | MkOpMin             (OpMin x)

    | MkOpFunctionImage   (OpFunctionImage x)
    | MkOpPreImage        (OpPreImage x)
    | MkOpDefined         (OpDefined x)
    | MkOpRange           (OpRange x)

    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (Ops x)
instance Hashable  x => Hashable  (Ops x)
instance ToJSON    x => ToJSON    (Ops x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (Ops x) where parseJSON = JSON.genericParseJSON jsonOptions

instance (TypeOf x, Show x, Pretty x, ExpressionLike x) => TypeOf (Ops x) where
    typeOf (MkOpPlus                x) = typeOf x
    typeOf (MkOpMinus               x) = typeOf x
    typeOf (MkOpTimes               x) = typeOf x
    typeOf (MkOpDiv                 x) = typeOf x
    typeOf (MkOpMod                 x) = typeOf x
    typeOf (MkOpPow                 x) = typeOf x
    typeOf (MkOpTwoBars             x) = typeOf x
    typeOf (MkOpNegate              x) = typeOf x
    typeOf (MkOpFactorial           x) = typeOf x
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
    typeOf (MkOpAllDiff             x) = typeOf x
    typeOf (MkOpIndexing            x) = typeOf x
    typeOf (MkOpSlicing             x) = typeOf x
    typeOf (MkOpFlatten             x) = typeOf x
    typeOf (MkOpLexLt               x) = typeOf x
    typeOf (MkOpLexLeq              x) = typeOf x
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
    typeOf (MkOpToSet               x) = typeOf x
    typeOf (MkOpToMSet              x) = typeOf x
    typeOf (MkOpMax                 x) = typeOf x
    typeOf (MkOpMin                 x) = typeOf x
    typeOf (MkOpFunctionImage       x) = typeOf x
    typeOf (MkOpPreImage            x) = typeOf x
    typeOf (MkOpDefined             x) = typeOf x
    typeOf (MkOpRange               x) = typeOf x

instance EvaluateOp Ops where
    evaluateOp (MkOpPlus                x) = evaluateOp x
    evaluateOp (MkOpMinus               x) = evaluateOp x
    evaluateOp (MkOpTimes               x) = evaluateOp x
    evaluateOp (MkOpDiv                 x) = evaluateOp x
    evaluateOp (MkOpMod                 x) = evaluateOp x
    evaluateOp (MkOpPow                 x) = evaluateOp x
    evaluateOp (MkOpTwoBars             x) = evaluateOp x
    evaluateOp (MkOpNegate              x) = evaluateOp x
    evaluateOp (MkOpFactorial           x) = evaluateOp x
    evaluateOp (MkOpEq                  x) = evaluateOp x
    evaluateOp (MkOpNeq                 x) = evaluateOp x
    evaluateOp (MkOpLt                  x) = evaluateOp x
    evaluateOp (MkOpLeq                 x) = evaluateOp x
    evaluateOp (MkOpGt                  x) = evaluateOp x
    evaluateOp (MkOpGeq                 x) = evaluateOp x
    evaluateOp (MkOpAnd                 x) = evaluateOp x
    evaluateOp (MkOpOr                  x) = evaluateOp x
    evaluateOp (MkOpImply               x) = evaluateOp x
    evaluateOp (MkOpNot                 x) = evaluateOp x
    evaluateOp (MkOpAllDiff             x) = evaluateOp x
    evaluateOp (MkOpIndexing            x) = evaluateOp x
    evaluateOp (MkOpSlicing             x) = evaluateOp x
    evaluateOp (MkOpFlatten             x) = evaluateOp x
    evaluateOp (MkOpLexLt               x) = evaluateOp x
    evaluateOp (MkOpLexLeq              x) = evaluateOp x
    evaluateOp (MkOpTrue                x) = evaluateOp x
    evaluateOp (MkOpToInt               x) = evaluateOp x
    evaluateOp (MkOpDontCare            x) = evaluateOp x
    evaluateOp (MkOpIn                  x) = evaluateOp x
    evaluateOp (MkOpSubset              x) = evaluateOp x
    evaluateOp (MkOpSubsetEq            x) = evaluateOp x
    evaluateOp (MkOpSupset              x) = evaluateOp x
    evaluateOp (MkOpSupsetEq            x) = evaluateOp x
    evaluateOp (MkOpIntersect           x) = evaluateOp x
    evaluateOp (MkOpUnion               x) = evaluateOp x
    evaluateOp (MkOpToSet               x) = evaluateOp x
    evaluateOp (MkOpToMSet              x) = evaluateOp x
    evaluateOp (MkOpMax                 x) = evaluateOp x
    evaluateOp (MkOpMin                 x) = evaluateOp x
    evaluateOp (MkOpFunctionImage       x) = evaluateOp x
    evaluateOp (MkOpPreImage            x) = evaluateOp x
    evaluateOp (MkOpDefined             x) = evaluateOp x
    evaluateOp (MkOpRange               x) = evaluateOp x


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
    prettyPrec prec (MkOpPlus   op@(OpPlus  [a,b])) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpPlus      (OpPlus   xs  )) = "sum" <> prettyList prParens "," xs
    prettyPrec prec (MkOpMinus  op@(OpMinus  a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpTimes  op@(OpTimes [a,b])) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpTimes     (OpTimes  xs  )) = "product" <> prettyList prParens "," xs
    prettyPrec prec (MkOpDiv    op@(OpDiv    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpMod    op@(OpMod    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpPow    op@(OpPow    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpTwoBars   (OpTwoBars  a )) = "|" <> pretty a <> "|"
    prettyPrec _    (MkOpNegate    (OpNegate a   )) = "-" <> prettyPrec 10000 a
    prettyPrec _    (MkOpFactorial (OpFactorial a)) = "factorial" <> prParens (pretty a)
    prettyPrec prec (MkOpEq     op@(OpEq     a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpNeq    op@(OpNeq    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpLt     op@(OpLt     a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpLeq    op@(OpLeq    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpGt     op@(OpGt     a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpGeq    op@(OpGeq    a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpAnd    op@(OpAnd    xs  )) = case xs of
        []    -> "false"
        [x]   -> "and" <> prParens (pretty x)
        [x,y] -> prettyPrecBinOp prec [op] x y
        _     -> "and" <> prettyList (prParens . prBrackets) "," xs
    prettyPrec prec (MkOpOr    op@(OpOr     xs  )) = case xs of
        []    -> "true"
        [x]   -> "or" <> prParens (pretty x)
        [x,y] -> prettyPrecBinOp prec [op] x y
        _     -> "or" <> prettyList (prParens . prBrackets) "," xs
    prettyPrec prec (MkOpImply op@(OpImply  a b )) = prettyPrecBinOp prec [op] a b
    prettyPrec _    (MkOpNot      (OpNot    a   )) = "!" <> prettyPrec 10000 a
    prettyPrec _ (MkOpAllDiff  (OpAllDiff   a   )) = "allDiff"  <> prParens (pretty a)
    prettyPrec _ (MkOpIndexing (OpIndexing  a b )) = pretty a <> prBrackets (pretty b)
    prettyPrec _ (MkOpSlicing  (OpSlicing m a b )) = pretty m <> prBrackets (pretty a <> ".." <> pretty b)
    prettyPrec _ (MkOpFlatten  (OpFlatten m     )) = "flatten" <> prParens (pretty m)
    prettyPrec prec (MkOpLexLt  op@(OpLexLt  a b)) = prettyPrecBinOp prec [op] a b
    prettyPrec prec (MkOpLexLeq op@(OpLexLeq a b)) = prettyPrecBinOp prec [op] a b
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
    prettyPrec _ (MkOpToSet    (OpToSet    a)) = "toSet"    <> prParens (pretty a)
    prettyPrec _ (MkOpToMSet   (OpToMSet   a)) = "toMSet"   <> prParens (pretty a)
    prettyPrec _ (MkOpMax      (OpMax     xs)) = "max" <> prettyList prParens "," xs
    prettyPrec _ (MkOpMin      (OpMin     xs)) = "min" <> prettyList prParens "," xs
    prettyPrec _ (MkOpFunctionImage (OpFunctionImage a b)) = "image" <> prettyList prParens "," (a:b)
    prettyPrec _ (MkOpPreImage (OpPreImage a b)) = "preImage" <> prettyList prParens "," [a,b]
    prettyPrec _ (MkOpDefined  (OpDefined  a)) = "defined"  <> prParens (pretty a)
    prettyPrec _ (MkOpRange    (OpRange    a)) = "range"    <> prParens (pretty a)


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
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpPlus x)
instance Hashable  x => Hashable  (OpPlus x)
instance ToJSON    x => ToJSON    (OpPlus x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPlus x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpPlus x) where
    opLexeme _ = L_Plus
instance (TypeOf x, Pretty x) => TypeOf (OpPlus x) where
    typeOf (OpPlus [a,b]) = intToIntToInt a b
    typeOf (OpPlus [x]) = do
        TypeList TypeInt <- typeOf x
        return TypeInt
    typeOf p@(OpPlus xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeInt:tys)
            then return TypeInt
            else raiseTypeError (MkOpPlus p)
instance EvaluateOp OpPlus where
    evaluateOp (OpPlus xs) = ConstantInt . sum <$> concatMapM intsOut xs


data OpMinus x = OpMinus x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpMinus x)
instance Hashable  x => Hashable  (OpMinus x)
instance ToJSON    x => ToJSON    (OpMinus x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMinus x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpMinus x) where
    opLexeme _ = L_Minus
instance TypeOf x => TypeOf (OpMinus x) where
    typeOf (OpMinus a b) = intToIntToInt a b
instance EvaluateOp OpMinus where
    evaluateOp (OpMinus x y) = ConstantInt <$> ((-) <$> intOut x <*> intOut y)


data OpTimes x = OpTimes [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpTimes x)
instance Hashable  x => Hashable  (OpTimes x)
instance ToJSON    x => ToJSON    (OpTimes x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTimes x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpTimes x) where
    opLexeme _ = L_Times
instance (TypeOf x, Pretty x) => TypeOf (OpTimes x) where
    typeOf (OpTimes [a,b]) = intToIntToInt a b
    typeOf p@(OpTimes xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeInt:tys)
            then return TypeInt
            else raiseTypeError (MkOpTimes p)
instance EvaluateOp OpTimes where
    evaluateOp (OpTimes xs) = ConstantInt . product <$> concatMapM intsOut xs


data OpDiv x = OpDiv x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpDiv x)
instance Hashable  x => Hashable  (OpDiv x)
instance ToJSON    x => ToJSON    (OpDiv x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDiv x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpDiv x) where
    opLexeme _ = L_Div
instance TypeOf x => TypeOf (OpDiv x) where
    typeOf (OpDiv a b) = intToIntToInt a b
instance EvaluateOp OpDiv where
    evaluateOp (OpDiv x y) = ConstantInt <$> (div <$> intOut x <*> intOut y)


data OpMod x = OpMod x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpMod x)
instance Hashable  x => Hashable  (OpMod x)
instance ToJSON    x => ToJSON    (OpMod x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMod x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpMod x) where
    opLexeme _ = L_Mod
instance TypeOf x => TypeOf (OpMod x) where
    typeOf (OpMod a b) = intToIntToInt a b
instance EvaluateOp OpMod where
    evaluateOp (OpMod x y) = ConstantInt <$> (mod <$> intOut x <*> intOut y)


data OpPow x = OpPow x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpPow x)
instance Hashable  x => Hashable  (OpPow x)
instance ToJSON    x => ToJSON    (OpPow x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPow x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpPow x) where
    opLexeme _ = L_Pow
instance TypeOf x => TypeOf (OpPow x) where
    typeOf (OpPow a b) = intToIntToInt a b
instance EvaluateOp OpPow where
    evaluateOp (OpPow x y) = ConstantInt <$> ((^) <$> intOut x <*> intOut y)


data OpTwoBars x = OpTwoBars x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpTwoBars x)
instance Hashable  x => Hashable  (OpTwoBars x)
instance ToJSON    x => ToJSON    (OpTwoBars x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTwoBars x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpTwoBars x) where
    typeOf p@(OpTwoBars a) = do
        ty <- typeOf a
        case ty of
            TypeInt{}      -> return ()
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            _              -> raiseTypeError (MkOpTwoBars p)
        return TypeInt
instance EvaluateOp OpTwoBars where
    evaluateOp (OpTwoBars x) =
        case x of
            ConstantInt y                        -> return $ ConstantInt $ abs y
            ConstantAbstract (AbsLitSet xs)      -> return $ ConstantInt $ length $ nub xs
            ConstantAbstract (AbsLitMSet xs)     -> return $ ConstantInt $ length       xs
            ConstantAbstract (AbsLitFunction xs) -> return $ ConstantInt $ length $ nub xs
            _ -> fail $ "evaluateOp OpTwoBars" <+> pretty (show x)


data OpNegate x = OpNegate x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpNegate x)
instance Hashable  x => Hashable  (OpNegate x)
instance ToJSON    x => ToJSON    (OpNegate x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNegate x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpNegate x) where
    typeOf (OpNegate a) = do TypeInt <- typeOf a ; return TypeInt
instance EvaluateOp OpNegate where
    evaluateOp (OpNegate x) = ConstantInt . negate <$> intOut x


data OpFactorial x = OpFactorial x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpFactorial x)
instance Hashable  x => Hashable  (OpFactorial x)
instance ToJSON    x => ToJSON    (OpFactorial x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFactorial x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpFactorial x) where
    typeOf (OpFactorial a) = do TypeInt <- typeOf a ; return TypeInt
instance EvaluateOp OpFactorial where
    evaluateOp (OpFactorial x) = ConstantInt . product . enumFromTo 1 <$> intOut x


data OpEq x = OpEq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpEq x)
instance Hashable  x => Hashable  (OpEq x)
instance ToJSON    x => ToJSON    (OpEq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpEq x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpEq x) where
    opLexeme _ = L_Eq
instance (TypeOf x, Pretty x) => TypeOf (OpEq x) where
    typeOf (OpEq a b) = sameToSameToBool a b
instance EvaluateOp OpEq where
    evaluateOp (OpEq a b) = return $ ConstantBool $ eq a b
        where
            eqs e xs ys = and (zipWith e xs ys)
            eq (ConstantBool x) (ConstantBool y) = x == y
            eq (ConstantInt  x) (ConstantInt  y) = x == y
            eq (ConstantAbstract (AbsLitTuple xs)) (ConstantAbstract (AbsLitTuple ys))
                = eqs eq xs ys
            eq (ConstantAbstract (AbsLitSet xs)) (ConstantAbstract (AbsLitSet ys))
                = eqs eq (sortNub xs) (sortNub ys)
            eq (ConstantAbstract (AbsLitMSet xs)) (ConstantAbstract (AbsLitMSet ys))
                = eqs eq (sort xs) (sort ys)
            eq (ConstantAbstract (AbsLitFunction xs)) (ConstantAbstract (AbsLitFunction ys))
                = eqs (\ x y -> eq (fst x) (fst y) && eq (snd x) (snd y) )
                      (sort xs) (sort ys)
            eq (ConstantAbstract (AbsLitRelation xs)) (ConstantAbstract (AbsLitRelation ys))
                = eqs (eqs eq) (sort xs) (sort ys)
            eq (ConstantAbstract (AbsLitPartition xs)) (ConstantAbstract (AbsLitPartition ys))
                = eqs (eqs eq) (sortNub xs) (sortNub ys)
            eq _ _ = False


data OpNeq x = OpNeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpNeq x)
instance Hashable  x => Hashable  (OpNeq x)
instance ToJSON    x => ToJSON    (OpNeq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNeq x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpNeq x) where
    opLexeme _ = L_Neq
instance (TypeOf x, Pretty x) => TypeOf (OpNeq x) where
    typeOf (OpNeq a b) = sameToSameToBool a b
instance EvaluateOp OpNeq where
    evaluateOp (OpNeq x y) = do
        r <- evaluateOp (OpEq x y)
        evaluateOp (OpNot r)


data OpLt x = OpLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpLt x)
instance Hashable  x => Hashable  (OpLt x)
instance ToJSON    x => ToJSON    (OpLt x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLt x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpLt x) where
    opLexeme _ = L_Lt
instance (TypeOf x, Pretty x) => TypeOf (OpLt x) where
    typeOf (OpLt a b) = sameToSameToBool a b
instance EvaluateOp OpLt where
    evaluateOp (OpLt x y) = return $ ConstantBool $ x < y


data OpLeq x = OpLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpLeq x)
instance Hashable  x => Hashable  (OpLeq x)
instance ToJSON    x => ToJSON    (OpLeq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLeq x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpLeq x) where
    opLexeme _ = L_Leq
instance (TypeOf x, Pretty x) => TypeOf (OpLeq x) where
    typeOf (OpLeq a b) = sameToSameToBool a b
instance EvaluateOp OpLeq where
    evaluateOp (OpLeq x y) = return $ ConstantBool $ x <= y


data OpGt x = OpGt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpGt x)
instance Hashable  x => Hashable  (OpGt x)
instance ToJSON    x => ToJSON    (OpGt x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGt x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpGt x) where
    opLexeme _ = L_Gt
instance (TypeOf x, Pretty x) => TypeOf (OpGt x) where
    typeOf (OpGt a b) = sameToSameToBool a b
instance EvaluateOp OpGt where
    evaluateOp (OpGt x y) = return $ ConstantBool $ x > y


data OpGeq x = OpGeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpGeq x)
instance Hashable  x => Hashable  (OpGeq x)
instance ToJSON    x => ToJSON    (OpGeq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGeq x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpGeq x) where
    opLexeme _ = L_Geq
instance (TypeOf x, Pretty x) => TypeOf (OpGeq x) where
    typeOf (OpGeq a b) = sameToSameToBool a b
instance EvaluateOp OpGeq where
    evaluateOp (OpGeq x y) = return $ ConstantBool $ x >= y


data OpAnd x = OpAnd [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpAnd x)
instance Hashable  x => Hashable  (OpAnd x)
instance ToJSON    x => ToJSON    (OpAnd x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAnd x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpAnd x) where
    opLexeme _ = L_And
instance (TypeOf x, Pretty x) => TypeOf (OpAnd x) where
    typeOf (OpAnd [a,b]) = boolToBoolToBool a b
    typeOf (OpAnd [x]) = do
        TypeList TypeBool <- typeOf x
        return TypeBool
    typeOf p@(OpAnd xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeBool:tys)
            then return TypeBool
            else raiseTypeError (MkOpAnd p)
instance EvaluateOp OpAnd where
    evaluateOp (OpAnd xs) = ConstantBool . and . concat <$> mapM boolsOut xs


data OpOr x = OpOr [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpOr x)
instance Hashable  x => Hashable  (OpOr x)
instance ToJSON    x => ToJSON    (OpOr x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpOr x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpOr x) where
    opLexeme _ = L_Or
instance (TypeOf x, Pretty x) => TypeOf (OpOr x) where
    typeOf (OpOr [a,b]) = boolToBoolToBool a b
    typeOf (OpOr [x]) = do
        TypeList TypeBool <- typeOf x
        return TypeBool
    typeOf p@(OpOr xs) = do
        tys <- mapM typeOf xs
        if typesUnify (TypeBool:tys)
            then return TypeBool
            else raiseTypeError (MkOpOr p)
instance EvaluateOp OpOr where
    evaluateOp (OpOr xs) = ConstantBool . or . concat <$> mapM boolsOut xs


data OpImply x = OpImply x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpImply x)
instance Hashable  x => Hashable  (OpImply x)
instance ToJSON    x => ToJSON    (OpImply x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpImply x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpImply x) where
    opLexeme _ = L_Imply
instance TypeOf x => TypeOf (OpImply x) where
    typeOf (OpImply a b) = boolToBoolToBool a b
instance EvaluateOp OpImply where
    evaluateOp (OpImply x y) = ConstantBool <$> ((<=) <$> boolOut x <*> boolOut y)


data OpNot x = OpNot x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpNot x)
instance Hashable  x => Hashable  (OpNot x)
instance ToJSON    x => ToJSON    (OpNot x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNot x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpNot x) where
    typeOf (OpNot a) = do TypeBool <- typeOf a ; return TypeBool
instance EvaluateOp OpNot where
    evaluateOp (OpNot x) = ConstantBool . not <$> boolOut x


data OpIndexing x = OpIndexing x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpIndexing x)
instance Hashable  x => Hashable  (OpIndexing x)
instance ToJSON    x => ToJSON    (OpIndexing x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIndexing x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Show x, Pretty x, ExpressionLike x) => TypeOf (OpIndexing x) where
    typeOf (OpIndexing m i) = do
        tyM <- typeOf m
        case tyM of
            TypeMatrix _ inn -> return inn
            TypeTuple inns   -> do
                TypeInt{} <- typeOf i
                iInt <- intOut i
                return (at inns (iInt-1))
            _ -> fail ("Indexing something other than a matrix or a tuple:" <++> vcat [pretty m, pretty tyM])
instance EvaluateOp OpIndexing where
    evaluateOp (OpIndexing m@(ConstantAbstract (AbsLitMatrix (DomainInt index) vals)) (ConstantInt x)) = do
        indexVals <- valuesInIntDomain index
        case [ v | (i, v) <- zip indexVals vals, i == x ] of
            [v] -> return v
            []  -> fail $ vcat [ "Matrix is not defined at this point:" <+> pretty x
                               , "Matrix value:" <+> pretty m
                               ]
            _   -> fail $ vcat [ "Matrix is multiply defined at this point:" <+> pretty x
                               , "Matrix value:" <+> pretty m
                               ]
    evaluateOp (OpIndexing (ConstantAbstract (AbsLitTuple vals)) (ConstantInt x)) = return (at vals (x-1))
    evaluateOp op = na $ "evaluateOp{OpIndexing}:" <++> pretty (show op)


data OpSlicing x = OpSlicing x (Maybe x) (Maybe x)
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpSlicing x)
instance Hashable  x => Hashable  (OpSlicing x)
instance ToJSON    x => ToJSON    (OpSlicing x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSlicing x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpSlicing x) where
    typeOf (OpSlicing m _ _) = do
        t@TypeMatrix{} <- typeOf m
        return t
instance EvaluateOp OpSlicing where
    evaluateOp op = na $ "evaluateOp{OpSlicing}:" <++> pretty (show op)


data OpFlatten x = OpFlatten x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpFlatten x)
instance Hashable  x => Hashable  (OpFlatten x)
instance ToJSON    x => ToJSON    (OpFlatten x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFlatten x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpFlatten x) where
    typeOf (OpFlatten m) = do
        let flattenType (TypeList inner) = flattenType inner
            flattenType ty = ty
        TypeList n <- typeOf m
        return (TypeList (flattenType n))
instance EvaluateOp OpFlatten where
    evaluateOp (OpFlatten m) = do
        let flat (ConstantAbstract (AbsLitMatrix _ xs)) = concatMap flat xs
            flat c = [c]
        let flattened = flat m
        return (ConstantAbstract (AbsLitMatrix
                    (DomainInt [RangeBounded (fromInt 1) (fromInt (length flattened))])
                    flattened))


data OpLexLt x = OpLexLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpLexLt x)
instance Hashable  x => Hashable  (OpLexLt x)
instance ToJSON    x => ToJSON    (OpLexLt x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLexLt x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpLexLt x) where
    opLexeme _ = L_LexLt
instance TypeOf x => TypeOf (OpLexLt x) where
    typeOf (OpLexLt a b) = do
        TypeMatrix{} <- typeOf a
        TypeMatrix{} <- typeOf b
        return TypeBool
instance EvaluateOp OpLexLt where
    evaluateOp (OpLexLt (ConstantAbstract (AbsLitMatrix _ xs)) (ConstantAbstract (AbsLitMatrix _ ys))) =
        return $ ConstantBool $ xs < ys
    evaluateOp op = na $ "evaluateOp{OpLexLt}:" <++> pretty (show op)


data OpLexLeq x = OpLexLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpLexLeq x)
instance Hashable  x => Hashable  (OpLexLeq x)
instance ToJSON    x => ToJSON    (OpLexLeq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLexLeq x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpLexLeq x) where
    opLexeme _ = L_LexLeq
instance TypeOf x => TypeOf (OpLexLeq x) where
    typeOf (OpLexLeq a b) = do
        TypeMatrix{} <- typeOf a
        TypeMatrix{} <- typeOf b
        return TypeBool
instance EvaluateOp OpLexLeq where
    evaluateOp (OpLexLeq (ConstantAbstract (AbsLitMatrix _ xs)) (ConstantAbstract (AbsLitMatrix _ ys))) =
        return $ ConstantBool $ xs <= ys
    evaluateOp op = na $ "evaluateOp{OpLexLeq}:" <++> pretty (show op)


data OpTrue x = OpTrue x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpTrue x)
instance Hashable  x => Hashable  (OpTrue x)
instance ToJSON    x => ToJSON    (OpTrue x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTrue x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpTrue x) where
    typeOf (OpTrue _) = return TypeBool
instance EvaluateOp OpTrue where
    evaluateOp op = na $ "evaluateOp{OpTrue}:" <++> pretty (show op)


data OpToInt x = OpToInt x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpToInt x)
instance Hashable  x => Hashable  (OpToInt x)
instance ToJSON    x => ToJSON    (OpToInt x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToInt x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpToInt x) where
    typeOf (OpToInt x) = do
        TypeBool{} <- typeOf x
        return TypeInt
instance EvaluateOp OpToInt where
    evaluateOp (OpToInt (ConstantBool False)) = return (ConstantInt 0)
    evaluateOp (OpToInt (ConstantBool True )) = return (ConstantInt 1)
    evaluateOp op = na $ "evaluateOp{OpToInt}:" <++> pretty (show op)


data OpDontCare x = OpDontCare x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpDontCare x)
instance Hashable  x => Hashable  (OpDontCare x)
instance ToJSON    x => ToJSON    (OpDontCare x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDontCare x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpDontCare x) where
    typeOf (OpDontCare _) = return TypeBool
instance EvaluateOp OpDontCare where
    evaluateOp op = na $ "evaluateOp{OpDontcare}:" <++> pretty (show op)


data OpIn x = OpIn x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpIn x)
instance Hashable  x => Hashable  (OpIn x)
instance ToJSON    x => ToJSON    (OpIn x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIn x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpIn x) where
    opLexeme _ = L_in
instance (TypeOf x, Pretty x) => TypeOf (OpIn x) where
    typeOf p@(OpIn a b) = do
        tyA <- typeOf a
        TypeSet tyB <- typeOf b
        if tyA `typeUnify` tyB
            then return TypeBool
            else raiseTypeError (MkOpIn p)
instance EvaluateOp OpIn where
    evaluateOp (OpIn c (ConstantAbstract (AbsLitSet  cs))) = return $ ConstantBool $ elem c cs
    evaluateOp (OpIn c (ConstantAbstract (AbsLitMSet cs))) = return $ ConstantBool $ elem c cs
    evaluateOp op = na $ "evaluateOp{OpIn}:" <++> pretty (show op)


data OpSubset x = OpSubset x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpSubset x)
instance Hashable  x => Hashable  (OpSubset x)
instance ToJSON    x => ToJSON    (OpSubset x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubset x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpSubset x) where
    opLexeme _ = L_subset
instance (TypeOf x, Pretty x) => TypeOf (OpSubset x) where
    typeOf (OpSubset a b) = sameToSameToBool a b
instance EvaluateOp OpSubset where
    evaluateOp (OpSubset (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantBool $ all (`elem` bs) as && length as <= length bs
    evaluateOp op = na $ "evaluateOp{OpSubset}:" <++> pretty (show op)


data OpSubsetEq x = OpSubsetEq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpSubsetEq x)
instance Hashable  x => Hashable  (OpSubsetEq x)
instance ToJSON    x => ToJSON    (OpSubsetEq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubsetEq x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpSubsetEq x) where
    opLexeme _ = L_subsetEq
instance (TypeOf x, Pretty x) => TypeOf (OpSubsetEq x) where
    typeOf (OpSubsetEq a b) = sameToSameToBool a b
instance EvaluateOp OpSubsetEq where
    evaluateOp (OpSubsetEq (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantBool $ all (`elem` bs) as
    evaluateOp op = na $ "evaluateOp{OpSubsetEq}:" <++> pretty (show op)


data OpSupset x = OpSupset x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpSupset x)
instance Hashable  x => Hashable  (OpSupset x)
instance ToJSON    x => ToJSON    (OpSupset x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSupset x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpSupset x) where
    opLexeme _ = L_supset
instance (TypeOf x, Pretty x) => TypeOf (OpSupset x) where
    typeOf (OpSupset a b) = sameToSameToBool a b
instance EvaluateOp OpSupset where
    evaluateOp (OpSupset (ConstantAbstract (AbsLitSet bs)) (ConstantAbstract (AbsLitSet as))) =
        return $ ConstantBool $ all (`elem` bs) as && length as <= length bs
    evaluateOp op = na $ "evaluateOp{OpSupset}:" <++> pretty (show op)


data OpSupsetEq x = OpSupsetEq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpSupsetEq x)
instance Hashable  x => Hashable  (OpSupsetEq x)
instance ToJSON    x => ToJSON    (OpSupsetEq x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSupsetEq x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpSupsetEq x) where
    opLexeme _ = L_supsetEq
instance (TypeOf x, Pretty x) => TypeOf (OpSupsetEq x) where
    typeOf (OpSupsetEq a b) = sameToSameToBool a b
instance EvaluateOp OpSupsetEq where
    evaluateOp (OpSupsetEq (ConstantAbstract (AbsLitSet bs)) (ConstantAbstract (AbsLitSet as))) =
        return $ ConstantBool $ all (`elem` bs) as
    evaluateOp op = na $ "evaluateOp{OpSupsetEq}:" <++> pretty (show op)


data OpIntersect x = OpIntersect x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpIntersect x)
instance Hashable  x => Hashable  (OpIntersect x)
instance ToJSON    x => ToJSON    (OpIntersect x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIntersect x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpIntersect x) where
    opLexeme _ = L_intersect
instance (TypeOf x, Pretty x) => TypeOf (OpIntersect x) where
    typeOf (OpIntersect a b) = sameToSameToSame a b
instance EvaluateOp OpIntersect where
    evaluateOp (OpIntersect (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub [ i | i <- as, i `elem` bs]
    evaluateOp op = na $ "evaluateOp{OpIntersect}:" <++> pretty (show op)


data OpUnion x = OpUnion x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpUnion x)
instance Hashable  x => Hashable  (OpUnion x)
instance ToJSON    x => ToJSON    (OpUnion x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpUnion x) where parseJSON = JSON.genericParseJSON jsonOptions
instance BinaryOperator (OpUnion x) where
    opLexeme _ = L_union
instance (TypeOf x, Pretty x) => TypeOf (OpUnion x) where
    typeOf (OpUnion a b) = sameToSameToSame a b
instance EvaluateOp OpUnion where
    evaluateOp (OpUnion (ConstantAbstract (AbsLitSet as)) (ConstantAbstract (AbsLitSet bs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub (as ++ bs)
    evaluateOp op = na $ "evaluateOp{OpUnion}:" <++> pretty (show op)


data OpToSet x = OpToSet x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpToSet x)
instance Hashable  x => Hashable  (OpToSet x)
instance ToJSON    x => ToJSON    (OpToSet x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToSet x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpToSet x) where
    typeOf p@(OpToSet x) = do
        tx <- typeOf x
        case tx of
            TypeRelation is  -> return (TypeSet (TypeTuple is))
            TypeMSet i       -> return (TypeSet i)
            TypeFunction i j -> return (TypeSet (TypeTuple [i,j]))
            _ -> raiseTypeError (MkOpToSet p)
instance EvaluateOp OpToSet where
    evaluateOp (OpToSet (ConstantAbstract (AbsLitSet xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet (ConstantAbstract (AbsLitMSet xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub xs
    evaluateOp (OpToSet (ConstantAbstract (AbsLitFunction xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub [ConstantAbstract (AbsLitTuple [a,b]) | (a,b) <- xs]
    evaluateOp (OpToSet (ConstantAbstract (AbsLitRelation xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map (ConstantAbstract . AbsLitTuple) xs
    evaluateOp op = na $ "evaluateOp{OpToSet}:" <++> pretty (show op)


data OpToMSet x = OpToMSet x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpToMSet x)
instance Hashable  x => Hashable  (OpToMSet x)
instance ToJSON    x => ToJSON    (OpToMSet x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToMSet x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpToMSet x) where
    typeOf p@(OpToMSet x) = do
        tx <- typeOf x
        case tx of
            TypeRelation is  -> return (TypeMSet (TypeTuple is))
            TypeSet i        -> return (TypeMSet i)
            TypeFunction i j -> return (TypeMSet (TypeTuple [i,j]))
            _ -> raiseTypeError (MkOpToMSet p)
instance EvaluateOp OpToMSet where
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitSet xs))) =
        return $ ConstantAbstract $ AbsLitMSet xs
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitMSet xs))) =
        return $ ConstantAbstract $ AbsLitMSet xs
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitFunction xs))) =
        return $ ConstantAbstract $ AbsLitMSet [ConstantAbstract (AbsLitTuple [a,b]) | (a,b) <- xs]
    evaluateOp (OpToMSet (ConstantAbstract (AbsLitRelation xs))) =
        return $ ConstantAbstract $ AbsLitMSet $ map (ConstantAbstract . AbsLitTuple) xs
    evaluateOp op = na $ "evaluateOp{OpToMSet}:" <++> pretty (show op)


data OpMax x = OpMax [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpMax x)
instance Hashable  x => Hashable  (OpMax x)
instance ToJSON    x => ToJSON    (OpMax x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMax x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpMax x) where
    typeOf (OpMax [a,b]) = intToIntToInt a b
    typeOf (OpMax [x]) = do
        TypeList TypeInt <- typeOf x
        return TypeInt
    typeOf p = raiseTypeError (MkOpMax p)
instance EvaluateOp OpMax where
    evaluateOp (OpMax [ConstantAbstract (AbsLitList xs)]) = ConstantInt . maximum <$> concatMapM intsOut xs
    evaluateOp (OpMax [ConstantAbstract (AbsLitSet  xs)]) = ConstantInt . maximum <$> concatMapM intsOut xs
    evaluateOp (OpMax [ConstantAbstract (AbsLitMSet xs)]) = ConstantInt . maximum <$> concatMapM intsOut xs
    evaluateOp (OpMax                               xs)   = ConstantInt . maximum <$> concatMapM intsOut xs


data OpMin x = OpMin [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpMin x)
instance Hashable  x => Hashable  (OpMin x)
instance ToJSON    x => ToJSON    (OpMin x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMin x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpMin x) where
    typeOf (OpMin [a,b]) = intToIntToInt a b
    typeOf (OpMin [x]) = do
        TypeList TypeInt <- typeOf x
        return TypeInt
    typeOf p = raiseTypeError (MkOpMin p)
instance EvaluateOp OpMin where
    evaluateOp (OpMin [ConstantAbstract (AbsLitList xs)]) = ConstantInt . minimum <$> concatMapM intsOut xs
    evaluateOp (OpMin [ConstantAbstract (AbsLitSet  xs)]) = ConstantInt . minimum <$> concatMapM intsOut xs
    evaluateOp (OpMin [ConstantAbstract (AbsLitMSet xs)]) = ConstantInt . minimum <$> concatMapM intsOut xs
    evaluateOp (OpMin                               xs)   = ConstantInt . minimum <$> concatMapM intsOut xs


data OpFunctionImage x = OpFunctionImage x [x]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpFunctionImage x)
instance Hashable  x => Hashable  (OpFunctionImage x)
instance ToJSON    x => ToJSON    (OpFunctionImage x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFunctionImage x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpFunctionImage x) where
    typeOf p@(OpFunctionImage f xs) = do
        TypeFunction from to <- typeOf f
        xTys <- mapM typeOf xs
        let xTy = case xTys of
                    [t] -> t
                    _   -> TypeTuple xTys
        if typesUnify [xTy, from]
            then return to
            else raiseTypeError (MkOpFunctionImage p)
instance EvaluateOp OpFunctionImage where
    evaluateOp (OpFunctionImage (ConstantAbstract (AbsLitFunction xs)) [a]) =
        case [ y | (x,y) <- xs, a == x ] of
            [y] -> return y
            []  -> fail $ vcat [ "Function is not defined at this point:" <+> pretty a
                               , "Function value:" <+> pretty (ConstantAbstract (AbsLitFunction xs))
                               ]
            _   -> fail $ vcat [ "Function is multiply defined at this point:" <+> pretty a
                               , "Function value:" <+> pretty (ConstantAbstract (AbsLitFunction xs))
                               ]
    evaluateOp op = na $ "evaluateOp{OpFunctionImage}:" <++> pretty (show op)


data OpPreImage x = OpPreImage x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpPreImage x)
instance Hashable  x => Hashable  (OpPreImage x)
instance ToJSON    x => ToJSON    (OpPreImage x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPreImage x) where parseJSON = JSON.genericParseJSON jsonOptions
instance (TypeOf x, Pretty x) => TypeOf (OpPreImage x) where
    typeOf p@(OpPreImage f x) = do
        TypeFunction from to <- typeOf f
        xTy <- typeOf x
        if typesUnify [xTy, to]
            then return (TypeSet from)
            else raiseTypeError (MkOpPreImage p)
instance EvaluateOp OpPreImage where
    evaluateOp (OpPreImage (ConstantAbstract (AbsLitFunction xs)) a) =
        return $ ConstantAbstract $ AbsLitSet [ x | (x,y) <- xs, a == y ]
    evaluateOp op = na $ "evaluateOp{OpPreImage}:" <++> pretty (show op)


data OpDefined x = OpDefined x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpDefined x)
instance Hashable  x => Hashable  (OpDefined x)
instance ToJSON    x => ToJSON    (OpDefined x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDefined x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpDefined x) where
    typeOf (OpDefined x) = do
        TypeFunction a _ <- typeOf x
        return (TypeSet a)
instance EvaluateOp OpDefined where
    evaluateOp (OpDefined (ConstantAbstract (AbsLitFunction xs))) =
        return $ ConstantAbstract $ AbsLitSet $ sortNub $ map fst xs
    evaluateOp op = na $ "evaluateOp{OpDefined}:" <++> pretty (show op)


data OpRange x = OpRange x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpRange x)
instance Hashable  x => Hashable  (OpRange x)
instance ToJSON    x => ToJSON    (OpRange x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRange x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpRange x) where
    typeOf (OpRange x) = do
        TypeFunction _ a <- typeOf x
        return (TypeSet a)
instance EvaluateOp OpRange where
    evaluateOp (OpRange (ConstantAbstract (AbsLitFunction xs))) =
        return (ConstantAbstract (AbsLitSet (sortNub (map snd xs))))
    evaluateOp op = na $ "evaluateOp{OpRange}:" <++> pretty (show op)


data OpAllDiff x = OpAllDiff x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)
instance Serialize x => Serialize (OpAllDiff x)
instance Hashable  x => Hashable  (OpAllDiff x)
instance ToJSON    x => ToJSON    (OpAllDiff x) where toJSON = JSON.genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpAllDiff x) where parseJSON = JSON.genericParseJSON jsonOptions
instance TypeOf x => TypeOf (OpAllDiff x) where
    typeOf (OpAllDiff x) = do
        TypeMatrix TypeInt TypeInt <- typeOf x
        return TypeBool
instance EvaluateOp OpAllDiff where
    evaluateOp op = na $ "evaluateOp{OpAllDiff}:" <++> pretty (show op)


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

sameToSameToBool :: (MonadFail m, TypeOf a, Pretty a) => a -> a -> m Type
sameToSameToBool a b = do
    tyA <- typeOf a
    tyB <- typeOf b
    if tyA `typeUnify` tyB
        then return TypeBool
        else fail $ "sameToSameToBool" <+> pretty a <+> "~~" <+> pretty b

sameToSameToSame :: (MonadFail m, TypeOf a, Pretty a) => a -> a -> m Type
sameToSameToSame a b = do
    tyA <- typeOf a
    tyB <- typeOf b
    if tyA `typeUnify` tyB
        then return (mostDefined [tyA,tyB])
        else fail $ "sameToSameToSame" <+> pretty a <+> "~~" <+> pretty b


mkBinOp :: OperatorContainer x => Text -> x -> x -> x
mkBinOp op a b =
    case textToLexeme op of
        Nothing -> bug ("Unknown binary operator:" <+> pretty op)
        Just l  ->
            let
                f = case l of
                    L_Plus      -> \ x y -> injectOp $ MkOpPlus      $ OpPlus       [x,y]
                    L_Minus     -> \ x y -> injectOp $ MkOpMinus     $ OpMinus       x y
                    L_Times     -> \ x y -> injectOp $ MkOpTimes     $ OpTimes      [x,y]
                    L_Div       -> \ x y -> injectOp $ MkOpDiv       $ OpDiv         x y
                    L_Mod       -> \ x y -> injectOp $ MkOpMod       $ OpMod         x y
                    L_Pow       -> \ x y -> injectOp $ MkOpPow       $ OpPow         x y
                    L_Eq        -> \ x y -> injectOp $ MkOpEq        $ OpEq          x y
                    L_Neq       -> \ x y -> injectOp $ MkOpNeq       $ OpNeq         x y
                    L_Lt        -> \ x y -> injectOp $ MkOpLt        $ OpLt          x y
                    L_Leq       -> \ x y -> injectOp $ MkOpLeq       $ OpLeq         x y
                    L_Gt        -> \ x y -> injectOp $ MkOpGt        $ OpGt          x y
                    L_Geq       -> \ x y -> injectOp $ MkOpGeq       $ OpGeq         x y
                    L_in        -> \ x y -> injectOp $ MkOpIn        $ OpIn          x y
                    L_And       -> \ x y -> injectOp $ MkOpAnd       $ OpAnd        [x,y]
                    L_Or        -> \ x y -> injectOp $ MkOpOr        $ OpOr         [x,y]
                    L_Imply     -> \ x y -> injectOp $ MkOpImply     $ OpImply       x y
                    L_Iff       -> \ x y -> injectOp $ MkOpEq        $ OpEq          x y
                    L_subset    -> \ x y -> injectOp $ MkOpSubset    $ OpSubset      x y
                    L_subsetEq  -> \ x y -> injectOp $ MkOpSubsetEq  $ OpSubsetEq    x y
                    L_supset    -> \ x y -> injectOp $ MkOpSupset    $ OpSupset      x y
                    L_supsetEq  -> \ x y -> injectOp $ MkOpSupsetEq  $ OpSupsetEq    x y
                    L_intersect -> \ x y -> injectOp $ MkOpIntersect $ OpIntersect   x y
                    L_union     -> \ x y -> injectOp $ MkOpUnion     $ OpUnion       x y
                    L_LexLt     -> \ x y -> injectOp $ MkOpLexLt     $ OpLexLt       x y
                    L_LexLeq    -> \ x y -> injectOp $ MkOpLexLeq    $ OpLexLeq      x y
                    _ -> bug ("Unknown lexeme for binary operator:" <+> pretty (show l))
            in
                f a b

mkOp :: (OperatorContainer x, ReferenceContainer x) => Text -> [x] -> x
mkOp op xs =
    case textToLexeme op of
        Nothing -> case op of
            "and"       -> injectOp (MkOpAnd       (OpAnd       xs))
            "or"        -> injectOp (MkOpOr        (OpOr        xs))
            "sum"       -> injectOp (MkOpPlus      (OpPlus      xs))
            "not"       -> injectOp (MkOpNot       (OpNot       (headNote "not takes a single argument"       xs)))
            "negate"    -> injectOp (MkOpNegate    (OpNegate    (headNote "negate takes a single argument"    xs)))
            "twoBars"   -> injectOp (MkOpTwoBars   (OpTwoBars   (headNote "twoBars takes a single argument"   xs)))
            "factorial" -> injectOp (MkOpFactorial (OpFactorial (headNote "factorial takes a single argument" xs)))
            _     -> bug ("Unknown operator:" <+> vcat [pretty op, pretty $ show $ textToLexeme op])
            -- _     -> opFunctionImage (fromName (Name op)) xs
        Just l -> case l of
            L_toInt    -> injectOp $ MkOpToInt    $ OpToInt    (headNote "toInt takes a single argument."    xs)
            L_defined  -> injectOp $ MkOpDefined  $ OpDefined  (headNote "defined takes a single argument."  xs)
            L_range    -> injectOp $ MkOpRange    $ OpRange    (headNote "range takes a single argument."    xs)
            L_allDiff  -> injectOp $ MkOpAllDiff  $ OpAllDiff  (headNote "allDiff takes a single argument."  xs)
            L_dontCare -> injectOp $ MkOpDontCare $ OpDontCare (headNote "dontCare takes a single argument." xs)
            L_flatten  -> injectOp $ MkOpFlatten  $ OpFlatten  (headNote "flatten takes a single argument."  xs)
            L_toSet    -> injectOp $ MkOpToSet    $ OpToSet    (headNote "toSet takes a single argument."    xs)
            L_toMSet   -> injectOp $ MkOpToMSet   $ OpToMSet   (headNote "toMSet takes a single argument."   xs)
            L_max      -> injectOp $ MkOpMax      $ OpMax xs
            L_min      -> injectOp $ MkOpMin      $ OpMin xs
            L_preImage -> injectOp $ MkOpPreImage $ OpPreImage (atNote "preImage 1" xs 0) (atNote "preImage 2" xs 1)
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

    , LIdentifier "and"
    , LIdentifier "or"
    , LIdentifier "sum"

    ]


-- this probably shouldn't be in this domain, should refactor later
valuesInIntDomain :: MonadFail m => [Range Constant] -> m [Int]
valuesInIntDomain ranges =
    if isFinite
        then return allValues
        else fail $ "Expected finite integer ranges, but got:" <+> prettyList id "," ranges

    where

        allRanges :: [Maybe [Int]]
        allRanges =
            [ vals
            | r <- ranges
            , let vals = case r of
                    RangeSingle (ConstantInt x) -> return [x]
                    RangeBounded (ConstantInt l) (ConstantInt u) -> return [l..u]
                    _ -> Nothing
            ]

        isFinite :: Bool
        isFinite = Nothing `notElem` allRanges

        allValues :: [Int]
        allValues = nub $ concat $ catMaybes allRanges


boolsOut :: MonadFail m => Constant -> m [Bool]
boolsOut (ConstantAbstract (AbsLitMatrix _ cs)) = concat <$> mapM boolsOut cs
boolsOut (ConstantAbstract (AbsLitList     cs)) = concat <$> mapM boolsOut cs
boolsOut b = return <$> boolOut b

intsOut :: MonadFail m => Constant -> m [Int]
intsOut (ConstantAbstract (AbsLitMatrix _ cs)) = concat <$> mapM intsOut cs
intsOut (ConstantAbstract (AbsLitList     cs)) = concat <$> mapM intsOut cs
intsOut b = return <$> intOut b

raiseTypeError :: MonadFail m => Pretty a => a -> m b
raiseTypeError p = fail ("Type error in" <+> pretty p)
