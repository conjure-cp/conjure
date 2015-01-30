{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Common
    ( module X

    , EvaluateOp(..), BinaryOperator(..)
    , boolsOut, intsOut

    , valuesInIntDomain

    , prettyPrecBinOp
    , Fixity(..), operators, functionals

    , raiseTypeError

    , intToInt
    , intToIntToInt
    , boolToBoolToBool
    , sameToSameToBool
    , sameToSameToSame

    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name as X
import Conjure.Language.AbstractLiteral as X
import Conjure.Language.Constant as X
import Conjure.Language.Type as X
import Conjure.Language.Domain as X
import Conjure.Language.TypeOf as X
import Conjure.Language.Pretty as X
import Conjure.Language.AdHoc as X
import Conjure.Language.Lexer  as X ( Lexeme(..), textToLexeme, lexemeFace )


class EvaluateOp op where
    evaluateOp :: MonadFail m => op Constant -> m Constant

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
    , L_restrict
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


-- this probably shouldn't be in this module, should refactor later
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
boolsOut b = return <$> boolOut b

intsOut :: MonadFail m => Constant -> m [Int]
intsOut (ConstantAbstract (AbsLitMatrix _ cs)) = concat <$> mapM intsOut cs
intsOut b = return <$> intOut b

raiseTypeError :: MonadFail m => Pretty a => a -> m b
raiseTypeError p = fail ("Type error in" <+> pretty p)
