{-# LANGUAGE ViewPatterns #-}

module Conjure.Language.Expression.Op.Internal.Common
    ( module X

    , EvaluateOp(..)
    , SimplifyOp(..)
    , BinaryOperator(..)
    , boolsOut, intsOut

    , prettyPrecBinOp
    , Fixity(..), operators, functionals
    , EssenceOperatorParsingDescr(..)

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
import Conjure.Language.Lexer as X ( Lexeme(..), textToLexeme, lexemeFace )
import Conjure.Language.NameGen ( NameGen )


-- | Assume: the input is already normalised.
--   Make sure the output is normalised.
class EvaluateOp op where
    evaluateOp :: 
        MonadFail m =>
        NameGen m =>
        (?typeCheckerMode :: TypeCheckerMode) =>
        op Constant -> m Constant

class SimplifyOp op x where
    simplifyOp ::
        MonadFail m =>
        Eq x =>
        Num x =>
        ExpressionLike x =>
        op x ->     -- the input
        m x         -- the simplified output (or failure if it cannot be simplified)

class BinaryOperator op where
    opLexeme :: proxy op -> Lexeme

-- | just the operator not the arguments
opPretty :: BinaryOperator op => proxy op -> Doc
opPretty = lexemeFace . opLexeme

opFixityPrec :: BinaryOperator op => proxy op -> (Fixity, Int)
opFixityPrec op =
    case [ (f,p) | (Binary l f, p) <- operators, l == opLexeme op ] of
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
            FRight -> parensIf (envPrec > prec) $ fsep [ prettyPrec (prec+1) a
                                                       , opPretty op
                                                       , prettyPrec  prec    b
                                                       ]

intToInt :: (MonadFail m, TypeOf a, Pretty p, ?typeCheckerMode :: TypeCheckerMode) => p -> a -> m Type
intToInt p a = do
    tya <- typeOf a
    case tya of
        TypeInt t -> return (TypeInt t)
        _         -> fail $ vcat
            [ "When type checking:" <+> pretty p
            , "Argument expected to be an int, but it is:" <++> pretty tya
            ]


intToIntToInt :: (MonadFail m, TypeOf a, Pretty p, ?typeCheckerMode :: TypeCheckerMode) => p -> a -> a -> m Type
intToIntToInt p a b = do
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        (TypeInt{}, TypeInt{}) ->
            if typeUnify tya tyb
                then return $ mostDefined [tya, tyb]
                else fail $ vcat
                        [ "When type checking:" <+> pretty p
                        , "Types do not unify:" <++> pretty tya 
                        ]
        (_, TypeInt{})         -> fail $ vcat
            [ "When type checking:" <+> pretty p
            ,  "First argument expected to be an int, but it is:" <++> pretty tya
            ]
        _                      -> fail $ vcat
            [ "When type checking:" <+> pretty p
            , "Second argument expected to be an int, but it is:" <++> pretty tyb
            ]


boolToBoolToBool :: (MonadFail m, TypeOf a, Pretty p, ?typeCheckerMode :: TypeCheckerMode) => p -> a -> a -> m Type
boolToBoolToBool p a b = do
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        (TypeBool, TypeBool) -> return TypeBool
        (_, TypeBool)        -> fail $ vcat
            [ "When type checking:" <+> pretty p
            ,  "First argument expected to be a bool, but it is:" <++> pretty tya
            ]
        _                    -> fail $ vcat
            [ "When type checking:" <+> pretty p
            , "Second argument expected to be a bool, but it is:" <++> pretty tyb
            ]


-- if acceptableTypes is null, use checkType
-- if acceptableTypes is not null, either one of these is true or checkType
sameToSameToBool
    :: ( MonadFail m, ?typeCheckerMode :: TypeCheckerMode
       , TypeOf a, Pretty a, Pretty p
       ) => p -> a -> a -> [Type] -> (Type -> Bool) -> m Type
sameToSameToBool p a b acceptableTypes checkType = do
    tyA <- typeOf a
    tyB <- typeOf b
    let tyAB = mostDefined [tyA,tyB]
    let allowed = if null acceptableTypes
                    then checkType tyAB
                    else checkType tyAB || any (typeUnify tyAB) acceptableTypes
    case (tyA `typeUnify` tyB, allowed) of
        (True, True) -> return TypeBool
        (False, _) -> fail $ vcat
            [ "When type checking:" <+> pretty p
            , "Cannot unify the types of the following."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]
        (_, False) -> fail $ vcat
            [ "When type checking:" <+> pretty p
            , "Arguments have unsupported types."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]

-- See sameToSameToBool
sameToSameToSame
    :: ( MonadFail m
       , ?typeCheckerMode :: TypeCheckerMode
       , TypeOf a, Pretty a, Pretty p
       ) => p -> a -> a -> [Type] -> (Type -> Bool) -> m Type
sameToSameToSame p a b acceptableTypes checkType = do
    tyA <- typeOf a
    tyB <- typeOf b
    let tyAB = mostDefined [tyA,tyB]
    let allowed = if null acceptableTypes
                    then checkType tyAB
                    else checkType tyAB || any (typeUnify tyAB) acceptableTypes
    case (tyA `typeUnify` tyB, allowed) of
        (True, True) -> return tyAB
        (False, _) -> fail $ vcat
            [ "When type checking:" <+> pretty p
            , "Cannot unify the types of the following."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]
        (_, False) -> fail $ vcat
            [ "When type checking:" <+> pretty p
            , "Arguments have unsupported types."
            , "lhs        :" <+> pretty a
            , "type of lhs:" <+> pretty tyA
            , "rhs        :" <+> pretty b
            , "type of rhs:" <+> pretty tyB
            ]


data Fixity = FNone | FLeft | FRight
    deriving Show

data EssenceOperatorParsingDescr = Binary Lexeme Fixity | UnaryPrefix Lexeme

operators :: [(EssenceOperatorParsingDescr, Int)]
operators =
    [ ( Binary L_Plus         FLeft   ,  600 )
    , ( Binary L_Minus        FLeft   ,  600 )
    , ( Binary L_Times        FLeft   ,  700 )
    , ( Binary L_Div          FLeft   ,  700 )
    , ( Binary L_Mod          FLeft   ,  700 )
    , ( Binary L_Pow          FRight  , 2001 )
    , ( Binary L_Lt           FNone   ,  400 )
    , ( Binary L_Leq          FNone   ,  400 )
    , ( Binary L_Gt           FNone   ,  400 )
    , ( Binary L_Geq          FNone   ,  400 )
    , ( Binary L_Neq          FNone   ,  400 )
    , ( Binary L_Eq           FNone   ,  400 )
    , ( Binary L_Or           FLeft   ,  110 )
    , ( Binary L_And          FLeft   ,  120 )
    , ( Binary L_Imply        FNone   ,   50 )
    , ( Binary L_Iff          FNone   ,   50 )
    , ( Binary L_union        FLeft   ,  600 )
    , ( Binary L_intersect    FLeft   ,  700 )
    , ( Binary L_subset       FNone   ,  400 )
    , ( Binary L_subsetEq     FNone   ,  400 )
    , ( Binary L_supset       FNone   ,  400 )
    , ( Binary L_supsetEq     FNone   ,  400 )
    , ( Binary L_subsequence  FNone   ,  400 )
    , ( Binary L_substring    FNone   ,  400 )
    , ( Binary L_in           FNone   ,  550 )
    , ( Binary L_HasRepr      FNone   ,   10 )
    , ( Binary L_HasType      FNone   ,   10 )
    , ( Binary L_HasDomain    FNone   ,   10 )
    , ( Binary L_LexLt        FNone   ,  400 )
    , ( Binary L_LexLeq       FNone   ,  400 )
    , ( Binary L_LexGt        FNone   ,  400 )
    , ( Binary L_LexGeq       FNone   ,  400 )
    , ( Binary L_DotLt        FNone   ,  400 )
    , ( Binary L_DotLeq       FNone   ,  400 )
    , ( Binary L_DotGt        FNone   ,  400 )
    , ( Binary L_DotGeq       FNone   ,  400 )
    , ( Binary L_TildeLt      FNone   ,  400 )
    , ( Binary L_TildeLeq     FNone   ,  400 )
    , ( Binary L_TildeGt      FNone   ,  400 )
    , ( Binary L_TildeGeq     FNone   ,  400 )
    , ( UnaryPrefix L_Minus           , 2000 )
    , ( UnaryPrefix L_ExclamationMark , 2000 )
    ]

functionals :: [Lexeme]
functionals =
    [ L_toInt
    , L_min
    , L_max
    , L_allDiff
    , L_alldifferent_except
    , L_catchUndef
    , L_dontCare
    , L_hist
    , L_factorial

    , L_toSet
    , L_toMSet
    , L_toRelation
    , L_defined
    , L_range
    , L_restrict
    , L_image
    , L_imageSet
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
    , L_concatenate
    , L_normIndices
    , L_indices
    , L_inverse

    , L_transform
    , L_true

    , LIdentifier "and"
    , LIdentifier "or"
    , LIdentifier "sum"
    , LIdentifier "product"
    , LIdentifier "xor"

    , L_active

    , L_pred
    , L_succ

    , L_powerSet

    ]


boolsOut :: MonadFail m => Constant -> m [Bool]
boolsOut (viewConstantMatrix -> Just (_, cs)) = concat <$> mapM boolsOut cs
boolsOut b = return <$> boolOut b

intsOut :: MonadFail m => Doc -> Constant -> m [Integer]
intsOut doc (viewConstantMatrix -> Just (_, cs)) = concat <$> mapM (intsOut doc) cs
intsOut doc (viewConstantSet -> Just cs) = concat <$> mapM (intsOut doc) cs
intsOut doc (viewConstantMSet -> Just cs) = concat <$> mapM (intsOut doc) cs
intsOut doc b = return <$> intOut ("intsOut" <+> doc) b

raiseTypeError :: MonadFail m => Pretty a => a -> m b
raiseTypeError p = fail ("Type error in" <+> pretty p)
