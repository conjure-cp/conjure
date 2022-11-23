module Conjure.Language.Expression.OpTypes where
import Conjure.Language.Lexemes (Lexeme)
import Conjure.Language.Expression.Op.Internal.Common (Type (..))
import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common (Lexeme(..), IntTag (TagInt), mostDefined)

type TypeMapping2 = Type -> Type -> Type

constantTyped2 :: Type -> TypeMapping2
constantTyped2 = const . const

rightArgDep :: (Type -> Type) -> TypeMapping2
rightArgDep f _ = f

leftArgDep :: (Type-> Type) -> TypeMapping2
leftArgDep f a _ = f a

binOpType :: Lexeme -> TypeMapping2
binOpType L_Plus = const id
binOpType L_Minus = const id
binOpType L_Times = const id
binOpType L_Div = constInt
binOpType L_Mod = constInt
binOpType L_Pow = constInt
binOpType L_Eq = constBool
binOpType L_Neq = constBool
binOpType L_Lt = constBool
binOpType L_Leq = constBool
binOpType L_Gt = constBool
binOpType L_Geq = constBool
binOpType L_in = constBool
binOpType L_And = constBool
binOpType L_Or = constBool
binOpType L_Imply = constBool
binOpType L_Iff = constBool -- b b b
binOpType L_subset = constBool -- set mset func rel
binOpType L_subsetEq = constBool -- ^^^
binOpType L_supset = constBool -- ^^^^
binOpType L_supsetEq = constBool -- ^^
binOpType L_subsequence = constBool -- seq - seq -bool
binOpType L_substring = constBool -- seq - seq -bool
binOpType L_intersect = const id
binOpType L_union = const id
binOpType L_LexLt = constBool
binOpType L_LexLeq = constBool
binOpType L_LexGt = constBool
binOpType L_LexGeq = constBool
binOpType L_DotLt = constBool -- same same bool
binOpType L_DotLeq = constBool
binOpType L_DotGt = constBool
binOpType L_DotGeq = constBool
binOpType L_TildeLt = constBool
binOpType L_TildeLeq = constBool
binOpType L_TildeGt = constBool
binOpType L_TildeGeq = constBool
binOpType _ = constantTyped2 TypeAny

constInt :: TypeMapping2
constInt = constantTyped2 (TypeInt TagInt)

constBool :: TypeMapping2
constBool = constantTyped2 TypeBool

same :: (Type -> Type)
same = id

-- mkOp :: (Op x :< x, ReferenceContainer x, ExpressionLike x) => OpType -> [x] -> x
-- mkOp op xs = case op of
--   PrefixOp lex -> case lex of
--         L_ExclamationMark -> inject $ MkOpNot       $ OpNot     (arg xs 0 "not")
--         L_Minus    -> inject $ MkOpNegate    $ OpNegate  (arg xs 0 "negate")
--         _ -> bug $ "Unexpected Prefix operator :" <+> pretty (show lex)
--   FactorialOp -> inject $ MkOpFactorial    $ OpFactorial    (arg xs 0 "factorial")
--   TwoBarOp -> inject $ MkOpTwoBars   $ OpTwoBars (arg xs 0 "twoBars")
--   FunctionOp lex -> case lex of
--             L_fAnd       -> inject $ MkOpAnd       $ OpAnd     (arg xs 0 "and")
--             L_fOr       -> inject $ MkOpOr        $ OpOr      (arg xs 0 "or")
--             L_fXor       -> inject $ MkOpXor       $ OpXor     (arg xs 0 "xor")
--             L_Sum       -> inject $ MkOpSum       $ OpSum     (arg xs 0 "sum")
--             L_Product   -> inject $ MkOpProduct   $ OpProduct (arg xs 0 "product")
--             -- _     -> opImage (fromName (Name op)) xs
--             L_true         -> inject $ MkOpTrue         $ OpTrue         (arg xs 0 "true")
--             L_toInt        -> inject $ MkOpToInt        $ OpToInt        (arg xs 0 "toInt")
--             L_makeTable    -> inject $ MkOpMakeTable    $ OpMakeTable    (arg xs 0 "makeTable")
--             L_table        -> inject $ MkOpTable        $ OpTable        (arg xs 0 "table") (arg xs 1 "table")
--             L_gcc          -> inject $ MkOpGCC          $ OpGCC          (arg xs 0 "gcc") (arg xs 1 "gcc") (arg xs 2 "gcc")
--             L_atleast      -> inject $ MkOpAtLeast      $ OpAtLeast      (arg xs 0 "atleast") (arg xs 1 "atleast") (arg xs 2 "atleast")
--             L_atmost       -> inject $ MkOpAtMost       $ OpAtMost       (arg xs 0 "atmost" ) (arg xs 1 "atmost" ) (arg xs 2 "atmost" )
--             L_defined      -> inject $ MkOpDefined      $ OpDefined      (arg xs 0 "defined")
--             L_range        -> inject $ MkOpRange        $ OpRange        (arg xs 0 "range")
--             L_restrict     -> inject $ MkOpRestrict     $ OpRestrict     (arg xs 0 "restrict") (arg xs 1 "restrict")
--             L_allDiff      -> inject $ MkOpAllDiff      $ OpAllDiff      (arg xs 0 "allDiff")
--             L_alldifferent_except -> inject $ MkOpAllDiffExcept $ OpAllDiffExcept
--                                                                          (arg xs 0 "allDiffExcept")
--                                                                          (arg xs 1 "allDiffExcept")
--             L_catchUndef   -> inject $ MkOpCatchUndef   $ OpCatchUndef   (arg xs 0 "catchUndef")
--                                                                          (arg xs 1 "catchUndef")
--             L_dontCare     -> inject $ MkOpDontCare     $ OpDontCare     (arg xs 0 "dontCare")
--             L_toSet        -> inject $ MkOpToSet        $ OpToSet        False (arg xs 0 "toSet")
--             L_toMSet       -> inject $ MkOpToMSet       $ OpToMSet       (arg xs 0 "toMSet")
--             L_toRelation   -> inject $ MkOpToRelation   $ OpToRelation   (arg xs 0 "toRelation")
--             L_max          -> inject $ MkOpMax          $ OpMax          (arg xs 0 "max")
--             L_min          -> inject $ MkOpMin          $ OpMin          (arg xs 0 "min")
--             L_image        -> inject $ MkOpImage        $ OpImage        (arg xs 0 "image")
--                                                                          (arg xs 1 "image")
--             L_transform    -> inject $ MkOpTransform    $ OpTransform    (arg xs 0 "transform")
--                                                                          (arg xs 1 "transform")

--             L_imageSet     -> inject $ MkOpImageSet     $ OpImageSet     (arg xs 0 "imageSet")
--                                                                          (arg xs 1 "imageSet")
--             L_preImage     -> inject $ MkOpPreImage     $ OpPreImage     (arg xs 0 "preImage")
--                                                                          (arg xs 1 "preImage")
--             L_inverse      -> inject $ MkOpInverse      $ OpInverse      (arg xs 0 "inverse")
--                                                                          (arg xs 1 "inverse")
--             L_freq         -> inject $ MkOpFreq         $ OpFreq         (arg xs 0 "freq")
--                                                                          (arg xs 1 "freq")
--             L_hist         -> inject $ MkOpHist         $ OpHist         (arg xs 0 "hist")
--             L_parts        -> inject $ MkOpParts        $ OpParts        (arg xs 0 "parts")
--             L_together     -> inject $ MkOpTogether     $ OpTogether     (arg xs 0 "together")
--                                                                          (arg xs 1 "together")
--             L_apart        -> inject $ MkOpApart        $ OpApart        (arg xs 0 "apart")
--                                                                          (arg xs 1 "apart")
--             L_party        -> inject $ MkOpParty        $ OpParty        (arg xs 0 "party")
--                                                                          (arg xs 1 "party")
--             L_participants -> inject $ MkOpParticipants $ OpParticipants (arg xs 0 "participants")
--             L_active       -> inject $ MkOpActive       $ OpActive       (arg xs 0 "active")
--                                                                          (arg xs 1 "active" |> nameOut |> fromMaybe (bug "active"))
--             L_pred         -> inject $ MkOpPred         $ OpPred         (arg xs 0 "pred")
--             L_succ         -> inject $ MkOpSucc         $ OpSucc         (arg xs 0 "succ")
--             L_factorial    -> inject $ MkOpFactorial    $ OpFactorial    (arg xs 0 "factorial")
--             L_powerSet     -> inject $ MkOpPowerSet     $ OpPowerSet     (arg xs 0 "powerSet")
--             L_concatenate  -> inject $ MkOpFlatten      $ OpFlatten      (Just 1)
--                                                                          (arg xs 0 "concatenate")
--             L_flatten      ->
--                  case xs of
--                      [m]   -> inject $ MkOpFlatten      $ OpFlatten      Nothing  m
--                      [n,m] ->
--                           let n' = fromInteger $ fromMaybe (bug "The 1st argument of flatten has to be a constant integer.") (intOut "flatten" n)
--                           in  inject $ MkOpFlatten      $ OpFlatten      (Just n') m
--                      _     -> bug "flatten takes 1 or 2 arguments."
--             _ -> bug ("Unknown lexeme for function type operator:" <+> pretty (show lex))


-- arg :: [a] -> Int -> Doc -> a
-- arg xs n op =
--     case atMay xs n of
--         Nothing -> bug ("Missing argument" <+> pretty (n+1) <+> "for operator" <+> op)
--         Just v  -> v
