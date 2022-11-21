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

