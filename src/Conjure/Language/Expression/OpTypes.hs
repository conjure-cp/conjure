module Conjure.Language.Expression.OpTypes  where
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

constInt :: TypeMapping2
constInt = constantTyped2 (TypeInt TagInt)

constBool :: TypeMapping2
constBool = constantTyped2 TypeBool

same :: (Type -> Type)
same = id

