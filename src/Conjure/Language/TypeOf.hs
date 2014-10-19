module Conjure.Language.TypeOf where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Type


class TypeOf a where
    typeOf :: MonadFail m => a -> m Type

homoType :: [Type] -> Type
homoType [] = userErr "empty collection, what's the type?"
homoType (x:xs) =
    if all (==x) xs
        then x
        else userErr "not a homoType"
