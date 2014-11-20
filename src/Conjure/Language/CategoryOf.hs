module Conjure.Language.CategoryOf ( Category(..), categoryOf ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain


data Category = CatBottom | CatConstant | CatParameter | CatQuantified | CatDecision
    deriving (Eq, Ord, Show)

class CategoryOf a where
    categoryOf :: a -> Category

instance CategoryOf Expression where
    categoryOf (Reference _ (Just ref)) = categoryOf ref
    categoryOf x = maximum (CatBottom : map categoryOf (children x))

instance CategoryOf ReferenceTo where
    categoryOf (Alias              x) = categoryOf x
    categoryOf (InComprehension    _) = CatQuantified
    categoryOf (DeclNoRepr  forg _ _) = categoryOf forg
    categoryOf (DeclHasRepr forg _ _) = categoryOf forg

instance CategoryOf Generator where
     categoryOf (GenDomain _ x) = categoryOf x
     categoryOf (GenInExpr _ x) = categoryOf x

instance CategoryOf x => CategoryOf (Domain r x) where
    categoryOf dom = maximum (CatBottom : toList (fmap categoryOf dom))

instance CategoryOf FindOrGiven where
    categoryOf Find = CatDecision
    categoryOf Given = CatParameter
