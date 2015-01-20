module Conjure.Language.CategoryOf
    ( Category(..)
    , categoryOf
    , categoryChecking
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


data Category = CatBottom | CatConstant | CatParameter | CatQuantified | CatDecision
    deriving (Eq, Ord, Show)

instance Pretty Category where
    pretty CatBottom = "_|_"
    pretty CatConstant = "constant"
    pretty CatParameter = "parameter"
    pretty CatQuantified = "quantified"
    pretty CatDecision = "decision"

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
     categoryOf (GenDomainNoRepr  _ x) = categoryOf x
     categoryOf (GenDomainHasRepr _ x) = categoryOf x
     categoryOf (GenInExpr        _ x) = categoryOf x

instance CategoryOf x => CategoryOf (Domain r x) where
    categoryOf dom = maximum (CatBottom : toList (fmap categoryOf dom))

instance CategoryOf FindOrGiven where
    categoryOf Find = CatDecision
    categoryOf Given = CatParameter
    categoryOf Quantified = CatQuantified


-- | Category checking to check if domains have anything >CatParameter in them.
--   Run only after name resolution.
categoryChecking :: MonadFail m => Model -> m Model
categoryChecking m = do
    errors1 <- liftM concat $ forM (mStatements m) $ \ st -> case st of
        Declaration (FindOrGiven _forg name domain) -> do
            let cat = categoryOf domain
            return [(domain, (name, cat)) | cat > CatParameter]
        _ -> return []
    errors2 <- liftM concat $ forM (universeBi (mStatements m) :: [Domain () Expression]) $ \ domain -> do
        let cat = categoryOf domain
        return [ (domain, cat)
               | cat > CatQuantified
               , not (domain `elem` map fst errors1)        -- only if this is a new error
               ]

    if null errors1 && null errors2
        then return m
        else fail $ vcat
            $  [ "Category checking failed." ]
            ++ concat [ [ "The domain   :" <+> pretty domain
                        , "Its category :" <+> pretty cat
                        , "In the definition of:" <+> pretty name
                        , ""
                        ]
                      | (domain, (name, cat)) <- errors1
                      ]
            ++ concat [ [ "The domain   :" <+> pretty domain
                        , "Its category :" <+> pretty cat
                        , ""
                        ]
                      | (domain, cat) <- errors2
                      ]
