module Conjure.Language.CategoryOf
    ( Category(..)
    , categoryOf
    , categoryChecking
    , initInfo_Lettings
    ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
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
    -- TODO: the following should check for which variable we quantify over
    categoryOf x@Comprehension{} = maximum $ filter (CatQuantified/=) $ CatBottom : map categoryOf (children x)
    categoryOf x                 = maximum                            $ CatBottom : map categoryOf (children x)

instance CategoryOf ReferenceTo where
    categoryOf (Alias                x) = categoryOf x
    categoryOf (InComprehension      _) = CatQuantified
    categoryOf (DeclNoRepr  forg _ _ _) = categoryOf forg
    categoryOf (DeclHasRepr forg _ _  ) = categoryOf forg
    categoryOf RecordField{}            = CatBottom
    categoryOf VariantField{}           = CatBottom
    categoryOf FrameUpdateVar{}         = CatBottom

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
    categoryOf CutFind = CatDecision
    categoryOf LocalFind = CatDecision


-- | Category checking to check if domains have anything >CatParameter in them.
--   Run only after name resolution.
categoryChecking :: (MonadFail m, MonadUserError m) => Model -> m Model
categoryChecking m = do
    errors1 <- fmap concat $ forM (mStatements m) $ \ st -> case st of
        Declaration (FindOrGiven _forg name domain) -> do
            let cat = categoryOf domain
            return [(domain, (name, cat)) | cat > CatParameter]
        _ -> return []
    errors2 <- fmap concat $ forM (universeBi (mStatements m) :: [Domain () Expression]) $ \ domain -> do
        let cat = categoryOf domain
        return [ (domain, cat)
               | cat > CatQuantified
               , domain `notElem` map fst errors1        -- only if this is a new error
               ]

    if null errors1 && null errors2
        then return m
        else userErr1 $ vcat
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

initInfo_Lettings :: Model -> Model
initInfo_Lettings model = model { mInfo = info }
    where
        info = (mInfo model)
            { miLettings = [ (nm,x) | Declaration (Letting nm x) <- mStatements model
                                    , categoryOf x <= CatParameter
                                    ]
            }
