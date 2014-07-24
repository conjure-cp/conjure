module Conjure.Language.ModelDiff
    ( modelDiff
    , modelDiffIO
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition


modelDiffIO :: Model -> Model -> IO ()
modelDiffIO m1 m2 =
    case modelDiff m1 m2 of
        Nothing -> return ()
        Just msg -> userErr $ sep ["Files differ, specifically:", msg]


-- | returns `Just msg` if the models are different, msg being an explanation of what's diff.
--   returns `Nothing` if the models are the same.
modelDiff :: Model -> Model -> Maybe Doc
modelDiff _ _ = Just "Just Plain Wrong (TM)"

