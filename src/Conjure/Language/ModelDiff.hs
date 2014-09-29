module Conjure.Language.ModelDiff
    ( modelDiff
    , modelDiffIO
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Pretty


modelDiffIO :: Model -> Model -> IO ()
modelDiffIO m1 m2 =
    case modelDiff m1 m2 of
        Nothing -> return ()
        Just msg -> userErr $ sep ["Files differ, specifically:", msg]


-- | returns `Just msg` if the models are different, msg being an explanation of what's diff.
--   returns `Nothing` if the models are the same.
modelDiff :: Model -> Model -> Maybe Doc
modelDiff m1 m2 =
    let
        m1Extra = mStatements m1 \\ mStatements m2
        m2Extra = mStatements m2 \\ mStatements m1
    in
        if null m1Extra && null m2Extra
            then Nothing
            else Just $ vcat $ concat
                [ [ "These models seem to be different." ]
                , [ hang "Only in the 1st:" 8 (vcat (map pretty m1Extra)) | not (null m1Extra) ]
                , [ hang "Only in the 2nd:" 8 (vcat (map pretty m2Extra)) | not (null m2Extra) ]
                ]

