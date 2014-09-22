module Conjure.UI.Model where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty


data ModelGen = ModelGen
    { originalEssence :: Model
    , history :: [ModelInfo]
    }

initialise :: Model -> ModelGen
initialise m = ModelGen m []

nextModel :: ModelGen -> IO (Maybe (Model, ModelGen))
nextModel (ModelGen m []) = return (Just ( languageEprime (oneSuchThat m)
                                         , ModelGen m [mInfo m]
                                         ))
nextModel _ = return Nothing

outputAllModels :: FilePath -> Int -> ModelGen -> IO ()
outputAllModels dir i gen = do
    createDirectoryIfMissing True dir
    may <- nextModel gen
    case may of
        Nothing -> return ()
        Just (eprime,gen') -> do
            let filename = dir </> "model" ++ show i ++ ".eprime"
            writeFile filename (renderWide eprime)
            outputAllModels dir (i+1) gen'

