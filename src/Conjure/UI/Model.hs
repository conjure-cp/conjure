{-# LANGUAGE FlexibleContexts #-}

module Conjure.UI.Model where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.ModelStats ( declarations )
import Conjure.Representations

import Data.Generics.Uniplate.Data ( transformBiM )


data ModelGen = ModelGen
    { originalEssence :: Model
    , history :: [ModelInfo]
    }

initialise :: Model -> ModelGen
initialise m = ModelGen m []

-- | repeatedly call `nextModel` to generate all models
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

-- | given a `ModelGen`, which contains info about previously generated models,
--   generate the next model.
nextModel :: ModelGen -> IO (Maybe (Model, ModelGen))
nextModel (ModelGen _ (_:_)) = return Nothing                       -- only generate one model and stop. for now.
nextModel (ModelGen essence pastInfos) = do
    meprime <- genNextModel essence pastInfos                       -- the workhorse
    case meprime of
        Nothing -> return Nothing                                   -- no more models to be generated
        Just eprime -> do
            let info = mInfo eprime
            return (Just ( languageEprime (oneSuchThat eprime)      -- return the newly generated model
                         , ModelGen essence (info:pastInfos)        -- and add its "info" to the log
                         ))

-- | given an initial essence model,
--   and a list of ModelInfo's describing previously generated models,
--   generate the next model
--   or return Nothing if no other model can be generated
genNextModel :: Model -> [ModelInfo] -> IO (Maybe Model)
genNextModel initialEssence _pastInfos = do

    putStrLn "" >> putStrLn ""

    let decls = declarations initialEssence

    forM_ decls $ \ (n,d) -> do
        let ds = reprOptions d
        print $ vcat $ (pretty n <> ":" <+> pretty d)
                     : map (nest 4 . pretty) ds

    putStrLn "" >> putStrLn ""

    let
        f :: (MonadState St m, MonadIO m) => Expression -> m Expression
        f (Reference nm) =
            case lookup nm decls of
                Nothing -> error $ "what's this a reference to? " ++ show nm
                Just inpDom -> do
                    let domOpts = reprOptions inpDom
                    let domSelected = head domOpts
                    liftIO $ do
                        print $ "Options for the domain of" <+> pretty nm
                        mapM_ (print . nest 4 . pretty) domOpts
                        print $ "    Picking:" <+> pretty domSelected
                    modify $ addReprToSt nm domSelected
                    return (Reference nm)
        f x = do
            gets nbExpression >>= \ nb -> liftIO $ print $ "--" <+> pretty nb <> ":" <+> pretty (show x)
            -- gets ascendants   >>= \ xs -> liftIO $ print $ vcat [ "----" <+> pretty (show i) | i <- xs ]
            -- liftIO $ putStrLn ""
            modify $ \ st -> st { nbExpression = 1 + nbExpression st
                                , ascendants   = x : ascendants st
                                }
            return x

    (statements', st) <- runStateT (transformBiM f (mStatements initialEssence)) (St 1 [] def)
    let model = initialEssence { mStatements = statements'
                               , mInfo = currInfo st
                               }
    return (Just model)


data St = St
    { nbExpression :: !Int
    , ascendants :: [Expression]
    , currInfo :: !ModelInfo
    }

addReprToSt :: Name -> Domain HasRepresentation Expression -> St -> St
addReprToSt nm dom st = st { currInfo = addReprToInfo (currInfo st) }
    where addReprToInfo i = i { miRepresentations = (nm, dom) : miRepresentations i }



