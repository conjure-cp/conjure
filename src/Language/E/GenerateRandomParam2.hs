{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.GenerateRandomParam2(generateParam) where

import Language.E
import Language.E.Pipeline.ConjureAll ( conjureWithMode )
import Language.E.Pipeline.Driver ( driverConjureSingle )
import Language.E.Pipeline.ReadIn(readSpecFromFile)
import Language.E.PrepareParam(prepareParamSpecification)
import Language.E.Up(translateSolution')

import Conjure.Mode

import Prelude hiding ( FilePath, reverse )
import Shelly

import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import qualified System.Directory    as FP
import qualified System.FilePath     as FP

type EssenceParam = Spec
type Essence = Spec


getSpecMaybe :: FP.FilePath -> IO (Maybe Spec)
getSpecMaybe filepath  = getSpecMaybe' filepath =<<  FP.doesFileExist filepath
    where
    getSpecMaybe' fp True = liftM Just (readSpecFromFile fp)
    getSpecMaybe' _ False = return Nothing


generateParam  :: RulesDB -> Essence -> FP.FilePath -> Maybe String -> IO EssenceParam
generateParam (ruleReprs,ruleRefns) essence intermediateDir prefix = do

    paramEssenceOld <- getSpecMaybe param_gen

    putStrLn $ "Creating the intermediate directory(if needed): " ++ intermediateDir
    FP.createDirectoryIfMissing True intermediateDir

    putStrLn  "Creating Essence specification of the param"
    driverConjureSingle False True
        param_gen
        $ runCompE "generateParamSolve" (prepareParamSpecification essence)

    paramEssence <- readSpecFromFile param_gen

    let runConjure (Just oldSpec) | oldSpec == paramEssence  = 
            putStrLn "NOT Running Conjure compact on created specification\n" 

        runConjure _ = do
            seed <- getStdGen
            putStrLn "Running Conjure compact on created specification\n"
            driverConjureSingle True True
                param_eprime
                (conjureWithMode
                    seed Nothing Nothing
                    (ConjureModeWithFlags (ModeSingleOutput ModeCompact param_gen param_eprime) M.empty def def)
                    ruleReprs ruleRefns paramEssence)

    _ <- runConjure paramEssenceOld

    _ <- shelly $ verbosely $ do
        echo "Running Savilerow"
        _ <- savilerow (T.pack param_eprime) (T.pack param_minion) (T.pack param_esolution) Nothing
        return ()

    putStrLn "\nRunning translateSolution"
    translateSolution'
        param_gen
        Nothing
        param_eprime
        Nothing
        param_esolution

    where
    basename        = FP.takeBaseName intermediateDir
    basename2       = fromMaybe (FP.takeBaseName intermediateDir) prefix
    param_gen       = intermediateDir FP.</> (basename ++ ".essence")
    param_eprime    = intermediateDir FP.</> (basename ++ ".eprime")
    param_minion    = intermediateDir FP.</> (basename2 ++ ".eprime.minion")
    param_esolution = intermediateDir FP.</> (basename2 ++ ".eprime.solution")


savilerow :: T.Text -> T.Text -> T.Text -> Maybe T.Text -> Sh T.Text
savilerow in_eprime out_minion out_solution in_param= run
                          "savilerow" $
                          ["-in-eprime",    in_eprime
                          ,"-out-minion",   out_minion
                          ,"-out-solution", out_solution
                          ,"-runsolver"
                          ,"-minion-options", "-randomiseorder"
                          ] ++ handleParam  in_param

    where
    handleParam Nothing = []
    handleParam (Just param) = ["-in-param",param]


_test :: IO EssenceParam
_test = do
    db <- decodeFromFile "/Users/bilalh/.cabal/bin/conjure.rulesdb"
    sp <- readSpecFromFile "/Users/bilalh/CS/conjure/test/generateParams/set-all.essence"
    generateParam db sp "intermediate" Nothing

