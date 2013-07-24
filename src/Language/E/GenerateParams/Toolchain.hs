{-# OPTIONS_GHC -fno-warn-type-defaults  #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.E.GenerateParams.Toolchain where


import Bug
import Language.E.Imports
import Language.E.GenerateParams.Typedefs

import Prelude hiding ( FilePath, reverse )
import Shelly

import qualified Data.Text.Lazy      as LT
import qualified System.FilePath     as FP

default (LT.Text)


type MinionTimeout  = Int
type OutputBaseName = String

runModelsWithParam :: OutputBaseName -> EprimeParamFP -> [EprimeFP] -> IO ()
runModelsWithParam outputBase param eprimes = do
   let eprimesVar = LT.pack .  unlines $ eprimes
   _ <- shelly  $ escaping False $ do
        mScriptDir <- get_env "PARAM_GEN_SCRIPTS"
        let scriptDir = fromMaybe (userErr "$PARAM_GEN_SCRIPTS not definded" ) mScriptDir
        echo "ScriptDir:"
        echo scriptDir

        setenv "GENERATED_OUTPUT_DIR"      (LT.pack $ "results-" ++ outputBase)
        setenv "STATS_OUTPUT_DIR"          (LT.pack $ "stats-"   ++ outputBase)
        setenv "PARAMS_TO_USE" (LT.pack param)
        setenv "MODELS_TO_USE" eprimesVar
        setenv "NO_MINION_STATS" "true"


        _ <- run "$PARAM_GEN_SCRIPTS/run/timeModel.sh" []
        return ()
   return ()

gatherData :: OutputBaseName -> IO ()
gatherData outputBase = do
   _ <- shelly  $ escaping False $ do
        mScriptDir <- get_env "PARAM_GEN_SCRIPTS"
        let scriptDir = fromMaybe (userErr "$PARAM_GEN_SCRIPTS not definded" ) mScriptDir
        echo "ScriptDir:"
        echo scriptDir

        setenv "GENERATED_OUTPUT_DIR"      (LT.pack $ "results-" ++ outputBase)
        setenv "STATS_OUTPUT_DIR"          (LT.pack $ "stats-"   ++ outputBase)
        setenv "NO_MINION_STATS" "true"

        _ <- run "$PARAM_GEN_SCRIPTS/db/gather_data.sh" []
        return ()
   return ()


runSavilerow :: MinionTimeout -> EprimeFP -> EprimeParamFP  -> IO ()
runSavilerow minionTimeout eprime param  = do
    _ <- shelly $ verbosely $ do
        echo "Running Savilerow"
        _ <- savilerow (LT.pack eprime) (LT.pack minion) (LT.pack eprimeSolution)
                       (Just . LT.pack $ param ) minionTimeout
        return ()
    return ()

    where
    baseName       = FP.dropExtensions eprime
    minion         = baseName FP.<.> "minion"
    eprimeSolution = baseName FP.<.> "eprime-solution"

savilerow :: LT.Text -> LT.Text -> LT.Text ->  Maybe LT.Text -> Int -> Sh LT.Text
savilerow in_eprime out_minion out_solution in_param timeout = run
                          "savilerow" $
                          ["-in-eprime",    in_eprime
                          ,"-out-minion",   out_minion
                          ,"-out-solution", out_solution
                          ,"-runsolver"
                          ,"-minion-options", LT.pack $  "-timelimit " ++   show timeout
                          ] ++ handleParam  in_param

    where
    handleParam Nothing = []
    handleParam (Just param) = ["-in-param",param]

