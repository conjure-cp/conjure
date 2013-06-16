{-# OPTIONS_GHC -fno-warn-type-defaults  -fno-warn-unused-binds #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.GenerateParams.Toolchain where


import Language.E.GenerateParams.Typedefs 

import Prelude hiding ( FilePath, reverse )
import Shelly

import qualified Data.Text.Lazy      as LT
{-import qualified System.Directory    as FP-}
import qualified System.FilePath     as FP

type MinionTimeout = Int

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
                          ,"-minion-options", "-timelimit ", LT.pack . show $ timeout
                          ] ++ handleParam  in_param

    where
    handleParam Nothing = []
    handleParam (Just param) = ["-in-param",param]

