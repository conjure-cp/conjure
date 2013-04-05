{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.GenerateRandomParam2 where


import Prelude hiding ( FilePath, reverse )
import qualified Data.Text.Lazy as LT
-- default (LT.Text)

import Shelly

import Control.Monad
import System.Environment
import Data.List ( genericLength )
import qualified Data.HashMap.Strict as M
import qualified Debug.Trace as Debug

import Language.E
import Language.E.Up(translateSolution')

intermediateDir = "intermediate"

type EssenceSolution = Spec

generateRandomParam :: IO EssenceSolution
generateRandomParam = do
    _ <- shelly $ verbosely $ do
        mkdir_p intermediateDir

        echo "Running Savilerow"
        _ <- savilerow in_eprime out_minion out_solution in_param

        return ()

    putStrLn "Running translateSolution"
    solution <- translateSolution' 
        (LT.unpack in_essence) 
        (LT.unpack <$> in_essence_param)
        (LT.unpack in_eprime) 
        (LT.unpack <$> in_param)
        (LT.unpack out_solution)

    
    return solution 


savilerow in_eprime out_minion out_solution in_param= run
                          "savilerow" $
                          ["-in-eprime",    in_eprime
                          ,"-out-minion",   out_minion
                          ,"-out-solution", out_solution
                          ,"-runsolver"
                          ] ++ handleParam  in_param

    where
    handleParam Nothing = []
    handleParam (Just param) = ["-in-param",param]


in_eprime        = "/Users/bilalh/CS/conjure/files/upTests/_easy/b3/0001.eprime"
out_minion       = "/Users/bilalh/CS/conjure/files/upTests/_easy/b3/0001.eprime.minion"
out_solution     = "/Users/bilalh/CS/conjure/files/upTests/_easy/b3/0001.eprime.solution"
{-in_param       = Just "/Users/bilalh/CS/conjure/files/upTests/__essence_catalog/prob005/prob005.param"-}
in_param         = Nothing
in_essence_param = Nothing
in_essence = "/Users/bilalh/CS/conjure/files/upTests/_easy/b3.essence" 

