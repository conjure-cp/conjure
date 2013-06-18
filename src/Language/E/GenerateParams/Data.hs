{-# LANGUAGE OverloadedStrings #-}
module Language.E.GenerateParams.Data where

import Bug
import Language.E
import Language.E.Pipeline.Driver

import Language.E.GenerateParams.Typedefs

import Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as M



data ParamGenState = ParamGenState{
     results :: Map EssenceParamFP (Map EprimeFP (Bool,Bool,Bool) )
    ,vars    :: [(Text,E)]
} deriving (Show)


type MonadParamGen a = State ParamGenState a

driverParamGen
    :: Bool     -- generate the *.logs file or not
    -> Bool     -- generate *.errors file or not
    -> FilePath -- the output filepath
    -> ParamGenState -- Starting state
    -> [(Either Doc (MonadParamGen EssenceParam), LogTree)]
    -> IO ParamGenState
driverParamGen logsOut _ pathOut startingState [(Right x, logs)] = do
    toFile  pathOut              (renderNormal (evalState x startingState) )
    _ <- when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)
    return $ execState x startingState

driverParamGen _ False _ _ [(Left x, _ )] = bug $ pretty x

driverParamGen logsOut True pathOut _ [(Left  x, logs)] = do
    toFile (pathOut ++ ".error") (renderNormal x)
    when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)
    bug $ pretty x

driverParamGen _ _ _ _ _ = error "driverParamGen could not happen"


getRights :: [(Either Doc [(Text, E)], LogTree)] -> IO [(Text,E)]
getRights [(Right x, logs )] = do
    printLogs  logs
    return x

getRights [(Left x, _ )]     = bug $ pretty x
getRights a                  = bug $ pretty $ show  a


startingParmGenState :: [(Text,E)] -> ParamGenState
startingParmGenState vars = ParamGenState{
    results=M.empty,
    vars = vars
}
