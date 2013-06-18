{-# LANGUAGE OverloadedStrings #-}
module Language.E.GenerateParams.Data where

import Bug
import Language.E 
import Language.E.Pipeline.Driver

import Language.E.GenerateParams.Typedefs

import Control.Monad.State

type ParamGenState = Int
type MonadParamGen a = State ParamGenState a 

driverParamGen
    :: Bool     -- generate the *.logs file or not
    -> Bool     -- generate *.errors file or not
    -> FilePath -- the output filepath
    -> ParamGenState -- Starting state 
    -> [(Either Doc (MonadParamGen EssenceParam), LogTree)]
    -> IO ParamGenState 
driverParamGen logsOut _ pathOut _ [(Right x, logs)] = do
    toFile  pathOut              (renderNormal (evalState x 0) )
    _ <- when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)
    return $ execState x 0

driverParamGen _ False _ _ [(Left x, _ )] = bug $ pretty x

driverParamGen logsOut True pathOut _ [(Left  x, logs)] = do
    toFile (pathOut ++ ".error") (renderNormal x)
    when logsOut $ toFile (pathOut ++ ".logs" ) (renderWide logs)
    bug "errors" 

driverParamGen _ _ _ _ _ = error "could not happen"

startingParmGenState :: ParamGenState
startingParmGenState = 0

