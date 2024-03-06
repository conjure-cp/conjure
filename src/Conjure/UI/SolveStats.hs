{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Conjure.UI.SolveStats (mkSolveStats, SolveStats (..), SolveStatus (..)) where

import Conjure.Prelude
import Data.HashMap.Strict qualified as M -- unordered-containers
import Data.Text qualified as T (isInfixOf) -- text

data SolveStats = SolveStats {status :: SolveStatus, totalTime :: Maybe Double, savilerowInfo :: M.HashMap String String}
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SolveStats

instance ToJSON SolveStats where toJSON = genericToJSON jsonOptions

instance FromJSON SolveStats where parseJSON = genericParseJSON jsonOptions

data SolveStatus = OK | TimeOut | MemOut
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SolveStatus

instance ToJSON SolveStatus where toJSON = genericToJSON jsonOptions

instance FromJSON SolveStatus where parseJSON = genericParseJSON jsonOptions

mkSolveStats :: String -> Text -> SolveStats
mkSolveStats raw stdout =
  let info = M.fromList [(k, v) | [k, v] <- map (splitOn ":") (lines raw)]
      status
        | info M.! "SavileRowTimeOut" == "1" = TimeOut
        | info M.! "SavileRowClauseOut" == "1" = TimeOut
        | info M.! "SolverTimeOut" == "1" = TimeOut
        | T.isInfixOf "Savile Row timed out." stdout = TimeOut
        | T.isInfixOf "java.lang.OutOfMemoryError" stdout = MemOut
        | otherwise = OK
      totalTime
        | Just srTotalTime <- readMay $ info M.! "SavileRowTotalTime",
          Just solverTotalTime <- readMay $ info M.! "SolverTotalTime" =
            Just (srTotalTime + solverTotalTime)
        | otherwise = Nothing
      savilerowInfo = info
   in SolveStats {..}
