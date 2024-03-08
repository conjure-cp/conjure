{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Conjure.UI.SolveStats (mkSolveStats, SolveStats (..), SolveStatus (..)) where

import Conjure.Bug
import Conjure.Prelude
import Conjure.UI (UI (..))
import Data.HashMap.Strict qualified as M -- unordered-containers
import Data.Text qualified as T (isInfixOf) -- text

data SolveStats = SolveStats
  { status :: SolveStatus,
    totalTime :: Maybe Double,
    savilerowInfo :: M.HashMap String String,
    essence :: FilePath,
    essenceParams :: [FilePath],
    useExistingModels :: [FilePath],
    savilerowOptions :: [String],
    solverOptions :: [String],
    solver :: String
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SolveStats

instance ToJSON SolveStats where toJSON = genericToJSON jsonOptions

instance FromJSON SolveStats where parseJSON = genericParseJSON jsonOptions

data SolveStatus = OK | TimeOut | MemOut
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SolveStatus

instance ToJSON SolveStatus where toJSON = genericToJSON jsonOptions

instance FromJSON SolveStatus where parseJSON = genericParseJSON jsonOptions

mkSolveStats :: UI -> String -> Text -> SolveStats
mkSolveStats Solve {..} raw stdout =
  let info = M.fromList [(k, v) | [k, v] <- map (splitOn ":") (lines raw)]
      status
        | M.lookup "SavileRowTimeOut" info == Just "1" = TimeOut
        | M.lookup "SavileRowClauseOut" info == Just "1" = TimeOut
        | M.lookup "SolverTimeOut" info == Just "1" = TimeOut
        | T.isInfixOf "Savile Row timed out." stdout = TimeOut
        | T.isInfixOf "java.lang.OutOfMemoryError" stdout = MemOut
        | otherwise = OK
      totalTime
        | Just srTotalTime <- M.lookup "SavileRowTotalTime" info >>= readMay,
          Just solverTotalTime <- M.lookup "SolverTotalTime" info >>= readMay =
            Just (srTotalTime + solverTotalTime)
        | otherwise = Nothing
      savilerowInfo = info
   in SolveStats {..}
mkSolveStats _ _ _ = bug "mkSolveStats"
