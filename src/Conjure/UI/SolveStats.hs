{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Conjure.UI.SolveStats (mkSolveStats, SolveStats (..), SolveStatus (..)) where

import Conjure.Bug
import Conjure.Prelude
import Conjure.RepositoryVersion (repositoryVersion)
import Conjure.UI (UI (..))
import Data.HashMap.Strict qualified as M -- unordered-containers
import Data.Text qualified as T (isInfixOf) -- text
import Data.Time (UTCTime, getCurrentTime) -- time
import Network.HostName (getHostName) -- hostname
import Shelly (run)

data SolveStats = SolveStats
  { status :: SolveStatus,
    totalTime :: Maybe Double,
    savilerowInfo :: M.HashMap String String,
    essence :: FilePath,
    essenceParams :: [FilePath],
    useExistingModels :: [FilePath],
    savilerowOptions :: [String],
    solverOptions :: [String],
    solver :: String,
    computer :: String,
    timestamp :: UTCTime,
    conjureVersion :: String,
    savileRowVersion :: String,
    logs :: Maybe String
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SolveStats

instance ToJSON SolveStats where toJSON = genericToJSON jsonOptions

instance FromJSON SolveStats where parseJSON = genericParseJSON jsonOptions

data SolveStatus = OK | TimeOut | MemOut | Error
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SolveStatus

instance ToJSON SolveStatus where toJSON = genericToJSON jsonOptions

instance FromJSON SolveStatus where parseJSON = genericParseJSON jsonOptions

mkSolveStats :: UI -> Int -> String -> Text -> IO SolveStats
mkSolveStats Solve {..} exitCodeSR rawInfo stdoutSR = do
  let info = M.fromList [(k, v) | [k, v] <- map (splitOn ":") (lines rawInfo)]
      status
        | M.lookup "SavileRowTimeOut" info == Just "1" = TimeOut
        | M.lookup "SavileRowClauseOut" info == Just "1" = TimeOut
        | M.lookup "SolverTimeOut" info == Just "1" = TimeOut
        | T.isInfixOf "Savile Row timed out." stdoutSR = TimeOut
        | T.isInfixOf "java.lang.OutOfMemoryError" stdoutSR = MemOut
        | exitCodeSR /= 0 = Error
        | otherwise = OK
      totalTime
        | Just srTotalTime <- M.lookup "SavileRowTotalTime" info >>= readMay,
          Just solverTotalTime <- M.lookup "SolverTotalTime" info >>= readMay =
            Just (srTotalTime + solverTotalTime)
        | otherwise = Nothing
      savilerowInfo = info
  computer <- getHostName
  timestamp <- getCurrentTime
  let conjureVersion = repositoryVersion
  savileRowVersion <- head . lines . textToString <$> sh (run "savilerow" ["-help"])
  let logs = if status == Error then Just (textToString stdoutSR) else Nothing
  return SolveStats {..}
mkSolveStats _ _ _ _ = bug "mkSolveStats"
