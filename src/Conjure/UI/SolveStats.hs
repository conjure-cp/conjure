{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Conjure.UI.SolveStats (mkSolveStats, SolveStats (..), SolveStatus (..)) where

import Conjure.Bug
import Conjure.Prelude
import Conjure.UI (UI (..), versionLine)
import Data.HashMap.Strict qualified as M -- unordered-containers
import Data.Text qualified as T (isInfixOf, null, unlines) -- text
import Data.Time (UTCTime, getCurrentTime) -- time
import Network.HostName (getHostName) -- hostname
import Shelly (run) -- shelly

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
    savilerowVersion :: String,
    savilerowLogs :: SavileRowLogs
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

data SavileRowLogs = SavileRowLogs
  { exitCode :: Int,
    stderr :: Maybe [String],
    stdout :: Maybe [String]
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Hashable SavileRowLogs

instance ToJSON SavileRowLogs where toJSON = genericToJSON jsonOptions

instance FromJSON SavileRowLogs where parseJSON = genericParseJSON jsonOptions

mkSolveStats :: UI -> (Int, Text, Text) -> String -> IO SolveStats
mkSolveStats Solve {..} (exitCodeSR, stdoutSR, stderrSR) rawInfo = do
  let combinedSR = T.unlines [stdoutSR, stderrSR]
  let info = M.fromList [(k, v) | [k, v] <- map (splitOn ":") (lines rawInfo)]
      status
        | or
            [ T.isInfixOf msg combinedSR
              | msg <-
                  [ "type error: undefined identifier",
                    "MiniZinc error", -- minizinc
                    "Check failed: ParseFlatzincFile",
                    "parse error: unexpected end-of-file after parsing number of clauses",
                    "error: Cannot open file",
                    "kissat: error: can not read", -- kissat
                    "kissat: fatal error: maximum arena capacity", -- kissat
                    "Error: syntax error, unexpected ]]" -- cplex
                  ]
            ] =
            Error
        | T.isInfixOf "java.lang.OutOfMemoryError" combinedSR = MemOut
        | T.isInfixOf "Out of Memory" combinedSR = MemOut
        | T.isInfixOf "Savile Row timed out." combinedSR = TimeOut
        | T.isInfixOf "time out: time limit reached" combinedSR = TimeOut
        | M.lookup "SavileRowTimeOut" info == Just "1" = TimeOut
        | M.lookup "SavileRowClauseOut" info == Just "1" = TimeOut
        | M.lookup "SolverTimeOut" info == Just "1" = TimeOut
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
  let conjureVersion = versionLine
  savilerowVersion <- head . lines . textToString <$> sh (run "savilerow" ["-help"])
  let savilerowLogs =
        SavileRowLogs
          { exitCode = exitCodeSR,
            stdout = if T.null stdoutSR then Nothing else Just (lines (textToString stdoutSR)),
            stderr = if T.null stderrSR then Nothing else Just (lines (textToString stderrSR))
          }
  return SolveStats {..}
mkSolveStats _ _ _ = bug "mkSolveStats"
