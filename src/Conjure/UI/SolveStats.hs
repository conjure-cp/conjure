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
    runsolverInfo :: M.HashMap String String,
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

-- OK: solved, can be sat or unsat.
-- Invalid: instance is not valid.
-- TimeOut/MemOut: what they sound like.
-- Error: could be a bug, but could also be to do with a very large encoding.
data SolveStatus = OK | Invalid | TimeOut | MemOut | Error
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

mkSolveStats :: UI -> (Int, Text, Text) -> String -> String -> IO SolveStats
mkSolveStats Solve {..} (exitCodeSR, stdoutSR, stderrSR) savilerowInfoText runsolverInfoText = do
  let combinedSR = T.unlines [stdoutSR, stderrSR]

      savilerowInfo = M.fromList [(k, v) | [k, v] <- map (splitOn ":") (lines savilerowInfoText)]

      runsolverInfo = M.fromList [(k, v) | [k, v] <- map (splitOn "=") (lines runsolverInfoText)]

      status
        | or
            [ T.isInfixOf msg combinedSR
              | msg <-
                  [ "type error: undefined identifier",
                    "MiniZinc error: Memory violation detected", -- minizinc
                    "Check failed: ParseFlatzincFile",
                    "parse error: unexpected end-of-file after parsing number of clauses", -- kissat
                    "error: Cannot open file",
                    "kissat: error: can not read", -- kissat
                    "kissat: fatal error: maximum arena capacity", -- kissat
                    "Error: syntax error, unexpected ]]", -- cplex
                    "*** Check failure stack trace: ***", -- or-tools
                    "Error: evaluation error: Index set mismatch.",
                    "Savile Row killed by: java.lang.AssertionError",
                    "java.lang.ClassCastException",
                    "ERROR: File not found" -- savilerow
                  ]
            ] =
            Error
        | or
            [ T.isInfixOf msg combinedSR
              | msg <-
                  [ "java.lang.OutOfMemoryError",
                    "ERROR: Out of Memory",
                    "Maximum memory exceeded" -- for when runsolver prints this message but won't set MEMOUT=true for some reason
                  ]
            ] =
            MemOut
        | or
            [ T.isInfixOf msg combinedSR
              | msg <-
                  [ "Savile Row timed out.",
                    "time out: time limit reached",
                    "Received SIGTERM or SIGINT, killing child" -- for when runsolver prints this message but won't set MEMOUT=true or TIMEOUT=true for some reason
                  ]
            ] =
            TimeOut
        | T.isInfixOf "ERROR: In statement: where false" combinedSR = Invalid
        | M.lookup "MEMOUT" runsolverInfo == Just "true" = MemOut
        | M.lookup "TIMEOUT" runsolverInfo == Just "true" = TimeOut
        | M.lookup "SavileRowTimeOut" savilerowInfo == Just "1" = TimeOut
        | M.lookup "SavileRowClauseOut" savilerowInfo == Just "1" = TimeOut
        | M.lookup "SolverTimeOut" savilerowInfo == Just "1" = TimeOut
        | exitCodeSR /= 0 = Error
        | otherwise = OK

      totalTime
        | Just srTotalTime <- M.lookup "SavileRowTotalTime" savilerowInfo >>= readMay,
          Just solverTotalTime <- M.lookup "SolverTotalTime" savilerowInfo >>= readMay =
            Just (srTotalTime + solverTotalTime)
        | otherwise = Nothing

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
mkSolveStats _ _ _ _ = bug "mkSolveStats"
