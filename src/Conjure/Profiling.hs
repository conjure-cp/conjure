{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Conjure.Profiling
    ( Stat(..)
    , KeepStats(..)
    , KeepStatsM(..)
    , tickPhase
    , runKeepStats
    ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Pretty
import Conjure.Language.NameGen

-- base
import System.CPUTime ( getCPUTime )
import Text.Printf ( printf )

-- pipes
import qualified Pipes


data Stat
    = StatRule
        { statRuleName      :: String       -- name of the rule
        , statNbFailedRules :: Int          -- number of failed rules before applying this one
        , statOldExpr       :: Doc          -- not an Expression so we don't rewrite these by accident
        , statNewExpr       :: Doc
        , statTimestamp     :: Integer      -- getCPUTime after rule application
        }
    | StatPhase
        { statPhaseName     :: String       -- phase name
        , statTimestamp     :: Integer      -- getCPUTime at phase
        }
    deriving Show


instance Pretty Stat where
    pretty StatRule{..}  = stringToDoc $ printf "TIME %-6.3f %-60s (%d)"
                                            (toSeconds statTimestamp)
                                            statRuleName
                                            statNbFailedRules
    pretty StatPhase{..} = stringToDoc $ printf "TIME %-6.3f %-60s"
                                            (toSeconds statTimestamp)
                                            statPhaseName


-- cputime is returned in pico-seconds. arbitrary precision integer.
-- divide by 10^9 first. use arbitrary precision integer arithmetic.
-- do the last 10^3 division via double to get 3 significant digits after the integer part.
toSeconds :: Integer -> Double
toSeconds cputime = fromInteger (cputime `div` 1000000000) / 1000


newtype KeepStatsM m a = KeepStatsM (StateT [Stat] m a)
    deriving ( Functor, Applicative, Monad
             , MonadFail, MonadUserError
             , MonadLog
             , MonadTrans
             , MonadState [Stat]
             , MonadIO
             )

runKeepStats :: Monad m => KeepStatsM m a -> m a
runKeepStats (KeepStatsM m) = evalStateT m []


tickPhase :: KeepStats m => String -> m ()
tickPhase ph = tick (StatPhase ph 0)

prepIO :: Stat -> IO Stat
prepIO stat = do
    cputime <- getCPUTime
    let stat2 = stat { statTimestamp = cputime }
    return stat2



class Monad m => KeepStats m where
    tick :: Stat -> m ()
    getStats :: m [Stat]

instance MonadIO m => KeepStats (KeepStatsM m) where
    tick stat' = do
        stat <- liftIO (prepIO stat')
        modify (stat:)
    getStats = get

instance KeepStats m => KeepStats (Pipes.Proxy a b c d m) where
    tick = lift . tick
    getStats = lift getStats

instance KeepStats m => KeepStats (NameGenM m) where
    tick = lift . tick
    getStats = lift getStats

