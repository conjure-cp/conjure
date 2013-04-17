-- 0001-10-9-3.eprime-minion.stats stats_arity_100:2188
-- 0001-10-9-3.eprime-minion.stats stats_TotalArity: 674514
-- 0001-10-9-3.eprime-minion.stats stats_arity_mean:5.919957
-- 0001-10-9-3.eprime-minion.stats stats_arity_mean_normalised:0.000051
-- 0001-10-9-3.eprime-minion.stats stats_cts_per_var_mean:5.843743
-- 0001-10-9-3.eprime-minion.stats stats_cts_per_var_mean_normalised:0.000051
-- 0001-10-9-3.eprime-minion.stats stats_alldiffdomovervars_0:0.000000
-- 0001-10-9-3.eprime-minion.stats stats_alldiffdomovervars_25:0.000000
-- 0001-10-9-3.eprime-minion.stats stats_alldiffdomovervars_50:0.000000
-- 0001-10-9-3.eprime-minion.stats stats_alldiffdomovervars_75:0.000000
-- 0001-10-9-3.eprime-minion.stats stats_alldiffdomovervars_100:0.000000
-- 0001-10-9-3.eprime-minion.stats stats_alldiffdomovervars_mean:0.000000
-- 0001-10-9-3.eprime-minion.stats stats_alldiff_count:0
-- 0001-10-9-3.eprime-minion.stats stats_alldiff_proportion:0.000000
-- 0001-10-9-3.eprime-minion.stats stats_sums_count:540


import Data.List
import Data.List.Split
import Data.Maybe
import System.Environment

main = do
    conjure_repo <- getEnv "CONJURE_REPO"
    args <- getArgs
    case args of
        [arg] -> interact
                    $ unlines
                    . (\ xs -> "BEGIN TRANSACTION;" : xs ++ ["COMMIT;"] )
                    . map (toSQL conjure_repo arg)
                    . mapMaybe parse
                    . filter nonHash
                    . lines
        _ -> error "gimme a spec name"

nonHash = not . ('#' `elem`)

-- parse line = [ eprime, param, attribute, value ]
parse line = case splitOn ":" blah of
    [attribute, value] -> Just [ eprime, param, filter (/=' ') attribute, value ]
    -- _                  -> Just [ eprime, param, blah ]
    _                  -> Nothing
    where
        eprime            = takeWhile (/='-') line
        param             = takeWhile (/='.') $ drop (length eprime + 1) line
        blah              = drop 1 $ dropWhile (/='\t') line
        -- [attribute,value] = splitOn ":" $ drop 1 $ dropWhile (/=' ') line
        --

toSQL conjure_repo essence xs
    = id
    $ ("INSERT OR REPLACE INTO attributes ( SPEC , MODEL , PARAM , ATTRIBUTE , VALUE) VALUES " ++)
    $ (\ i -> "(" ++ i ++ ");" )
    $ intercalate "," $ map show (essence:xs)

