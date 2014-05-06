
import Data.List

nbCons :: [Int]
nbCons = [0,10..100]

main :: IO ()
main = do
    mapM_ (gen "set" ) nbCons
    mapM_ (gen "mset") nbCons

gen :: String -> Int -> IO ()
gen setOrMSet n = writeFile (setOrMSet ++ "-" ++ pad 3 (show n) ++ ".essence") $ unlines $
    [ "language Essence 1.3"
    , "given n, a, b " ++ concatMap (", "++ ) vals ++ ": int"
    , "find x : " ++ setOrMSet ++ " (size n) of int(a..b)"
    , "such that"
    ] ++ [ "    " ++ val ++ " in x," | val <- vals ]
      ++ ["    true"]
    where
        vals = map ("p"++) $ map show [1..n]

pad :: Int -> String -> String
pad n s = replicate (n - length s) '0' ++ s
