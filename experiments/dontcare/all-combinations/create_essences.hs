module Main where

import Control.Monad ( replicateM )
import Data.List ( intercalate )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )


intDomain :: Int -> String
intDomain maxSize = "int(1.." ++ show maxSize ++ ")"

domains :: Int -> [(String, String -> String)]
domains maxSize =
    [ ("set"       , \ d -> "set (maxSize " ++ show maxSize ++ ") of " ++ d)
    , ("mset"      , \ d -> "mset (maxSize " ++ show maxSize ++ ") of " ++ d)
    , ("relation"  , \ d -> "relation (maxSize " ++ show maxSize ++ ") of (" ++ intercalate " * " (replicate 2 d) ++ ")")
    , ("function"  , \ d -> "function (maxSize " ++ show maxSize ++ ") " ++ intDomain maxSize ++ " --> " ++ d)
    , ("partition" , \ d -> "partition (maxNumParts " ++ show maxSize ++ ") from " ++ d)
    ]

main :: IO ()
main = do
    (level:maxSize:_) <- getArgs
    create (read level) (read maxSize)

create :: Int -- level of nesting (0 for int, at least 1 for abstract domains)
       -> Int -- maxSize
       -> IO ()
create level maxSize = sequence_
    [ do createDirectoryIfMissing True dirname
         writeFile filename essence
         putStrLn $ "Created " ++ filename
    | (ns, ds) <- map unzip $ replicateM level $ domains maxSize
    , let dirname  = intercalate "-" (("nesting" ++ show level) : ns)
    , let filename = dirname ++ "/" ++ dirname ++ ".essence"
    , let essence  = unlines
            [ "language Essence 1.3"
            , "find x : " ++ chainapp ds (intDomain maxSize)
            ]
    ]

chainapp :: [a -> a] -> a -> a
chainapp [] a = a
chainapp (f:fs) a = f (chainapp fs a)

