
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
    (n:_) <- getArgs
    let maxSize = read n
    sequence_
        [ do
            createDirectoryIfMissing True dirname
            writeFile filename essence
            putStrLn $ "Created " ++ filename
        | (n1,d1) <- domains maxSize
        , (n2,d2) <- domains maxSize
        , let dirname  = n1 ++ "-" ++ n2
        , let filename = dirname ++ "/" ++ dirname ++ ".essence"
        , let essence  = unlines [ "language Essence 1.3"
                                 , "find x : " ++ d1 (d2 (intDomain maxSize))
                                 ]
        ]

