
import Data.List ( intercalate )
import System.Directory ( createDirectoryIfMissing )

intDomain :: String
intDomain = "int(1..3)"

domains :: [(String, String -> String)]
domains =
    [ ("set"       , \ d -> "set (maxSize 3) of " ++ d)
    , ("mset"      , \ d -> "mset (maxSize 3) of " ++ d)
    , ("relation"  , \ d -> "relation (maxSize 3) of (" ++ intercalate " * " (replicate 2 d) ++ ")")
    , ("function"  , \ d -> "function (maxSize 3) int(1..3) --> " ++ d)
    , ("partition" , \ d -> "partition (maxNumParts 3) from " ++ d)
    ]

main :: IO ()
main = sequence_
        [ do
            createDirectoryIfMissing True dirname
            writeFile filename essence
            putStrLn $ "Created " ++ filename
        | (n1,d1) <- domains
        , (n2,d2) <- domains
        , let dirname  = n1 ++ "-" ++ n2
        , let filename = dirname ++ "/" ++ dirname ++ ".essence"
        , let essence  = unlines [ "language Essence 1.3"
                                 , "find x : " ++ d1 (d2 intDomain)
                                 ]
        ]

