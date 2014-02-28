
import Data.List ( intercalate )
import System.Directory ( createDirectoryIfMissing )

maxSize :: String
maxSize = "3"

intDomain :: String
intDomain = "int(1.." ++ maxSize ++ ")"

domains :: [(String, String -> String)]
domains =
    [ ("set"       , \ d -> "set (maxSize " ++ maxSize ++ ") of " ++ d)
    , ("mset"      , \ d -> "mset (maxSize " ++ maxSize ++ ") of " ++ d)
    , ("relation"  , \ d -> "relation (maxSize " ++ maxSize ++ ") of (" ++ intercalate " * " (replicate 2 d) ++ ")")
    , ("function"  , \ d -> "function (maxSize " ++ maxSize ++ ") " ++ intDomain ++ " --> " ++ d)
    , ("partition" , \ d -> "partition (maxNumParts " ++ maxSize ++ ") from " ++ d)
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

