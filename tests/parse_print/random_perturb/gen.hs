
-- base
import System.Environment

-- directory
import System.Directory

-- random
import System.Random


main :: IO ()
main = do
    args <- getArgs
    case args of
        [action, dir, file, seq_] -> do
            con <- readFile file
            let l = length (filter (not . skip) con)
            r <- randomRIO (0, l-1)
            let
                apply [] _ = return []
                apply (x:xs) i
                    | skip x = (x :) <$> apply xs i                 -- if x is to be skipped
                    | i == r =                                      -- act!
                        case action of
                            "delete" -> return xs
                            "change" -> do
                                ch <- getRandomChar
                                return (ch:xs)
                            _ -> error ("action is " ++ action)
                    | otherwise = (x :) <$> apply xs (i+1)          -- keep going
            con' <- apply con 0
            let outdir = dir ++ "/" ++ action ++ "-" ++ seq_
            let outfile = outdir ++ "/" ++ action ++ "-" ++ seq_ ++ ".essence"
            createDirectoryIfMissing True outdir
            writeFile outfile con'
        _ -> error (unlines [ "Usage: runhaskell gen.hs <action> <dir> <file> <seq>"
                            , "Given: " ++ show args
                            ])

-- skip these chars
skip :: Char -> Bool
skip ch = ch `elem` " \n\t"

getRandomChar :: IO Char
getRandomChar = do
    let chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "!%()*+,-./:;<=>@[\\]`{|}~"
    let nm = length chars
    i <- randomRIO (0, nm-1)
    return (chars !! i)
