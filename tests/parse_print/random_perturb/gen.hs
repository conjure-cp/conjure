
-- base
import System.Environment
import Control.Monad
import Text.Read

-- directory
import System.Directory

-- random
import System.Random


main :: IO ()
main = do
    args <- getArgs
    case args of
        [action, dir, file, numStr] | Just num <- readMaybe numStr -> do
            con <- readFile file
            let l = length (filter (not . skip) con)
            positions <- getRandomPositions [0..l-1] num
            let
                apply [] _ _ = return []
                apply (x:xs) i pos
                    | skip x = (x :) <$> apply xs i pos             -- if x is to be skipped
                    | i == pos =                                    -- act!
                        case action of
                            "delete" -> return xs
                            "change" -> do
                                ch <- getRandomChar
                                return (ch:xs)
                            _ -> error ("action is " ++ action)
                    | otherwise = (x :) <$> apply xs (i+1) pos     -- keep going
            forM_ (zip [1..] positions) $ \ (seq_, pos) -> do
                con' <- apply con 0 pos
                let outdir = dir ++ "/" ++ action ++ "-" ++ paddedShow 2 seq_
                let outfile = outdir ++ "/" ++ action ++ "-" ++ paddedShow 2 seq_ ++ ".essence"
                createDirectoryIfMissing True outdir
                writeFile outfile con'
        _ -> error (unlines [ "Usage: runhaskell gen.hs <action> <dir> <file> <num>"
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

getRandomPositions :: [Int] -> Int -> IO [Int]
getRandomPositions options count = go options (length options) count
    where
        skip _ [] = error "getRandomPositions.skip"
        skip 0 (x:xs) = (x, xs)
        skip i (x:xs) = let (y, ys) = skip (i-1) xs
                        in  (y, x:ys)
        go opts _ 0 = return []
        go opts optsL n = do
            i <- randomRIO (0, optsL-1)
            let (this, opts') = skip i opts
            those <- go opts' (optsL-1) (n-1)
            return (this:those)

paddedShow n x = let s = show x in replicate (n - length s) '0' ++ s
