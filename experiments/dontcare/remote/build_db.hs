
import Control.Applicative ( (<$>) )
import Control.Monad ( forM, forM_ )
import Data.List ( nub, intercalate )
import Data.List.Split ( splitOn )
import System.Environment ( getArgs )

main :: IO ()
main = do
    infoFiles <- getArgs
    allData <- forM infoFiles $ \ f -> do
        ls <- lines <$> readFile f
        return $ ("filename", f)
               : [ (key,value)
                 | l <- ls
                 , not (null l)
                 , head l /= '#'
                 , [key,value] <- [splitOn ":" l]
                 ]
    let allKeys = nub $ concatMap (map fst) (allData :: [[(String,String)]])
    putStrLn $ intercalate ", " $ map show allKeys
    forM_ allData $ \ dat ->
        if map fst dat == allKeys
            then putStrLn $ intercalate ", " $ map (show . snd) dat
            else error $ unlines [ "Doesn't have all keys."
                                 , show dat
                                 , show $ map fst dat
                                 , show $ allKeys
                                 ]

