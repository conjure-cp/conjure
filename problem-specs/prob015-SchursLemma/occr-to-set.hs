
import Data.List
import Data.Maybe

main = interact ( unlines . map (\ l -> l ++ " # " ++ occrToSet l) . lines )

occrToSet xs
    = intercalate ","
    $ map show
    $ catMaybes
    $ zipWith (\ i j -> if i == "0" then Nothing else Just j) (words xs) [1..]
