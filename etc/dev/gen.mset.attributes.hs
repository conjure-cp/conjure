
import Data.List
import Data.Ord

main
    = mapM_ putStrLn
    $ map (\ line -> "*** mset (" ++ line ++ ") of &tau" )
    $ map unwords
    $ map (concat . intersperse [","] . map (\ x -> let y = "&" ++ x ++ "_" in [x,y]))
    $ reverse
    $ sortBy (comparing length)
    $ subsequences ["size","minSize", "maxSize", "minOccur", "maxOccur"]

