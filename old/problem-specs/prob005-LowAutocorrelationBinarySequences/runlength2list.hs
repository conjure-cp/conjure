
import Data.Char(digitToInt,isDigit)

main :: IO ()
main = interact $ \ i -> runlength2list (cycle "-+") i ++ "\n"

runlength2list :: [a] -> String -> [a]
runlength2list _ [] = []
runlength2list (c:cs) (n:ns)
    | isDigit n = replicate (digitToInt n) c ++ runlength2list cs ns
    | otherwise = runlength2list cs ns
