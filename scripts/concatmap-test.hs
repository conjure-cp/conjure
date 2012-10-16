
import Control.Applicative
import System.IO.Unsafe

-- f is a weird function. it prints the result of a computation before returning it.
f :: Show a => IO a -> IO a
f i = do j <- i; print j; return j

-- let's have an [IO Int] list to use in tests.
xs :: [IO Int]
xs = map return [1..10]

-- ghci> mapM f xs -- prints numbers from 1 to 10, and returns a list: [1..10]
-- ghci> take 3 <$> mapM f xs -- prints numbers from 1 to 10, and returns a list: [1..3]

-- What if we want to be 'lazier', only print those numbers that are in the output list?
-- Albeit unsafe in certain cases, one way is the following:

g :: Show a => IO a -> IO a
g = unsafeInterleaveIO . f

-- ghci> take 3 <$> mapM g xs -- prints numbers from 1 to 3 and returns a list [1..3]

