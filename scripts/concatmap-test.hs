-- 
-- import Control.Monad
-- import Control.Monad.Identity
-- import Control.Monad.Error
-- import Debug.Trace
-- import Control.DeepSeq
-- 
-- tracing :: String -> String
-- tracing s = trace ("trace: " ++ s) s
-- 
-- f :: Monad m => String -> m [String]
-- f x = return [ tracing (x ++ show i)
--              | i <- [1..3]
--              ]
-- 
-- doAll :: Monad m => Int -> String -> m [String]
-- doAll 1 x = f x
-- doAll i x = do
--     ys <- f x
--     concatMapM (doAll (i-1)) ys
-- 
-- concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- concatMapM f as = do
--     bbs <- mapM f as
--     return (concat bbs)
-- 
-- 
-- testID :: [String]
-- testID = runIdentity $ doAll 2 "x_"
-- testID_short :: [String]
-- testID_short = take 1 testID
-- 
-- 
-- testIO :: IO [String]
-- testIO = doAll 2 "x_"
-- testIO_short :: IO [String]
-- testIO_short = fmap (take 1) testIO 
-- 
-- testErrIO :: IO (Either String [String])
-- testErrIO = runErrorT $ doAll 2 "x_"
-- testErrIO_short :: IO (Either String [String])
-- testErrIO_short = do
--     tmp <- testErrIO
--     case tmp of
--         Left _   -> return tmp
--         Right xs -> return $ Right $ take 1 xs


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

-- What if we want to be 'lazier', only print those numbers that are in the output list? Albeit unsafe in certain cases, one way is the following:

g :: Show a => IO a -> IO a
g = unsafeInterleaveIO . f

-- ghci> take 3 <$> mapM g xs -- prints numbers from 1 to 3 and returns a list [1..3]