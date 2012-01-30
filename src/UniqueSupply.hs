{-# LANGUAGE BangPatterns #-}

module UniqueSupply where

import Data.IORef ( IORef, readIORef, writeIORef )
import Data.Global ( declareIORef )

intCounter :: IORef Int
intCounter = declareIORef "intCounter" 0

nextUniqueInt :: IO Int
nextUniqueInt = do
    !i <- readIORef intCounter
    writeIORef intCounter (i+1)
    return i
