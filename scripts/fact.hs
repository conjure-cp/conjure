
import Data.List

factorial :: Integer -> Integer
factorial i = genericIndex factorials i

factorials :: [Integer]
factorials = 1 : [ i * factorial (i-1) | i <- [1..] ]

-- index :: a -> [a] -> n -> a
index def []     n | n <= 0 = def
index def (x:_ ) n | n <= 0 = x
index def (x:xs) n = index def xs (n-1)

(!) :: (Num n, Ord n, Num i) => n -> n -> [t2] -> a -> t2
l ! i = index 0
