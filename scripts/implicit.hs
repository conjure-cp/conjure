{-# LANGUAGE ImplicitParams #-}

import Prelude -- hiding ( sort )
import Data.List ( sortBy )

import Criterion.Main ( defaultMain, bench, nf )


sort :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort = sortBy ?cmp

least :: (?cmp :: a -> a -> Ordering) => [a] -> a
least = head . sort


len_acc1 [] = ?acc
len_acc1 (x:xs) = let ?acc = ?acc + 1
                  in  len_acc1 xs

len :: [a] -> Int
len = let ?acc = 0 in len_acc1

main = do
    let
        l1 = replicate 100 [False]
        l2 = replicate 200 [True]
        l3 = l1 ++ l2
        l4 = l3 ++ l3
    defaultMain
        [ bench "l1.len"    $ nf len    l1
        , bench "l1.length" $ nf length l1
        , bench "l2.len"    $ nf len    l2
        , bench "l2.length" $ nf length l2
        , bench "l3.len"    $ nf len    l3
        , bench "l3.length" $ nf length l3
        , bench "l4.len"    $ nf len    l4
        , bench "l4.length" $ nf length l4
        ]
