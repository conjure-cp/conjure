{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Language.My.Test where

import Stuff.Generic.Definition


-- x = [test|[Prim 1]|]

x :: Generic Int Int
x =
    [xMake| x.a := [Prim 1]
         | x.b := [Prim 2]
         |]

f :: Generic Int Int -> (Int, Int)
f [xMatch| [Prim i] := x.a
        | [Prim j] := x.b
        |] = (i,j)

g :: Generic Int Int -> (Int, Int)
g [xMatch| [Prim i] := x.c
        | [Prim j] := x.b
        |] = (i,j)



-- y :: Generic Int Int
-- y =
--     [xMake| operator.plus.left  := 1
--          | operator.plus.right.operator.times.left  := 2
--          | operator.plus.right.operator.times.right := 3
--          |]

-- evalPlus :: Generic Int Int -> Maybe (Generic Int Int)
-- evalPlus
--     [xMatch| [Prim a] := operator.plus.left
--           | [Prim b] := operator.plus.right
--           |] = return $ Prim $ a + b
-- evalPlus _ = Nothing

-- evalMinus :: Generic Int Int -> Maybe (Generic Int Int)
-- evalMinus
--     [xMatch| [Prim a] = operator.minus.left
--           | [Prim b] = operator.minus.right
--           |] = return $ Prim $ a - b
-- evalMinus _ = Nothing

-- evalTimes :: Generic Int Int -> Maybe (Generic Int Int)
-- evalTimes
--     [xMatch| [Prim a] = operator.times.left
--           | [Prim b] = operator.times.right
--           |] = return $ Prim $ a * b
-- evalTimes _ = Nothing

-- evalDiv :: Generic Int Int -> Maybe (Generic Int Int)
-- evalDiv
--     [xMatch| [Prim a] = operator.times.left
--           | [Prim b] = operator.times.right
--           |]
--           | b /= 0 = return $ Prim $ div a b
-- evalDiv _ = Nothing
