{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.SavileRowCompat where

import Language.E
import Language.E.Pipeline.NoGuards ( conjureNoGuards )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.BubbleUp ( bubbleUpSpec )


savilerowCompat :: (Monad m, Functor m)
    => Spec
    -> CompE m Spec
savilerowCompat
     =  traverseSpec Nothing toIntIsNoOp Nothing
    >=> conjureNoGuards
    >=> bubbleUpSpec
    >=> return . atMostOneSuchThat
    >=> return . langEPrime


toIntIsNoOp :: (Functor m, Monad m) => E -> CompE m E
toIntIsNoOp [eMatch| toInt(&x) |] = return x
toIntIsNoOp p = return p


langEPrime :: Spec -> Spec
langEPrime (Spec _ xs) = Spec ("ESSENCE'", [1,0]) xs


