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
     =  return . onSpec toIntIsNoOp
    >=> conjureNoGuards
    >=> bubbleUpSpec
    >=> return . atMostOneSuchThat
    >=> return . langEPrime


onSpec :: (E -> E) -> Spec -> Spec
onSpec f (Spec v xs) = Spec v $ map (transform f) xs


toIntIsNoOp :: E -> E
toIntIsNoOp [eMatch| toInt(&x) |] = x
toIntIsNoOp p = p


langEPrime :: Spec -> Spec
langEPrime (Spec _ xs) = Spec ("ESSENCE'", [1,0]) xs


