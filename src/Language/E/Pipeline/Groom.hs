module Language.E.Pipeline.Groom where

import Language.E
-- import Language.E.Pipeline.NoGuards ( conjureNoGuards )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )

groomSpec :: (Functor m, Monad m) => Spec -> CompE m Spec
groomSpec = pipeline
    where pipeline = -- trySimplifySpec >=> conjureNoGuards >=>
                return . atMostOneSuchThat >=>
                return . langEPrime
          langEPrime (Spec _ xs) = Spec ("ESSENCE'", [1,0]) xs
