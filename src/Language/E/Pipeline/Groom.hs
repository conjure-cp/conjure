module Language.E.Pipeline.Groom where

import Language.E
-- import Language.E.Pipeline.NoGuards ( conjureNoGuards )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )

groomSpec :: Spec -> IO [Spec]
groomSpec = runCompEIO . pipeline
    where pipeline = trySimplifySpec
                        -- >=> conjureNoGuards
                        >=> return . atMostOneSuchThat
                        >=> return . langEPrime
          langEPrime (Spec _ xs) = Spec ("ESSENCE'", [1,0]) xs
