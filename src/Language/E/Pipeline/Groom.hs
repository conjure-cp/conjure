module Language.E.Pipeline.Groom where

import Language.E
import Language.E.Pipeline.SavileRowCompat ( savilerowCompat )
import Language.E.Pipeline.IntroduceRegions ( removeRegions )


groomSpec :: MonadConjure m => Spec -> m Spec
groomSpec =  removeRegions
         >=> savilerowCompat

