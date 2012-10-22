module Language.E.Pipeline.Groom where

import Language.E
import Language.E.Pipeline.SavileRowCompat ( savilerowCompat )


groomSpec :: (Functor m, Monad m) => Spec -> CompE m Spec
groomSpec = trySimplifySpec >=> savilerowCompat

