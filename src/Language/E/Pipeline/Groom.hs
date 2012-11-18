module Language.E.Pipeline.Groom where

import Language.E
import Language.E.Pipeline.SavileRowCompat ( savilerowCompat )


groomSpec :: MonadConjure m => Spec -> m Spec
groomSpec = savilerowCompat

