module Language.E.Pipeline.Groom where

import Language.E
import Language.E.Pipeline.SavileRowCompat ( savilerowCompat )


groomSpec :: (Functor m, Monad m) => Spec -> CompE m Spec
groomSpec spec@(Spec _ statements) = do
    initialiseSpecState spec
    mapM_ introduceStuff statements
    savilerowCompat spec

