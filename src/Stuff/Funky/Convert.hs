module Stuff.Funky.Convert where

import Stuff.Funky.FunkySingle
import Stuff.Funky.FunkyMulti


funkySingleToMulti
    :: Monad m
    => FunkySingle    st err m a
    -> FunkyMulti  () st err m a
funkySingleToMulti msingle = FunkyMulti $ \ () st -> do
    single <- runFunkySingle st msingle
    return ([single], ())


funkyMultiToSingle
    :: Monad m
    => FunkyMulti  () st err m a
    -> FunkySingle    st err m a
funkyMultiToSingle mmulti = FunkySingle $ \ st -> do
    (multis, _) <- runFunkyMulti () st mmulti
    case multis of
        [single] -> return single
        _        -> error "funkyMultiToSingle"

