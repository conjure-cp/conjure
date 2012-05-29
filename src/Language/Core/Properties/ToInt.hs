{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToInt where

import Language.Core
import Language.Core.Properties.Pretty
import Language.Core.Properties.ToLit

toInt :: (Monad m, ToLit a, Pretty a) => a -> CompT m Integer
toInt a = do
    b <- toLit a
    case b of
        I x -> return x
        _   -> err $ "Core.toInt, not an integer: " <+> pretty a
