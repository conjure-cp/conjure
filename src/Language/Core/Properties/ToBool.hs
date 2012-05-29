{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToBool where

import Language.Core
import Language.Core.Properties.Pretty
import Language.Core.Properties.ToLit

toBool :: (Monad m, ToLit a, Pretty a) => a -> CompT m Bool
toBool a = do
    b <- toLit a
    case b of
        B x -> return x
        _   -> err $ "Core.toBool, not a boolean: " <+> pretty a
