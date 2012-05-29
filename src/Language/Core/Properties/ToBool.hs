{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToBool where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.Core.Properties.ToLit

toBool :: (Monad m, ToLit a, ShowAST a) => a -> CompT m Bool
toBool a = do
    b <- toLit a
    case b of
        B x -> return x
        _   -> err $ "Core.toBool, not a boolean: " <+> showAST a
