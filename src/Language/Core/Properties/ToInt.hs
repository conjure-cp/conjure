{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToInt where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.Core.Properties.ToLit


toInt :: (Monad m, ToLit a, ShowAST a) => a -> CompT m Integer
toInt a = do
    b <- toLit a
    case b of
        I x -> return x
        _   -> err $ "Core.toInt, not an integer: " <+> showAST a
