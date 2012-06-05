{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToInt where

import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.Core.Properties.ToLit


toInt :: (Functor m, Monad m, ToLit a, ShowAST a) => a -> CompT m (Maybe Integer)
toInt a = do
    b <- toLit a
    case b of
        Just (I x) -> return (Just x)
        _   -> return Nothing -- err $ "Core.toInt, not an integer: " <+> showAST a
