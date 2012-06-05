{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToBool where

import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.Core.Properties.ToLit

toBool :: (Functor m, Monad m, ToLit a, ShowAST a) => a -> CompT m (Maybe Bool)
toBool a = do
    b <- toLit a
    case b of
        Just (B x) -> return $ Just x
        _   -> return Nothing
