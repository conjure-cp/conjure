{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.MkSafe where

import Language.Core.Definition


class MkSafe a where
    mkSafe :: Monad m => a -> CompT m Core

instance MkSafe Core where
    mkSafe _ = err ErrMkSafe "Unknown domain."

instance MkSafe Reference where
    mkSafe r = do
        val <- lookUpRef r
        mkSafe val
