{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.MkSafe where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST


class MkSafe a where
    mkSafe :: Monad m => a -> CompT m Core

instance MkSafe Core where
    mkSafe _ = err "Unknown domain."

instance MkSafe Reference where
    mkSafe r = core <?> "safety check for reference" <+> showAST r
        where
            core = do
                val <- lookUpRef r
                mkSafe val
