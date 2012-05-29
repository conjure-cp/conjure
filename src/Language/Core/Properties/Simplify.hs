{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.Simplify where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST


class Simplify a where
    simplify :: Monad m => a -> CompT m Core

instance Simplify Core where
    simplify = return

instance Simplify Reference where
    simplify r = core <?> "domain check for reference" <+> showAST r
        where
            core = do
                val <- lookUpRef r
                simplify val
