{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToLit where


import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST


class ToLit a where
    toLit :: Monad m => a -> CompT m Literal

instance ToLit Core where
    toLit (L x) = toLit x
    toLit (R x) = toLit x
    toLit p     = err $ "Core.toLit:" <+> showAST p

instance ToLit Literal where
    toLit = return

instance ToLit Reference where
    toLit r = core <?> "Reference.toLit:" <+> showAST r
        where
            core = do
                val <- lookUpRef r
                toLit val
