{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.ToLit where


import Language.Core
import Language.Core.Properties.Pretty


class ToLit a where
    toLit :: Monad m => a -> CompT m Literal

instance ToLit Core where
    toLit (L x) = toLit x
    toLit (R x) = toLit x
    toLit p     = err $ "Core.toLit:" <+> pretty p

instance ToLit Literal where
    toLit = return

instance ToLit Reference where
    toLit r = core <?> "Reference.toLit:" <+> pretty r
        where
            core = do
                val <- lookUpRef r
                toLit val
