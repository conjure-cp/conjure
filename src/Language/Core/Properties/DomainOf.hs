{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.DomainOf where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST


class DomainOf a where
    domainOf :: Monad m => a -> CompT m Core

instance DomainOf Core where
    domainOf _ = err "Unknown domain."

instance DomainOf Reference where
    domainOf r = core <?> "domain check for reference" <+> showAST r
        where
            core = do
                val <- lookUpRef r
                domainOf val

