{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.CategoryOf where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST


class CategoryOf a where
    categoryOf :: Monad m => a -> CompT m Category

instance CategoryOf Core where
    categoryOf _ = err "Unknown category."

instance CategoryOf Reference where
    categoryOf r = core <?> "category check for reference" <+> showAST r
        where
            core = do
                val <- lookUpRef r
                categoryOf val

