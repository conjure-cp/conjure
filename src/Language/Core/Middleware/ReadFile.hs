{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.ReadFile ( worker ) where

import Language.Core.Imports
import Language.Core.Middleware

import Data.Text.IO as T

worker :: MonadIO m => Middleware m FilePath (FilePath, Text)
worker filepath = do
    content <- liftIO $ T.readFile filepath
    return (filepath, content)
