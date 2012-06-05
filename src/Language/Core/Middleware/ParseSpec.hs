{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.ParseSpec ( worker ) where

import Language.Core
import Language.Core.Middleware

worker :: (Functor m, Monad m) => Middleware (CompT m) (FilePath, Text) Spec
worker (fp,text) = runP (Just fp) parseSpec text
