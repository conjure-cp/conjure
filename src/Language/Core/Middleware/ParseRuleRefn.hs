{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.ParseRuleRefn ( worker ) where

import Language.Core
import Language.Core.Middleware

worker :: (Functor m, Monad m) => Middleware (CompT m) (FilePath, Text) RuleRefn
worker (fp,text) = runP (Just fp) (parseRuleRefn $ stringToText fp) text
