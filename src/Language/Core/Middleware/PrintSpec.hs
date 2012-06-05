{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.PrintSpec ( worker ) where

import Language.Core
import Language.Core.Middleware

worker :: Monad m => Middleware m Spec Doc
worker = return . pretty
