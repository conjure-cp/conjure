module Language.Core.Middleware.Combined.All ( worker ) where

import Language.Core.Definition

import Language.Core.Middleware ( Middleware )

import qualified Language.Core.Middleware.AtMostOneSuchThat as AtMostOneSuchThat ( worker )

worker :: Monad m => Middleware m Spec Spec
worker = AtMostOneSuchThat.worker
