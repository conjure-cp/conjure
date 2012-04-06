{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.ToETyped where

import Control.Applicative
import Control.Monad.Error ( MonadError )

import PrintUtils ( Doc )
import GenericOps.Core ( GPlate, bottomUpM )

import Language.Essence



toETyped :: (Applicative m, MonadError Doc m) => Spec -> m Spec
toETyped = bottomUpM toETypedExpr


toETypedG :: (Applicative m, MonadError Doc m, GPlate a) => a -> m a
toETypedG = bottomUpM toETypedExpr


toETypedExpr :: (Applicative m, MonadError Doc m) => Expr -> m Expr
toETypedExpr (EOp HasDomain [x,D d]) = do
    t <- domToType d
    return $ ETyped x t
toETypedExpr x = return x

