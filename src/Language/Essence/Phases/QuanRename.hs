{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.QuanRename where

import Control.Monad.State ( MonadState, get, put )

import Constants ( isFreshName )
import GenericOps.Core ( GPlate, topDownM, bottomUp )

import Language.Essence.Identifier
import Language.Essence.QuantifiedExpr



quanRename :: forall a f m . (GPlate a, Functor m, MonadState (f,[String]) m) => a -> m a
quanRename = topDownM quanRenamer
    where
        supply :: m String
        supply = do
            (f,s:ss) <- get
            put (f,ss)
            return s

        quanRenamer :: QuantifiedExpr -> m QuantifiedExpr
        quanRenamer p@(QuantifiedExpr {quanVar = Identifier qnVar}) = do
            if isFreshName qnVar
                then return p
                else do
                    nm <- supply
                    return $ bottomUp (identifierRenamer qnVar nm) p
        -- quanRenamer p@(QStructured {quanSVar = ?})


identifierRenamer :: String -> String -> Identifier -> Identifier
identifierRenamer oldName newName (Identifier nm) | oldName == nm = Identifier newName
identifierRenamer _ _ p = p
