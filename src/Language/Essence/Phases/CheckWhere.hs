{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.CheckWhere where

import Control.Applicative
import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.Writer ( MonadWriter )
import Control.Monad.State ( MonadState )
-- import Control.Monad.IO.Class ( MonadIO, liftIO )

import ParsePrint ( pretty )
import PrintUtils ( (<+>), Doc )

import GenericOps.Core ( BindingsMap )
import Language.Essence.Expr
import Language.Essence.Value
import Language.Essence.Where
import Language.EssenceEvaluator ( deepSimplify )



checkWhere ::
    ( Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState BindingsMap m
    , MonadWriter [Doc] m
    ) => Where -> m ()
-- checkWhere (Where p@(EOp HasType [a,b])) = do
--     ta <- typeOf a
--     tb <- typeOf b
--     if ta `typeUnify` tb
--         then return ()
--         else throwError $ "Type mismatch in where statement:" <+> pretty p
checkWhere (Where x) = do
    x' <- deepSimplify x
    case x' of
        V (VBool True ) -> return ()
        V (VBool False) -> do
            -- tell $ return $ vcat $ ("where statement evaluated to false:" <+> pretty x) : map (nest 4) logs
            throwError $ "where statement evaluated to false:" <+> pretty x
        _               -> do
            -- tell $ return $ vcat $ ("where statement cannot be fully evaluated:" <+> pretty x) : map (nest 4) logs
            throwError $ "where statement cannot be fully evaluated:" <+> pretty x
