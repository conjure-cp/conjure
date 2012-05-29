{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.CheckWhere where

import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.Writer ( MonadWriter )
import Control.Monad.State ( MonadState )
-- import Control.Monad.IO.Class ( MonadIO, liftIO )

import Has
import Nested
import ParsePrint ( pretty )
import PrintUtils ( (<+>), Doc )

import GenericOps.Core ( GNode, BindingsMap )
import Language.Essence.Expr
import Language.Essence.Value
import Language.Essence.Where
import Language.EssenceEvaluator ( deepSimplify )



checkWhere ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [GNode]
    , Has st [(GNode,GNode)]
    , Monad m
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => Where -> m ()
-- checkWhere (Where p@(EOp HasType [a,b])) = do
--     ta <- typeOf a
--     tb <- typeOf b
--     if ta `typeUnify` tb
--         then return ()
--         else throwError $ "Type mismatch in where statement:" <+> pretty p
checkWhere (Where x) = do
    (x',_) <- deepSimplify x
    case x' of
        V (VBool True ) -> return ()
        V (VBool False) -> do
            throwErrorSingle $ "where statement evaluated to false:" <+> pretty x
        _               -> do
            throwErrorSingle $ "where statement cannot be fully evaluated:" <+> pretty x
