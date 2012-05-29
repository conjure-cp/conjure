{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.QuanRename where

import Control.Applicative
import Control.Monad ( (<=<) )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter, tell )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as M

import Has
import Constants ( FreshName, getFreshName, isFreshName )
import GenericOps.Core ( GPlate, topDownM, bottomUpM )
import ParsePrint ( pretty )
import qualified PrintUtils as Pr
import PrintUtils ( (<+>) )

import Language.Essence
import Language.Essence.Phases.InBubbleRename ( inBubbleRename )


quanRename :: forall a m st .
    ( GPlate a
    , Applicative m
    , Has st [FreshName]
    , MonadState st m
    , MonadWriter [Pr.Doc] m
    ) => a -> m a
quanRename inp = do
    outp <- (inBubbleRename <=< bottomUpM worker) inp
    -- tell ["before:" <+> pretty inp]
    -- tell ["after: " <+> pretty outp]
    return outp
    where
        supply :: String -> m (Maybe String)
        supply old = do
            if isFreshName old
                then return Nothing
                else Just <$> getFreshName

        worker :: QuantifiedExpr -> m QuantifiedExpr
        -- worker p@(QuantifiedExpr {quanVar = I (Identifier old)}) = do
        --     mnew <- supply old
        --     return $ case mnew of
        --         Nothing  -> p
        --         Just new -> bottomUp (identifierRenamer old new) p
        worker p@(QuantifiedExpr {quanVar = qnSVar}) = do
            let
                rec :: StructuredVar -> m (M.Map String String)
                rec (I (Identifier old)) = do
                    mnew <- supply old
                    return $ case mnew of
                        Nothing  -> M.empty
                        Just new -> M.singleton old new
                rec (STuple  ps) = M.unions <$> mapM rec ps
                rec (SMatrix ps) = M.unions <$> mapM rec ps

            lu <- rec qnSVar

            -- tell [ "quanRename all:" <+> Pr.text (show lu) ]

            let
                f i@(Identifier nm) = do
                    -- tell [ "    trying:" <+> Pr.text nm ]
                    case M.lookup nm lu of
                        Nothing  -> return i
                        Just nm' -> do
                            -- tell [ "        changed to:" <+> Pr.text nm' ]
                            return $ Identifier nm'

            bottomUpM f p

