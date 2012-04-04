{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.PhaseRefn where

import Control.Applicative
import Control.Monad ( (>=>), forM )
import Control.Monad.Error ( MonadError, throwError, catchError )
import Control.Monad.State ( MonadState, evalStateT, execStateT )
import Control.Monad.Writer ( MonadWriter, tell, runWriterT )
import Data.Either ( lefts )

import Data.List ( nub )
import Data.Maybe ( catMaybes )
import Data.Monoid ( Any(..), getAny )
import qualified Data.Map as M

import Constants ( FreshName, mkFreshNames, newRuleVar )
import Has ( Has, getM, putM, modifyM)
import GenericOps.Core ( GPlate, GNode, BindingsMap, mkG, runMatch, runBind, universe, topDownM )
import PrintUtils ( Doc, (<+>), text )
import Utils.MonadList ( MonadList, option, runListT )

import Language.Essence
import Language.Essence.Phases.BubbleUp ( bubbleUp )
import Language.Essence.Phases.CheckWhere ( checkWhere )
import Language.Essence.Phases.CleanUp ( cleanUp )
import Language.Essence.Phases.PostParse ( postParse )
import Language.Essence.Phases.QuanRename ( quanRename )
import Language.EssenceEvaluator ( runSimplify )



callRefn ::
    ( Applicative m
    , MonadError Doc m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> Spec -> m [Spec]
callRefn rules' specParam = do
    spec <- (postParse >=> runSimplify) specParam
    let qNames = mkFreshNames $ nub [ nm | Identifier nm <- universe spec ]
    let rules = map (scopeIdentifiers newRuleVar) rules'
    (bindings,_) <- flip execStateT ( M.empty :: BindingsMap
                                    , []      :: [(GNode,GNode)]
                                    ) $ mapM_ addBinding' (lefts (topLevels spec))
    results <- flip evalStateT ( bindings :: BindingsMap
                               , qNames   :: [FreshName]
                               ) $ applyRefnsDeepSpec rules spec
    ss <- fmap (map cleanUp) $ mapM runSimplify $ map bubbleUp results
    mapM runSimplify ss


applyRefnsDeepSpec ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> Spec -> m [Spec]
applyRefnsDeepSpec = applyRefnsDeep


applyRefnsDeep ::
    ( GPlate a
    , Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> a -> m [a]
applyRefnsDeep rules x = do
    (y,(b,logs)) <- runWriterT $ runListT $ topDownM tryApply x
    -- (y,(b,logs)) <- runWriterT $ runListT $ bottomUpM tryApply x
    -- error $ show $ map pretty y
    tell logs
    if getAny b
        then return y
        else do tell ["[in applyRefnsDeep] No rule applications."]
                return [x]
    where
        tryApply ::
            ( Applicative m
            , Has st BindingsMap
            , Has st [FreshName]
            , Monad m
            , MonadError Doc m
            , MonadList m
            , MonadState st m
            , MonadWriter (Any,[Doc]) m
            ) => Expr -> m Expr
        tryApply i = do
            mp :: BindingsMap <- getM

            -- adding the quantified variable to the state.
            case i of
                Q (QuantifiedExpr {quanVar = Left (Identifier qnVar), quanOverDom = Just qnOverDom}) -> do
                    case M.lookup qnVar mp of
                        Nothing -> modifyM (M.insert qnVar (mkG qnOverDom) :: BindingsMap -> BindingsMap)
                        Just _  -> throwError ("Name is already bound: " <+> text qnVar)
                _ -> return ()

            -- tell (Any False, ["tryApply:" <+> pretty i])
            res <- catchError
                       ( do (j,logs) <- runWriterT $ runListT $ applyRefns rules i
                            case j of
                                [] -> do tell (Any False, logs); option [i]
                                _  -> do tell (Any True , logs); option j
                            -- tell (Any True, logs)
                            -- return j
                       )
                       (\ e -> do tell (Any False, [e])
                                  return i
                       )

            -- rest state to init.
            putM mp
            return res



-- applyRefnDeep ::
--     ( GPlate a
--     , Applicative m
--     , Monad m
--     , MonadError Doc m
--     , MonadList m
--     ) => RuleRefn -> a -> m a
-- applyRefnDeep rule x = do
--     (y,b) <- runWriterT $ bottomUpM tryApply x
--     if getAny b
--         then return y
--         else throwError "Rule not applied."
--     where
--         tryApply ::
--             ( Applicative m
--             , Monad m
--             , MonadError Doc m
--             , MonadList m
--             , MonadWriter Any m
--             ) => Expr -> m Expr
--         tryApply i = catchError (       tell (Any True ) >> applyRefn rule i )
--                                 (\ _ -> tell (Any False) >> return i         )


applyRefns ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError Doc m
    , MonadList m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> Expr -> m Expr
applyRefns rs current = do
    -- tell ["[applyRefns]" <+> pretty current]
    let one r = catchError ( Just <$> runListT (applyRefn r current) )
                           (\ e -> do tell [e]; return Nothing )
    (xs,logs) <- runWriterT $ mapM one rs
    tell logs
    let result = concat $ catMaybes xs
    case result of
        -- [] -> throwError $ Pr.vcat $ logs ++ ["[in applyRefns] Rules not applied:" <+> pretty current]
        [] -> option []
        _  -> option result


applyRefn ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError Doc m
    , MonadList m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => RuleRefn -> Expr -> m Expr
applyRefn (RuleRefn {..}) current = do
    mp :: BindingsMap <- getM
    (res',_) <- flip evalStateT
                    ( mp :: BindingsMap
                    , [] :: [(GNode,GNode)]
                    , [] :: [GNode]
                    ) $ do
        runMatch refnPattern current
        newvars <- forM refnLocals $ \ l ->
            case l of
                Left b@(Find {}) -> Just <$> runBind b          -- new var in bubble alert!
                Left  b -> addBinding' b >> return Nothing
                Right w -> checkWhere w  >> return Nothing
        res <- option =<< mapM runBind refnTemplates
        return (res,catMaybes newvars)
    res <- quanRename res'
    return res
    -- case (newvars,res) of
    --     ([Find (Identifier nm) dom],Bubble a b bs) -> do
    --         newname:_ <- gets snd; modify (second tail)
    --         let a' = bottomUp (identifierRenamer nm newname) a
    --         let b' = bottomUp (identifierRenamer nm newname) b
    --         return $ Bubble a' b' (Left (Find (Identifier newname) dom) : bs)
    --     _ -> return res -- probably should do more checks. what if two new vars?


-- withLog :: MonadWriter [Doc] m => Doc -> m a -> m a
-- withLog d comp = do
--     tell ["START:" <+> d]
--     res <- comp
--     tell ["END:  " <+> d]
--     return res
