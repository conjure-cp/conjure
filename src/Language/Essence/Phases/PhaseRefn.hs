{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.PhaseRefn where

import Control.Applicative
import Control.Monad ( (<=<), (>=>) )
import Control.Monad.Error ( MonadError, throwError, catchError )
import Control.Monad.State ( MonadState, evalStateT, execStateT, runStateT )
import Control.Monad.Writer ( MonadWriter, tell, runWriterT )
import Data.Either ( lefts )
import Data.Foldable ( forM_ )
import Data.List ( nub )
import Data.Maybe ( catMaybes )
import qualified Data.Map as M

import Constants ( FreshName, mkFreshNames, newRuleVar )
import GenericOps.Core ( GPlate, GNode, BindingsMap, mkG, runMatch, runBind, universe, topDownM )
import Has ( Has, getM, putM, modifyM)
import ParsePrint ( pretty )
import PrintUtils ( Doc, (<+>), text, nest )
import Utils ( concatMapM )
import Utils.MonadList ( MonadList, option, runListT )

import Language.Essence
import Language.Essence.Phases.BubbleUp ( bubbleUp )
import Language.Essence.Phases.CheckWhere ( checkWhere )
import Language.Essence.Phases.CleanUp ( cleanUp )
import Language.Essence.Phases.PostParse ( postParse )
import Language.Essence.Phases.QuanRename ( quanRename )
import Language.EssenceEvaluator ( deepSimplify, runSimplify )



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
                                    ) $ do mapM_ addBinding' builtIns
                                           mapM_ addBinding' $ lefts $ topLevels spec
    results <- flip evalStateT ( bindings :: BindingsMap
                               , qNames   :: [FreshName]
                               ) $ applyRefnsDeepSpec rules spec
    mapM (cleanUp <=< runSimplify) $ map bubbleUp results


applyRefnsDeepSpec ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
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
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> a -> m [a]
applyRefnsDeep rules x = do
    mp  :: BindingsMap <- getM
    nms :: [FreshName] <- getM
    (y,(_,nms',b)) <- flip runStateT ( mp  :: BindingsMap
                                     , nms :: [FreshName]
                                     , False
                                     ) $ runListT $ topDownM tryApply x
    putM nms'
    if b
        then concatMapM (applyRefnsDeep rules) y
        else return [x]
    where
        prettyNoParens :: Expr -> Doc
        prettyNoParens (Q i) = pretty i
        prettyNoParens i     = pretty i

        tryApply ::
            ( Applicative m
            , Has st BindingsMap
            , Has st [FreshName]
            , Has st Bool
            , MonadError Doc m
            , MonadList m
            , MonadState st m
            , MonadWriter [Doc] m
            ) => Expr -> m Expr
        tryApply i = do
            mp :: BindingsMap <- getM

            -- adding the quantified variable to the state.
            case i of
                Q (QuantifiedExpr {quanVar = Left (Identifier qnVar), quanOverDom = Just qnOverDom}) -> do
                    case M.lookup qnVar mp of
                        Just _  -> throwError ("Name is already bound: " <+> text qnVar)
                        Nothing -> modifyM (M.insert qnVar (mkG qnOverDom) :: BindingsMap -> BindingsMap)
                _ -> return ()

            res <- catchError
                       ( do j <- applyRefns rules i
                            case j of
                                [] -> return i
                                _  -> do putM True
                                         tell $ ("***" <+> prettyNoParens i)
                                              : map (nest 4 . ("~~>" <+>) . prettyNoParens) j
                                         option j
                       )
                       (\ e -> do tell [e]
                                  return i
                       )

            -- rest state to init.
            putM mp
            return res


applyRefns ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> Expr -> m [Expr]
applyRefns rs current = do
    -- tell ["[applyRefns]" <+> pretty current]
    let one r = catchError ( Just <$> applyRefn r current )
                           (\ _ -> return Nothing )
                           -- (\ e -> do tell [e]; return Nothing )
    (xs,logs) <- runWriterT $ mapM one rs
    tell logs
    let result = concat $ catMaybes xs
    return result


applyRefn ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => RuleRefn -> Expr -> m [Expr]
applyRefn (RuleRefn {..}) current = do
    mp :: BindingsMap <- getM
    res <- flip evalStateT
                    ( mp :: BindingsMap
                    , [] :: [(GNode,GNode)]
                    , [] :: [GNode]
                    ) $ do
        runMatch refnPattern current
        forM_ refnLocals $ \ l ->
            case l of
                Left  b -> addBinding' b
                Right w -> checkWhere w
        mapM (runBind >=> deepSimplify) refnTemplates
    mapM quanRename res

