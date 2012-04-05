{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.PhaseRefn where

import Control.Applicative
import Control.Monad ( (<=<), (>=>) )
import Control.Monad.Error ( MonadError, catchError )
import Control.Monad.State ( MonadState, evalStateT, execStateT, runStateT )
import Control.Monad.Writer ( MonadWriter, tell, runWriterT )
import Data.Either ( lefts )
import Data.Foldable ( forM_ )
import Data.List ( nub )
import Data.Maybe ( catMaybes )
import Unsafe.Coerce ( unsafeCoerce )
import qualified Data.Map as M
import qualified Data.Typeable as T

import Constants ( FreshName, mkFreshNames, newRuleVar )
import GenericOps.Core ( GPlate, gplate, GNode(..), BindingsMap, mkG, fromGs, runMatch, runBind, universe )
import Has ( Has, getM, putM )
import ParsePrint ( pretty )
import PrintUtils ( Doc, (<+>), nest )
import Utils ( concatMapM )

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

applyRefnsDeep :: forall a st m .
    ( GPlate a
    , Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> a -> m [a]
applyRefnsDeep rules param = do
    let func = funcFromRules rules
    let
        runner :: GNode -> m [GNode]
        runner gparam = do
            mp  :: BindingsMap <- getM
            nms :: [FreshName] <- getM
            (results,(_,nms',_)) <- flip runStateT ( mp  :: BindingsMap
                                                   , nms :: [FreshName]
                                                   , False
                                                   -- , []  :: [GNode]
                                                   ) $ treeWalker func gparam
            putM nms'
            return results
    results <- runner $ mkG param
    return $ fromGs results


funcFromRules ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    -- , Has st [GNode]
    , Has st Bool
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> Expr -> m [Expr]
funcFromRules rules i = catchError
        ( do
            j <- applyRefns rules i
            case j of
                [] -> return [i]
                _  -> do
                    putM True
                    tell $ ("***" <+> prettyNoParens i)
                         : map (nest 4 . ("~~>" <+>) . prettyNoParens) j
                    return j
        )
        (\ e -> do
                    tell [e]
                    return [i]
        )
    where
        prettyNoParens :: Expr -> Doc
        prettyNoParens (Q x) = pretty x
        prettyNoParens x     = pretty x


treeWalker :: forall m st .
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    -- , Has st [GNode]
    , Has st Bool
    , MonadError Doc m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => (Expr -> m [Expr]) -> GNode -> m [GNode]
treeWalker f gx = do
    putM False
    -- modifyM ((gx:) :: [GNode] -> [GNode])
    result <- case gx of
        GNode tx x' | tx == T.typeOf (undefined :: Expr) -> do
            let x = unsafeCoerce x' :: Expr
            let (children, generate) = gplate x
            results :: [Expr]
                   <- map generate . sequence <$> mapM (treeWalker f) children
            map mkG <$> concatMapM f results
        GNode _  x -> do
            let (children, generate) = gplate x
            map mkG . map generate . sequence <$> mapM (treeWalker f) children
    -- modifyM (tail :: [GNode] -> [GNode])
    b <- getM
    if b
        then putM False >> concatMapM (treeWalker f) result
        else putM False >> return result


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

