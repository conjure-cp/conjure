{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.PhaseRefn where

import Control.Applicative
import Control.Monad ( (<=<), (>=>), when )
import Control.Monad.Error ( MonadError, catchError )
import Control.Monad.State ( MonadState, evalStateT, execStateT, runStateT )
import Control.Monad.Writer ( MonadWriter, tell, runWriterT )
import Data.Either ( lefts )
import Data.Foldable ( forM_ )
import Data.Function ( on )
import Data.List ( groupBy, sortBy, nub )
import Data.Maybe ( catMaybes )
import Unsafe.Coerce ( unsafeCoerce )
import qualified Data.Map as M
import qualified Data.Typeable as T

import Nested
import Constants ( FreshName, mkFreshNames, newRuleVar )
import GenericOps.Core ( GPlate, gplate, GNode(..), BindingsMap, mkG, fromGs, runMatch, runBind, universe )
import Has ( Has, getM, putM )
import ParsePrint ( pretty )
import PrintUtils ( Doc, (<+>), nest )
import qualified PrintUtils as Pr
import Utils ( concatMapM, ppShow )

import Language.Essence
-- import Language.Essence.Phases.BubbleUp ( bubbleUp )
import Language.Essence.Phases.CheckWhere ( checkWhere )
import Language.Essence.Phases.CleanUp ( cleanUp )
import Language.Essence.Phases.QuanRename ( quanRename )
import Language.EssenceEvaluator ( deepSimplify, oldDeepSimplify, runSimplify )



callRefn ::
    ( Applicative m
    , MonadError (Nested Doc) m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> Spec -> m [Spec]
callRefn rules' spec = do
    let qNames = mkFreshNames $ nub [ nm | Identifier nm <- universe spec ]
    let rules = map (scopeIdentifiers newRuleVar) rules'
    (bindings,_) <- flip execStateT ( M.empty :: BindingsMap
                                    , []      :: [(GNode,GNode)]
                                    ) $ do mapM_ addBinding' builtIns
                                           mapM_ addBinding' $ lefts $ topLevels spec
    results <- flip evalStateT ( bindings :: BindingsMap
                               , qNames   :: [FreshName]
                               ) $ applyRefnsDeepSpec (groupRuleRefns rules) spec
    -- mapM (cleanUp <=< runSimplify) $ map bubbleUp results
    mapM (cleanUp <=< runSimplify) $ results
    -- mapM cleanUp results
    -- return results

groupRuleRefns :: [RuleRefn] -> [[RuleRefn]]
groupRuleRefns = groupBy ((==) `on` refnLevel) . sortBy (compare' `on` refnLevel)
    where
        compare' Nothing  Nothing  = EQ
        compare' Nothing  (Just _) = GT
        compare' (Just _) Nothing  = LT
        compare' (Just a) (Just b) = compare a b

applyRefnsDeepSpec ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [[RuleRefn]] -> Spec -> m [Spec]
applyRefnsDeepSpec = applyRefnsDeep

applyRefnsDeep :: forall a st m .
    ( GPlate a
    , Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [[RuleRefn]] -> a -> m [a]
applyRefnsDeep rules param = do
    let func = funcFromRules rules
    let gparam = mkG param

    mp  :: BindingsMap <- getM
    nms :: [FreshName] <- getM
    (results,(_,nms',_,GlobalFlag b))
        <- runStateT (treeWalker func gparam)
                     ( mp  :: BindingsMap
                     , nms :: [FreshName]
                     , LocalFlag False
                     , GlobalFlag False
                     ) 
    putM nms'
    if b
        then return $ fromGs results
        else return []


funcFromRules ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    -- , Has st [GNode]
    , Has st LocalFlag
    , Has st GlobalFlag
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [[RuleRefn]] -> Expr -> m [Expr]
funcFromRules rules i' = catchError
        ( do
            -- tell ["in funcFromRules"]
            -- apply the simplifier first.
            (i,b) <- deepSimplify i'
            when b $ tell [ "***" <+> prettyNoParens i'
                          , nest 4 $ "~~> {Simplifier}" <+> prettyNoParens i
                          ]

            let
                eachGroup rs = do
                    j <- applyRefns rs i
                    case j of
                        [] -> do
                            let lvl = case rs of []    -> "unknown"
                                                 (r:_) -> show $ refnLevel r
                            -- tell [ "didn't apply anthing at level:" <+> text lvl <+> "to" <+> prettyNoParens i ]
                            return Nothing
                        _  -> do
                            putM $ LocalFlag True
                            putM $ GlobalFlag True
                            tell $ ("***" <+> prettyNoParens i)
                                 : map (nest 4 . ("~~>" <+>) . prettyNoParens) j
                            return $ Just j

            let
                firstJust :: forall m . Monad m => [m (Maybe [Expr])] -> m [Expr]
                firstJust [] = return [i]
                firstJust (ma:mas) = do
                    a <- ma
                    case a of
                        Nothing -> firstJust mas
                        Just a' -> return a'

            firstJust (map eachGroup rules)
        )
        (\ e -> do
                    tell [nestedToDoc e]
                    return [i']
        )
    where
        prettyNoParens :: Expr -> Doc
        prettyNoParens (Q x) = pretty x
        prettyNoParens x     = pretty x


newtype LocalFlag = LocalFlag Bool
newtype GlobalFlag = GlobalFlag Bool

treeWalker :: forall m st .
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    -- , Has st [GNode]
    , Has st LocalFlag
    , Has st GlobalFlag
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => (Expr -> m [Expr]) -> GNode -> m [GNode]
treeWalker f gx = do
    putM $ LocalFlag False
    -- modifyM ((gx:) :: [GNode] -> [GNode])
    result <- case gx of
        GNode tx x' | tx == T.typeOf (undefined :: Expr) -> do
            -- tell ["treeWalker:" <+> text (show tx) <+> pretty x']
            let x = unsafeCoerce x' :: Expr
            let (children, generate) = gplate x
            results :: [Expr]
                   <- map generate . sequence <$> mapM (treeWalker f) children
            -- tell ["treeWalker trying:" <+> Pr.vcat (map pretty results)]
            map mkG <$> concatMapM f results
        GNode tx  x -> do
            -- tell ["treeWalker:" <+> text (show tx) <+> pretty x]
            let (children, generate) = gplate x
            map mkG . map generate . sequence <$> mapM (treeWalker f) children
    -- modifyM (tail :: [GNode] -> [GNode])
    LocalFlag b <- getM
    if b
        then concatMapM (treeWalker f) result       -- if any rule is applied here, rerun the thing.
        else return result                          -- if no rule is applied here, just return whatever you have
                                                    --      this would be identical to "return [gx]"


applyRefns ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRefn] -> Expr -> m [Expr]
applyRefns rs current = do
    -- tell ["[applyRefns]" <+> pretty current]
    let one r = catchError ( Just <$> applyRefn r current )
                           (\ _ -> return Nothing )
                           -- (\ e -> do tell [nestedToDoc e]; return Nothing ) -- THIS gives why matching fails.
    (xs,logs) <- runWriterT $ mapM one rs
    tell logs
    let result = concat $ catMaybes xs
    return result


applyRefn ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError (Nested Doc) m
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

        mp :: BindingsMap <- getM
        tell [ Pr.text $ ppShow mp ]

        tell ["tmpl1:" <+> Pr.vcat (map (Pr.nest 4 . pretty) refnTemplates) ]

        tmpl2 <- mapM runBind refnTemplates
        tell ["tmpl2:" <+> Pr.vcat (map (Pr.nest 4 . pretty) tmpl2) ]

        tmpl3 <- mapM oldDeepSimplify tmpl2
        tell ["tmpl3:" <+> Pr.vcat (map (Pr.nest 4 . pretty) tmpl3) ]

        return tmpl3
    mapM quanRename res
    -- return res

