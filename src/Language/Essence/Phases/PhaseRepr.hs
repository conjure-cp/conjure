{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.PhaseRepr where

import Control.Applicative
import Control.Arrow ( (&&&) )
import Control.Monad ( (<=<), (>=>), msum, when )
import Control.Monad.Error ( MonadError, catchError )
import Control.Monad.State ( MonadState, StateT, evalStateT, execStateT, runStateT )
import Control.Monad.Writer ( MonadWriter, tell, WriterT, runWriterT )
import Data.Either ( lefts, rights )
import Data.Foldable ( forM_ )
import Data.Function ( on )
-- import Data.Generics ( Data )
import Data.List ( group, sort, groupBy, sortBy, nub )
import Data.Maybe ( catMaybes, maybeToList, isNothing )
import Data.Ord ( comparing )
import Data.Traversable ( forM )
-- import Data.Typeable ( Typeable )
-- import GHC.Generics ( Generic )
import qualified Data.Map as M
import qualified Data.Set as S

import Nested
import Constants ( FreshName, mkFreshNames, newRuleVar )
import GenericOps.Core ( GNode, BindingsMap, runMatch, runBind, universe, bottomUp, bottomUpM )
import Has ( Has, getM, modifyM )
import ParsePrint ( ParsePrint, pretty, prettyListDoc )
import PrintUtils ( (<>), (<+>), text, Doc )
import qualified PrintUtils as Pr
import Utils ( allPairs, fst3 )
import Utils.MonadList ( MonadList, option, runListT )

import Language.Essence
import Language.Essence.Phases.BubbleUp ( bubbleUp )
import Language.Essence.Phases.CheckWhere ( checkWhere )
import Language.Essence.Phases.CleanUp ( cleanUp )
import Language.Essence.Phases.PostParse ( postParse )
import Language.Essence.Phases.QuanRename ( quanRename )
import Language.EssenceEvaluator ( oldDeepSimplify, runSimplify )



callRepr ::
    ( Applicative m
    , MonadError (Nested Doc) m
    , MonadWriter [Doc] m
    ) => [RuleRepr] -> Spec -> m [Spec]
callRepr rules' specParam = do
    spec <- (postParse >=> runSimplify) specParam
    let qNames = mkFreshNames $ nub [ nm | Identifier nm <- universe spec ]
    let rules = map (scopeIdentifiers newRuleVar) rules'
    (bindings,_) <- flip execStateT ( M.empty :: BindingsMap
                                    , [] :: [(GNode,GNode)]
                                    ) $ mapM_ addBinding' (lefts (topLevels spec))
    results      <- flip evalStateT ( bindings :: BindingsMap
                                    , qNames   :: [FreshName]
                                    ) $ applyReprsToSpec rules spec
    ss <- mapM (cleanUp <=< runSimplify) $ map bubbleUp results
    return ss
    -- mapM runSimplify =<< concatMapM (quanDomRefine rules) =<< mapM runSimplify ss


type ReprResult = ( String     -- name of the representation
                  , Domain     -- replacement domain
                  , [Expr]     -- structural constraints
                  )

applyReprsToSpec ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRepr]
      -> Spec
      -> m [Spec]
applyReprsToSpec rules spec = do
    let
        remainingBindings :: M.Map String Domain
        remainingBindings = M.fromList $
            [ (nm, dom) | Find (Identifier nm) dom <- lefts (topLevels spec)
                        , needsRepresentation dom
                        , isNothing $ representationValue dom ]
            ++
            [ (nm, dom) | Given (Identifier nm) dom <- lefts (topLevels spec)
                        , needsRepresentation dom
                        , isNothing $ representationValue dom ]

    candidates :: M.Map String [ReprResult]
               <- forM remainingBindings $ applyReprsToDom rules

    let
        counts :: M.Map String Int
        counts =
            let lu = M.keysSet remainingBindings
            in  M.fromList
              $ map (head &&& length)
              . group
              . sort
              $  [ nm | Identifier nm <- concatMap universe (maybeToList $ objective spec), S.member nm lu ]
              ++ [ nm | Identifier nm <- concatMap universe (            constraints spec), S.member nm lu ]
              ++ [ nm | Identifier nm <- concatMap universe (     rights $ topLevels spec), S.member nm lu ]

    if M.null remainingBindings
        then return []
        else do
            forM_ (M.toList candidates) $ \ (nm,rs) -> case rs of
                [] -> throwErrorSingle $ text nm <+> "needs a representation, but no rule matches it."
                _  -> return ()

            tell [ Pr.vcat $ "Representation options:"
                           : map (\ (nm,rs) -> Pr.nest 4 $ text nm <> Pr.colon
                                                                  <+> prettyListDoc id Pr.comma (map (text . fst3) rs)
                                 ) (M.toList candidates)
                 ]

            tell [ Pr.vcat $ "Number of occurrences in the specification:"
                           : map (\ (nm,c) -> Pr.nest 4 $ text nm <> Pr.colon <+> Pr.int c ) (M.toList counts)
                 ]

            let
                f :: ( MonadList m
                     , MonadWriter ([Binding],[Expr]) m
                     , MonadState [(String,String)] m                -- those added. name reprName pair.
                     ) => Expr -> m Expr
                f p@(EHole (Identifier nm)) = case M.lookup nm candidates of
                    Nothing -> return p
                    Just rs -> do
                        (nmR,dom,xs) <- option rs
                        st :: [(String,String)] <- getM
                        when ((nm,nmR) `notElem` st) $ do
                            let refn = nm ++ "_" ++ nmR
                            modifyM ((nm,nmR):)
                            tell ( map (bottomUp (identifierRenamer "refn" refn)) [ Find (Identifier refn) dom ]
                                 , map (bottomUp (identifierRenamer "refn" refn)) xs
                                 )
                        return $ EHole $ Identifier $ nm ++ "#" ++ nmR
                f p = return p

            results <- runListT $ flip runStateT [] $ runWriterT $ bottomUpM f spec
            forM results $ \ ((sp,(bs,xs)),stuffAdded) -> do
                let
                    channels :: [(String,[String])]
                    channels
                        = filter (\ (_,i) -> length i > 1 )                    -- remove those which have only one repr
                        $ map (\ i -> (fst (head i), map snd i) )
                        $ groupBy ((==) `on` fst)
                        $ sortBy (comparing fst)
                          stuffAdded
                    channelCons :: [Expr]
                    channelCons
                        = map (\ (a,b) -> EOp Eq [ EHole (Identifier a)
                                                 , EHole (Identifier b) ] )
                        $ concatMap allPairs
                        $ map (\ (nm,rs) -> [ nm ++ "_" ++ r | r <- rs ] )
                          channels
                return sp { topLevels   = topLevels   sp ++ map Left bs
                          , constraints = constraints sp ++ xs ++ channelCons
                          }


applyReprsToDom ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRepr]
      -> Domain
      -> m [ReprResult]
applyReprsToDom rules dom = withLog ("applyReprsToDom     " <+> pretty dom)
                          $ catMaybes <$> mapM tryApply rules
    where
        tryApply rule = catchError ( Just <$> applyReprToDom rule dom )
                                   ( \ _ -> return Nothing )


applyReprToDom ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => RuleRepr
      -> Domain
      -> m ReprResult
applyReprToDom rule dom = do
    candidates <- mapM tryApply (reprCases rule)
    case msum candidates of
        Nothing -> throwErrorSingle "Rule not applied."
        Just r  -> return r

    where
        tryApply rulecase = catchError ( Just <$> applyReprCaseToDom rule rulecase dom )
                                       ( \ _ -> return Nothing )


applyReprCaseToDom ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , Monad m
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => RuleRepr
      -> RuleReprCase
      -> Domain
      -> m ReprResult
applyReprCaseToDom (RuleRepr {..}) (RuleReprCase {..}) dom = do
    mp :: BindingsMap <- getM
    (tmpl, str) <- flip evalStateT ( mp :: BindingsMap
                                   , [] :: [GNode]
                                   , [] :: [(GNode,GNode)]
                                   ) $ do
        runMatch reprCasePattern dom
        forM_ (reprCaseLocals ++ reprLocals) $ \ l ->
            case l of
                Left  b -> withLog "addBinding" $ addBinding' b
                Right w -> withLog "checkWhere" $ checkWhere w
        tmpl <- runBind reprTemplate
        str  <- mapM (oldDeepSimplify <=< runBind) (maybeToList reprStructural ++ maybeToList reprCaseStructural)
        return (tmpl, str)
    tmpl' <- quanRename tmpl
    str'  <- mapM quanRename str
    return (reprName,tmpl',str')


withLog :: MonadWriter [Doc] m => Doc -> m a -> m a
withLog _ = id
-- withLog d comp = do
--     tell ["START:" <+> d]
--     res <- comp
--     tell ["END:  " <+> d]
--     return res
