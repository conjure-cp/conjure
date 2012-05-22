{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.QuanDomRefine where

import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState, evalStateT )
import Control.Monad.Writer ( MonadWriter )
import Data.Default ( def )
import Data.List ( nub )

import Has
import Nested
import Constants ( FreshName, mkFreshNames )
import GenericOps.Core ( BindingsMap, universe, bottomUp, topDownM )
import PrintUtils ( Doc )
import Utils.MonadList( MonadList, option, runListT )

import Language.Essence
import Language.Essence.Phases.PhaseRepr ( applyReprsToDom )


-- refining quantification over complex domains.
quanDomRefine ::
    ( Applicative m
    , MonadError (Nested Doc) m
    , MonadWriter [Doc] m
    ) => [RuleRepr] -> Spec -> m [Spec]
quanDomRefine rules spec = do
    let qNames = mkFreshNames $ nub [ nm | Identifier nm <- universe spec ]
    flip evalStateT ( def    :: BindingsMap
                    , qNames :: [FreshName]
                    ) $ runListT $ topDownM (domRefineQ rules) spec

domRefineQ ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [FreshName]
    , MonadError (Nested Doc) m
    , MonadList m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => [RuleRepr] -> QuantifiedExpr -> m QuantifiedExpr
domRefineQ rules q@(QuantifiedExpr qnName (I (Identifier qnVar)) (Just qnOverDom) Nothing (QuanGuard qnGuard) qnBody) = do
    results <- applyReprsToDom rules qnOverDom
    case results of
        [] -> return q
        _  -> do
            (nmR,newDom,str) <- option results
            -- in qnGuard and qnBody, replace references to qnVar with qnVar#nmR
            let refn = qnVar ++ "_" ++ nmR

            let newDom' =      bottomUp (identifierRenamer "refn" refn)  newDom
            let str'    = map (bottomUp (identifierRenamer "refn" refn)) str

            let refnHash = qnVar ++ "#" ++ nmR
            let qnGuard' = map (bottomUp (identifierRenamer qnVar refnHash)) qnGuard
            let qnBody'  =      bottomUp (identifierRenamer qnVar refnHash)  qnBody

            let theGuard = conjunct $ str' ++ qnGuard'

            return $ QuantifiedExpr qnName (I (Identifier refn)) (Just newDom') Nothing (QuanGuard [theGuard]) qnBody'
domRefineQ _ q = return q



conjunct :: [Expr] -> Expr
conjunct [] = error "conjunct []"
conjunct [x] = x
conjunct (x:xs) = EOp And [conjunct xs,x]
