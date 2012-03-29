{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.QuanDomRefine where

import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState, evalStateT )
import Control.Monad.Writer ( MonadWriter )
import Data.Default ( def )
import Data.List ( (\\) )

import Constants ( freshNames )
import GenericOps.Core ( BindingsMap, universe, bottomUp, topDownM )
import PrintUtils ( Doc )
import Utils.MonadList( MonadList, option, runListT )

import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Op
import Language.Essence.QuantifiedExpr
import {-# SOURCE #-} Language.Essence.RuleRepr
import Language.Essence.Spec
import Language.Essence.Phases.QuanRename



-- refining quantification over complex domains.
quanDomRefine ::
    ( Applicative m
    , MonadError Doc m
    , MonadWriter [Doc] m
    ) => [RuleRepr] -> Spec -> m [Spec]
quanDomRefine rules spec = do
    let identifiers = [ nm | Identifier nm <- universe spec ]
    let qNames = freshNames \\ identifiers
    flip evalStateT (def,qNames) $ runListT $ topDownM (domRefineQ rules) spec

domRefineQ ::
    ( Applicative m
    , MonadError Doc m
    , MonadList m
    , MonadState (BindingsMap,[String]) m
    , MonadWriter [Doc] m
    ) => [RuleRepr] -> QuantifiedExpr -> m QuantifiedExpr
domRefineQ rules q@(QuantifiedExpr qnName (Left (Identifier qnVar)) (Just qnOverDom) Nothing (QuanGuard qnGuard) qnBody) = do
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

            return $ QuantifiedExpr qnName (Left (Identifier refn)) (Just newDom') Nothing (QuanGuard [theGuard]) qnBody'
domRefineQ _ q = return q



conjunct :: [Expr] -> Expr
conjunct [] = error "conjunct []"
conjunct [x] = x
conjunct (x:xs) = EOp And [conjunct xs,x]
