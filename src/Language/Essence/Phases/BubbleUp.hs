{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.BubbleUp where

-- a buble is of the follwoing form: (Bubble x y bindings)
-- y is to be lifted.
-- bindings are to be lifted.

-- (a = Bubble x y bindings) /\ foo => bar
-- ~~>
-- (Bubble (a = x) y bindings) /\ foo => bar
-- ~~>
-- Bubble (a=x /\ foo) y bindings => bar
-- ~~>
-- Bubble (a=x/\foo=>bar) y bindings
-- ~~>
-- a=x/\foo=>bar,
-- y
-- {bindings to the top level.}

-- if bindings pass through a quantified context, they are "lifted up"

import Control.Applicative
import Control.Monad.State ( MonadState, runState, runStateT )
import Data.Default ( Default, def )
import Data.Either ( lefts )
import Data.List ( nub )
import Data.Maybe ( fromJust, maybeToList )

import Has
import Constants ( FreshName, mkFreshNames, getFreshName )
import GenericOps.Core ( universe, bottomUp, descendM )

import Language.Essence.Binding
import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Objective
import Language.Essence.Op ( Op(Index,In) )
import Language.Essence.QuantifiedExpr
import Language.Essence.Spec


-- Spec { language    :: String
--            , version     :: [Int]
--            , topLevels   :: [Either Binding Where]
--            , objective   :: Maybe Objective
--            , constraints :: [Expr]
--            , metadata    :: [Metadata]
--            }

bubbleUp :: Spec -> Spec
bubbleUp spec = spec { constraints = constraints'   ++ concat newcons1  ++ concat newcons2
                     , topLevels   = topLevels spec ++ map Left (concat newbinds1 ++ concat newbinds2)
                     , objective   = case objective' of [ ] -> Nothing
                                                        [x] -> Just $ (fromJust (objective spec)) { objExpr = x }
                                                        _   -> error "multiple objectives?"
                     }
    where
        qNames = mkFreshNames $ nub [ nm | Identifier nm <- universe spec ]
        f :: [Expr] -> ([Expr],[[Expr]],[[Binding]])
        f xs = let (xs',(nc,nb,_)) = flip runState (def,def,qNames) $ mapM bubbleUpCons xs
               in  (xs',nc,nb)
        (constraints', newcons1, newbinds1) = f (constraints spec)
        (objective'  , newcons2, newbinds2) = f (map objExpr $ maybeToList $ objective spec)



bubbleUpCons ::
    ( Applicative m
    , Has st [[Expr]]
    , Has st [[Binding]]
    , Has st [FreshName]
    , MonadState st m
    ) => Expr -> m Expr

bubbleUpCons (Bubble actual bubble bindings) = do
    modifyM ([bubble] :)
    modifyM (lefts bindings :)
    return actual

bubbleUpCons (Q (QuantifiedExpr qnName (Left qnVar) (Just qnOverDom) Nothing qnGuard qnBody)) = do

    let bubbleUpDom dom = descendM bubbleUpCons dom
    let bubbleUpQuanGuard (QuanGuard xs) = QuanGuard <$> mapM bubbleUpCons xs

    names :: [FreshName] <- getM
    ((qnOverDom',qnGuard',qnBody'),(xs,bs,names')) <-
        runStateT ( (,,) <$> bubbleUpDom qnOverDom
                         <*> bubbleUpQuanGuard qnGuard
                         <*> bubbleUpCons qnBody
                  )
                  ( def   :: [[Expr]]
                  , def   :: [[Binding]]
                  , names :: [FreshName]
                  )
    putM names'

    -- bs needs lifting -> those bindings defined in bs should be lifted to matrix indexed by [qnOver'] of ?
    let bsLifted = [ Find i (DMatrix qnOverDom dom)
                   | Find i dom <- concat bs
                   ]
    modifyM (bsLifted :)

    -- xs needs lifting -> replace i -> m[i] in zip oldbs newbs
    q <- getFreshName

    let
        xsLifted :: [Expr]
        xsLifted =
            [ Q ( QuantifiedExpr
                    (Identifier "forAll")
                    (Left (Identifier q))
                    (Just qnOverDom')
                    Nothing
                    (QuanGuard [])
                    (flip bottomUp x $ \ t@(EHole (Identifier nm)) ->
                        if null [ () | Find (Identifier nm') _ <- concat bs, nm == nm' ]
                            then EOp Index [EHole (Identifier nm), EHole (Identifier q)]
                            else t)
                 )
            | x <- concat xs ]

    modifyM (xsLifted :)

    return $ Q $ QuantifiedExpr qnName (Left qnVar) (Just qnOverDom') Nothing qnGuard' qnBody'

bubbleUpCons (Q (QuantifiedExpr qnName qnVar Nothing (Just (In,qnOverExpr)) qnGuard qnBody)) = do

    qnOverExpr' <- bubbleUpCons qnOverExpr

    -- modify $ first $ second (bs :)
    -- modify $ first $ first  ([[xs]] ++)

    return $ Q $ QuantifiedExpr qnName qnVar Nothing (Just (In,qnOverExpr')) qnGuard qnBody

bubbleUpCons p@(Q (QuantifiedExpr {})) = return p

bubbleUpCons p = descendM bubbleUpCons p



-- withState :: MonadState s m => s -> m a -> m (a, s)
-- withState initSt comp = do
--     st <- S.get
--     S.put initSt
--     result <- comp
--     st' <- S.get
--     S.put st
--     return (result,st')
