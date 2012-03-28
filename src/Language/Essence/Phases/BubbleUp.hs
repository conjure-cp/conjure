{-# LANGUAGE FlexibleContexts #-}

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
import Control.Arrow ( first, second )
import Control.Monad.State ( MonadState, modify, get, gets, put, runState )
import Data.Default ( Default, def )
import Data.List ( (\\) )
import Data.Maybe ( fromJust, maybeToList )

import GenericOps.Core ( universe, bottomUp, descendM )

import Language.Essence.Binding
import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Objective
import Language.Essence.Op ( Op(Index) )
import Language.Essence.QuantifiedExpr
import Language.Essence.Spec
import Language.Essence.Where


-- Spec { language    :: String
--            , version     :: [Int]
--            , topLevels   :: [Either Binding Where]
--            , objective   :: Maybe Objective
--            , constraints :: [Expr]
--            , metadata    :: [Metadata]
--            }

bubbleUp :: Spec -> Spec
bubbleUp spec = spec { constraints = constraints'   ++ concat newcons1  ++ concat newcons2
                     , topLevels   = topLevels spec ++ concat newbinds1 ++ concat newbinds2
                     , objective   = case objective' of [ ] -> Nothing
                                                        [x] -> Just $ (fromJust (objective spec)) { objExpr = x }
                                                        _   -> error "multiple objectives?"
                     }
    where
        identifiers = [ nm | Identifier nm <- universe spec ]
        qNames = [ "_q_" ++ show i | i <- [ (0 :: Int) .. ] ] \\ identifiers
        f xs = let (xs',((nc,nb),_)) = flip runState (def,qNames) $ mapM bubbleUpCons xs
               in  (xs',nc,nb)
        (constraints', newcons1, newbinds1) = f (constraints spec)
        (objective'  , newcons2, newbinds2) = f (map objExpr $ maybeToList $ objective spec)



bubbleUpCons ::
    ( Applicative m
    , MonadState ( ( [ [Expr] ]
                   , [ [Either Binding Where] ]
                   )
                 , [String]
                 ) m
    ) => Expr -> m Expr

bubbleUpCons (Bubble actual bubble bindings) = do
    modify $ first $ first  ([bubble] :)
    modify $ first $ second (bindings :)
    return actual

bubbleUpCons (Q (QuantifiedExpr qnName qnVar (Just qnOverDom) Nothing qnGuard qnBody)) = do

    let bubbleUpDom dom = descendM bubbleUpCons dom
    let bubbleUpQuanGuard (QuanGuard xs) = QuanGuard <$> mapM bubbleUpCons xs

    names <- gets snd
    ((qnOverDom',qnGuard',qnBody'),((xs,bs),names')) <-
        withState (def,names) $ (,,)
            <$> bubbleUpDom qnOverDom
            <*> bubbleUpQuanGuard qnGuard
            <*> bubbleUpCons qnBody
    modify $ second (const names')

    -- bs needs lifting -> those bindings defined in bs should be lifted to matrix indexed by [qnOver'] of ?
    let bsLifted = [ Left (Find i (DMatrix qnOverDom dom))
                   | Left (Find i dom) <- concat bs
                   ]
    modify $ first $ second (bsLifted :)

    -- xs needs lifting -> replace i -> m[i] in zip oldbs newbs
    q:_ <- gets snd
    modify $ second tail
    
    let
        xsLifted =
            [ Q ( QuantifiedExpr
                    (Identifier "forall")
                    (Identifier q)
                    (Just qnOverDom')
                    Nothing
                    (QuanGuard [])
                    (flip bottomUp x $ \ t@(EHole (Identifier nm)) ->
                        if null [ () | Left (Find (Identifier nm') _) <- concat bs, nm == nm' ]
                            then EOp Index [EHole (Identifier nm), EHole (Identifier q)]
                            else t)
                 )
            | x <- concat xs ]

    modify $ first $ first (xsLifted :)

    return $ Q $ QuantifiedExpr qnName qnVar (Just qnOverDom') Nothing qnGuard' qnBody'

bubbleUpCons p = descendM bubbleUpCons p



withState :: MonadState s m => s -> m a -> m (a, s)
withState initSt comp = do
    st <- get
    put initSt
    result <- comp
    st' <- get
    put st
    return (result,st')
