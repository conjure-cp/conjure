{-# LANGUAGE FlexibleContexts #-}

module Phases.BubbleUp ( bubbleUp ) where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State as S ( MonadState, runStateT, get, put )
import Data.Default ( Default, def )
import Data.Generics.Uniplate.Direct ( descendM, transform )
import Data.Maybe ( fromMaybe )

import Has
import Language.Essence
import UniqueSupply ( nextUniqueInt )
import Utils


bubbleUp :: (Applicative m, MonadIO m) => Spec -> m Spec
bubbleUp sp = do
    -- liftIO $ putStrLn $ render prSpec sp
    (objective', toPosts1, binds1)
        <- case objective sp of
            Nothing -> return (Nothing, [], [])
            Just (oEnum,x) -> do
                (x',(p,b)) <- runStateT (bubbleUpConstraint x) ([],[])
                return (Just (oEnum,x'),p,b)
    (constraints',(toPosts2,binds2)) <- runStateT (mapM bubbleUpConstraint (constraints sp)) ([],[])

    let sp' = sp { topLevelBindings = concat $ topLevelBindings sp : binds1   ++ binds2
              , constraints      = concat $ constraints'        : toPosts1 ++ toPosts2
              , objective        = objective'
              }
    -- liftIO $ putStrLn $ render prSpec sp'
    return sp'

bubbleUpConstraint ::
    ( Applicative m
    , MonadIO m
    , MonadState ( [[Expr]]
                 , [[Binding]]
                 ) m
    ) => Expr -> m Expr
bubbleUpConstraint (Bubble actual toPost binds) = do
    -- liftIO $ putStrLn $ "in Bubble"
    modifyM (toPost:)
    modifyM (binds :)
    -- liftIO $ putStrLn $ "taken bubble out: " ++ render prExpr actual
    bubbleUpConstraint actual

bubbleUpConstraint (ExprQuantifier qnName qnVar qnOver qnGuardParam qnBody) = do
    let qnGuard = fromMaybe Underscore qnGuardParam
    -- liftIO $ putStrLn $ "in ExprQuantifier Nothing"
    ((qnOver',qnGuard',qnBody'),(xs,bs)) <- withDefState $ (,,)
                                                  <$> bubbleUpConstraint qnOver
                                                  <*> bubbleUpConstraint qnGuard
                                                  <*> bubbleUpConstraint qnBody

    -- bs needs lifting -> those bindings defined in bs should be lifted to matrix indexed by [qnOver'] of ?
    let bsLifted = [ (bEnum,nm,DomainMatrix qnOver' dom) | (bEnum,nm,dom) <- concat bs ]
    modifyM (bsLifted :)


    -- xs needs lifting -> replace i -> m[i] in zip oldbs newbs
    q <- (\ i -> Identifier ("UQ_" ++ show i)) <$> liftIO nextUniqueInt
    let liftAux ind (Identifier nm) | nm `elem` map snd3 (concat bs) = GenericNode Index [Identifier nm, ind]
        liftAux _   t = t
    let xsLifted =
            [ ExprQuantifier
                (Identifier "forAll")
                q
                qnOver'
                Nothing
                (flip transform x (\ t -> case t of
                                            Identifier nm | nm `elem` map snd3 (concat bs) -> GenericNode Index [Identifier nm, q]
                                            _ | t == qnVar -> q
                                            _ -> t
                                  ))
            | x <- concat xs ]
    modifyM (xsLifted :)

    -- when ((concat xs,concat bs) /= def) $ liftIO $ ppPrint (xs,bs)
    -- when ((xsLifted,bsLifted) /= def) $ liftIO $ ppPrint (xsLifted,bsLifted)

    let qnGuardOut = case qnGuardParam of Nothing -> Nothing
                                          Just _  -> Just qnGuard'
    return $ ExprQuantifier qnName qnVar qnOver' qnGuardOut (transform (liftAux qnVar) qnBody')

bubbleUpConstraint x = descendM bubbleUpConstraint x


withDefState :: (Default st, MonadState st m) => m a -> m (a, st)
withDefState comp = do
    st <- S.get
    S.put def
    result <- comp
    st' <- S.get
    S.put st
    return (result,st')
