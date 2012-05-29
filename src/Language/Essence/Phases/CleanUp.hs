{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.CleanUp where

import Prelude hiding ( mapM )
import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState, get, put, evalStateT )
import Data.List ( nub, isPrefixOf )
import Data.Maybe ( catMaybes )
import Data.Either ( rights )
import Data.Traversable ( mapM )
import qualified Data.Map as M

import Nested
import Constants ( FreshName, mkPrettyFreshNames, getFreshName )
import GenericOps.Core ( GPlate, universe, bottomUp )
import PrintUtils ( Doc )

import Language.Essence


isReferencedIn :: GPlate a => Identifier -> a -> Bool
isReferencedIn (Identifier nm) x = not $ null [ () | Identifier nm' <- universe x
                                                   , or [ nm == nm'
                                                        , isPrefixOf (nm ++ "#") nm'
                                                        ]
                                                   ]

isReferencedIn' :: GPlate a => Identifier -> [a] -> Bool
isReferencedIn' iden xs = or [isReferencedIn iden x | x <- xs]

removeNonreferenced :: Spec -> Spec
removeNonreferenced spec =
    let
        checkOneIdentifier i = or [ isReferencedIn' i (constraints spec)
                                  , isReferencedIn' i (rights $ topLevels spec)
                                  ]
                        
        checkOneBinding (Find  i _) = checkOneIdentifier i
        checkOneBinding (Given i _) = checkOneIdentifier i
        checkOneBinding _ = True

        newTopLevels = flip map (topLevels spec) $ \ tl -> case tl of
                        Left b -> if checkOneBinding b
                                    then Just $ Left b
                                    else Nothing
                        Right w -> Just (Right w)
    in spec { topLevels = catMaybes newTopLevels }


cleanUp :: (Applicative m, MonadError (Nested Doc) m) => Spec -> m Spec
cleanUp spec = return (removeNonreferenced $ topLevelConstraints spec)
-- cleanUp spec = quanRenameFinal (topLevelConstraints spec)


topLevelConstraints :: Spec -> Spec
topLevelConstraints spec = spec { constraints = nub
                                              $ filter (/=V (VBool True))
                                              $ concatMap conjunct'
                                              $ constraints spec
                                }
    where
        conjunct' :: Expr -> [Expr]
        conjunct' (EOp And [x,y]) = conjunct' x ++ conjunct' y
        conjunct' x = [x]


quanRenameFinal ::
    ( Applicative m
    , MonadError (Nested Doc) m
    ) => Spec -> m Spec
quanRenameFinal spec = do
    let qNames = mkPrettyFreshNames $ nub [ nm | Identifier nm <- universe spec ]
    flip evalStateT qNames $ do
        topLevels'   <- mapM (withState . workerTL  ) (topLevels   spec)
        constraints' <- mapM (withState . workerExpr) (constraints spec)
        objective'   <- mapM (withState . workerObj ) (objective   spec)
        return $ spec { topLevels  = topLevels'
                      , objective  = objective'
                      , constraints = constraints'
                      }

    where
        workerExpr :: (Applicative m, MonadError (Nested Doc) m, MonadState [FreshName] m) => Expr -> m Expr
        workerExpr (Q       x) = Q <$> workerQ x
        workerExpr (Bubble x y zs) = Bubble <$> (withState $ workerExpr x)
                                            <*> (withState $ workerExpr y)
                                            <*> (mapM (withState . workerTL) zs)
        workerExpr (EOp op xs) = EOp op <$> mapM (withState . workerExpr) xs
        workerExpr x           = return x

        workerObj :: (Applicative m, MonadError (Nested Doc) m, MonadState [FreshName] m) => Objective -> m Objective
        workerObj (OMin x) = OMin <$> workerExpr x
        workerObj (OMax x) = OMax <$> workerExpr x

        workerTL :: (Applicative m, MonadError (Nested Doc) m, MonadState [FreshName] m) => Either Binding Where -> m (Either Binding Where)
        workerTL (Left (LettingExpr i x))  = Left  . LettingExpr i  <$> (withState . workerExpr) x
        workerTL (Left (BindingDimFind x)) = Left  . BindingDimFind <$> (withState . workerExpr) x
        workerTL (Left  b)                 = Left                   <$> return b
        workerTL (Right (Where w))         = Right . Where          <$> (withState . workerExpr) w

        workerQ :: (Applicative m, MonadError (Nested Doc) m, MonadState [FreshName] m) => QuantifiedExpr -> m QuantifiedExpr
        workerQ p@(QuantifiedExpr {quanVar = I (Identifier old)}) = do
            new <- getFreshName
            let p' = bottomUp (identifierRenamer old new) p
            guard' <- (withState . workerG)    $ quanGuard p'
            body'  <- (withState . workerExpr) $ quanBody p'
            return $ p' { quanBody = body', quanGuard = guard' }
        workerQ p@(QuantifiedExpr {quanVar = qnSVar}) = do
            let
                -- rec :: StructuredVar -> m (M.Map String String)
                rec (I (Identifier old)) = do
                    new <- getFreshName
                    return $ M.singleton old new
                rec (STuple  ps) = M.unions <$> mapM rec ps
                rec (SMatrix ps) = M.unions <$> mapM rec ps

            lu <- rec qnSVar

            let f i@(Identifier nm) = case M.lookup nm lu of
                                            Nothing  -> i
                                            Just new -> Identifier new

            let p' = bottomUp f p
            guard' <- (withState . workerG)    $ quanGuard p'
            body'  <- (withState . workerExpr) $ quanBody p'
            return $ p' { quanBody = body', quanGuard = guard' }

        workerG :: (Applicative m, MonadError (Nested Doc) m, MonadState [FreshName] m) => QuanGuard -> m QuanGuard
        workerG (QuanGuard [x]) = do
            x' <- workerExpr x
            return $ QuanGuard [x']
        workerG p = return p

withState :: MonadState s m => m a -> m a
withState comp = do
    st <- get
    res <- comp
    put st
    return res
