{-# LANGUAGE FlexibleContexts #-}

module Language.EssenceEvaluator ( runEvaluateExpr ) where


import Control.Monad.RWS ( RWS, evalRWS
                         , MonadReader, ask
                         , MonadWriter, tell
                         )
import Data.Generics.Uniplate.Direct ( rewriteBiM )
import Data.List ( sort, intersect, union )
import Data.Maybe ( fromJust )

import Language.Essence
import Language.EssencePrinters ( prExpr )
import PrintUtils ( render )


runEvaluateExpr :: [Binding] -> Expr -> (Expr,[Log])
runEvaluateExpr topLevels x = evalRWS (comp x) topLevels ()
    where
        comp :: Expr -> RWS [Binding] [Log] () Expr
        comp = rewriteBiM combined

        withLog ::
            ( MonadReader [Binding] m
            , MonadWriter [Log] m
            ) => String
              -> (Expr -> m (Maybe Expr))
              -> Expr
              -> m (Maybe Expr)
        withLog msg f i = do
            let p = render . fromJust . prExpr
            mr <- f i
            case mr of
                Nothing -> return Nothing
                Just r  -> do
                    tell [msg ++ ": " ++ p i ++ " ~~> " ++ p r]
                    return mr

        combined ::
            ( MonadReader [Binding] m
            , MonadWriter [Log] m
            ) => Expr -> m (Maybe Expr)
        combined i = do
            j <- withLog "Evaluator " evaluateExpr i
            case j of
                Nothing -> withLog "Normaliser" normaliseExpr i
                Just _  -> return j


evaluateExpr :: MonadReader [Binding] m => Expr -> m (Maybe Expr)

-- full evaluators

evaluateExpr (GenericNode Plus   [ValueInteger i,ValueInteger j])          = rJust $ ValueInteger $ i + j
evaluateExpr (GenericNode Minus  [ValueInteger i,ValueInteger j])          = rJust $ ValueInteger $ i - j
evaluateExpr (GenericNode Times  [ValueInteger i,ValueInteger j])          = rJust $ ValueInteger $ i * j
evaluateExpr (GenericNode Div    [ValueInteger i,ValueInteger j]) | j /= 0 = rJust $ ValueInteger $ div i j
evaluateExpr (GenericNode Mod    [ValueInteger i,ValueInteger j]) | j >  0 = rJust $ ValueInteger $ mod i j
evaluateExpr (GenericNode Pow    [ValueInteger i,ValueInteger j]) | j >  0 = rJust $ ValueInteger $ i ^ j

evaluateExpr (GenericNode Abs    [ValueInteger i]) = rJust $ ValueInteger $ abs i
evaluateExpr (GenericNode Negate [ValueInteger i]) = rJust $ ValueInteger $ negate i

evaluateExpr (GenericNode Lt  [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i <  j
evaluateExpr (GenericNode Leq [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i <= j
evaluateExpr (GenericNode Gt  [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i >  j
evaluateExpr (GenericNode Geq [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i >= j
evaluateExpr (GenericNode Neq [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i /= j
evaluateExpr (GenericNode Eq  [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i == j

evaluateExpr (GenericNode Elem [i,ValueSet  is]) | i `elem` is = rJust $ ValueBoolean True
evaluateExpr (GenericNode Elem [i,ValueMSet is]) | i `elem` is = rJust $ ValueBoolean True

evaluateExpr (GenericNode Intersect [ValueSet is,ValueSet js]) = rJust $ ValueSet $ sort $ is `intersect` js
evaluateExpr (GenericNode Union     [ValueSet is,ValueSet js]) = rJust $ ValueSet $ sort $ is `union`     js

-- partial evaluators

evaluateExpr (GenericNode Plus   [ValueInteger 0,x]) = rJust x
evaluateExpr (GenericNode Plus   [x,ValueInteger 0]) = rJust x
evaluateExpr (GenericNode Minus  [ValueInteger 0,x]) = rJust $ GenericNode Negate [x]
evaluateExpr (GenericNode Times  [ValueInteger 0,_]) = rJust $ ValueInteger 0
evaluateExpr (GenericNode Times  [_,ValueInteger 0]) = rJust $ ValueInteger 0
evaluateExpr (GenericNode Times  [ValueInteger 1,x]) = rJust x
evaluateExpr (GenericNode Times  [x,ValueInteger 1]) = rJust x
evaluateExpr (GenericNode Div    [x,ValueInteger 1]) = rJust x
evaluateExpr (GenericNode Mod    [x,y])     | x == y = rJust $ ValueInteger 0
evaluateExpr (GenericNode Pow    [_,ValueInteger 0]) = rJust $ ValueInteger 1
evaluateExpr (GenericNode Pow    [x,ValueInteger 1]) = rJust x

evaluateExpr (GenericNode Times [x,y]) | x == y
    = rJust $ GenericNode Pow [x,ValueInteger 2]
evaluateExpr (GenericNode Times [GenericNode Pow [x,y],z]) | x == z
    = rJust $ GenericNode Pow [x,GenericNode Plus [y,ValueInteger 1]]

evaluateExpr (GenericNode And [ValueBoolean True ,x]) = rJust x
evaluateExpr (GenericNode And [ValueBoolean False,_]) = rJust $ ValueBoolean False

evaluateExpr (GenericNode Or  [ValueBoolean False,x]) = rJust x
evaluateExpr (GenericNode Or  [ValueBoolean True ,_]) = rJust $ ValueBoolean True

evaluateExpr (GenericNode Imply [ValueBoolean True ,x]) = rJust x
evaluateExpr (GenericNode Imply [ValueBoolean False,_]) = rJust $ ValueBoolean True
evaluateExpr (GenericNode Imply [a,b]) | unifyExpr (GenericNode Not [a]) b = rJust b -- is this too clever?

evaluateExpr (GenericNode Iff [ValueBoolean True ,x]) = rJust x
evaluateExpr (GenericNode Iff [ValueBoolean False,x]) = rJust $ GenericNode Not [x]

-- symbolic full evaluators

evaluateExpr (GenericNode Minus [a,b]) | a == b = rJust $ ValueInteger 0
evaluateExpr (GenericNode Negate [GenericNode Negate [x]]) = rJust x
evaluateExpr (GenericNode Eq [a,b]) | a == b = rJust $ ValueBoolean True

-- some special cases

evaluateExpr (GenericNode Minus [GenericNode Plus [x,y],z])
    | y == z = rJust x
    | x == z = rJust y
evaluateExpr (GenericNode Plus [GenericNode Minus [x,y],z])
    | y == z = rJust x
    | x == z = rJust y

-- no eval

evaluateExpr (Identifier nm) = do
    bs <- ask
    case [ x | (Letting,nm',x) <- bs, nm == nm' ] of
        [x] -> rJust x
        _   -> rNothing

evaluateExpr _ = rNothing


normaliseExpr :: Monad m => Expr -> m (Maybe Expr)
normaliseExpr (GenericNode op [GenericNode op2 [a,b],c])
    | op == op2
    , op `elem` associativeOps
    = let [x,y,z] = sort [a,b,c]
      in  if [x,y,z] == [a,b,c]
              then rNothing
              else rJust $ GenericNode op [GenericNode op [x,y],z]
normaliseExpr (GenericNode op [a,b])
    | op `elem` commutativeOps
    , b < a
    = rJust $ GenericNode op [b,a]
normaliseExpr _ = rNothing


-- return Nothing in a given monad
rNothing :: Monad m => m (Maybe a)
rNothing = return Nothing

-- return Just value in a given monad
rJust :: Monad m => a -> m (Maybe a)
rJust = return . Just


-- do these two expressions unify to the same thing?
unifyExpr :: Expr -> Expr -> Bool
unifyExpr (GenericNode Not [GenericNode Eq [a,b]]) (GenericNode Neq [c,d])
    = unifyExpr a c && unifyExpr b d
unifyExpr x y = x == y
