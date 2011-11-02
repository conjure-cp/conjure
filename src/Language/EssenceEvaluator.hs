{-# LANGUAGE FlexibleContexts #-}

module Language.EssenceEvaluator where

import Control.Applicative
import Control.Monad.RWS ( RWS, evalRWS
                         , MonadReader
                         , MonadWriter, tell
                         )
import Data.Generics.Uniplate.Direct ( rewriteBiM )
import Data.List ( sort )
import Data.Maybe ( fromJust )

import Language.Essence
import Language.EssencePrinters ( prExpr )
import PrintUtils ( render )


runEvaluateExpr :: [Binding] -> Expr -> (Expr,[Log])
runEvaluateExpr topLevels x = evalRWS (comp x) topLevels ()
    where
        comp :: Expr -> RWS [Binding] [Log] () Expr
        comp = rewriteBiM combined

        combined ::
            ( Applicative m
            , MonadReader [Binding] m
            , MonadWriter [Log] m
            ) => Expr -> m (Maybe Expr)
        combined i = do
            j <- evaluateExpr i
            case j of
                Nothing -> normaliseExpr i
                Just _  -> return j


evaluateExpr ::
    ( MonadReader [Binding] m
    , MonadWriter [Log] m
    ) => Expr -> m (Maybe Expr)

-- full evaluators

evaluateExpr inp@(GenericNode Plus   [ValueInteger i,ValueInteger j])          = inp ~~$ ValueInteger $ i + j
evaluateExpr inp@(GenericNode Minus  [ValueInteger i,ValueInteger j])          = inp ~~$ ValueInteger $ i - j
evaluateExpr inp@(GenericNode Times  [ValueInteger i,ValueInteger j])          = inp ~~$ ValueInteger $ i * j
evaluateExpr inp@(GenericNode Div    [ValueInteger i,ValueInteger j]) | j /= 0 = inp ~~$ ValueInteger $ div i j
evaluateExpr inp@(GenericNode Mod    [ValueInteger i,ValueInteger j]) | j >  0 = inp ~~$ ValueInteger $ mod i j
evaluateExpr inp@(GenericNode Pow    [ValueInteger i,ValueInteger j]) | j >  0 = inp ~~$ ValueInteger $ i ^ j

evaluateExpr inp@(GenericNode Abs    [ValueInteger i]) = inp ~~$ ValueInteger $ abs i
evaluateExpr inp@(GenericNode Negate [ValueInteger i]) = inp ~~$ ValueInteger $ negate i

evaluateExpr inp@(GenericNode Lt  [ValueInteger i, ValueInteger j]) = inp ~~$ ValueBoolean $ i <  j
evaluateExpr inp@(GenericNode Leq [ValueInteger i, ValueInteger j]) = inp ~~$ ValueBoolean $ i <= j
evaluateExpr inp@(GenericNode Gt  [ValueInteger i, ValueInteger j]) = inp ~~$ ValueBoolean $ i >  j
evaluateExpr inp@(GenericNode Geq [ValueInteger i, ValueInteger j]) = inp ~~$ ValueBoolean $ i >= j
evaluateExpr inp@(GenericNode Neq [ValueInteger i, ValueInteger j]) = inp ~~$ ValueBoolean $ i /= j
evaluateExpr inp@(GenericNode Eq  [ValueInteger i, ValueInteger j]) = inp ~~$ ValueBoolean $ i == j

-- partial evaluators

evaluateExpr inp@(GenericNode Plus   [x,ValueInteger 0]) = inp ~~$ x
evaluateExpr inp@(GenericNode Minus  [ValueInteger 0,x]) = inp ~~$ GenericNode Negate [x]
evaluateExpr inp@(GenericNode Times  [_,ValueInteger 0]) = inp ~~$ ValueInteger 0
evaluateExpr inp@(GenericNode Times  [x,ValueInteger 1]) = inp ~~$ x
evaluateExpr inp@(GenericNode Div    [x,ValueInteger 1]) = inp ~~$ x
evaluateExpr inp@(GenericNode Mod    [x,y])     | x == y = inp ~~$ ValueInteger 0
evaluateExpr inp@(GenericNode Pow    [_,ValueInteger 0]) = inp ~~$ ValueInteger 1
evaluateExpr inp@(GenericNode Pow    [x,ValueInteger 1]) = inp ~~$ x

-- symbolic full evaluators

evaluateExpr inp@(GenericNode Minus [a,b]) | a == b = inp ~~$ ValueInteger 0
evaluateExpr inp@(GenericNode Negate [GenericNode Negate [x]]) = inp ~~$ x
evaluateExpr inp@(GenericNode Eq [a,b]) | a == b = inp ~~$ ValueBoolean True

-- some special cases

evaluateExpr inp@(GenericNode Minus [GenericNode Plus [x,y],z])
    | y == z = inp ~~$ x
    | x == z = inp ~~$ y
evaluateExpr inp@(GenericNode Plus [GenericNode Minus [x,y],z])
    | y == z = inp ~~$ x
    | x == z = inp ~~$ y

-- no eval

evaluateExpr _ = noRewrite


normaliseExpr :: MonadWriter [Log] m => Expr -> m (Maybe Expr)
normaliseExpr inp@(GenericNode op [GenericNode op2 [a,b],c])
    | op == op2
    , op `elem` associativeOps
    = let [x,y,z] = sort [a,b,c]
      in  if [x,y,z] == [a,b,c]
              then noRewrite
              else rewriteTo "Normaliser" inp $ GenericNode op [GenericNode op [x,y],z]
normaliseExpr inp@(GenericNode op [a,b])
    | op `elem` commutativeOps
    , b < a
    = rewriteTo "Normaliser" inp $ GenericNode op [b,a]
normaliseExpr _ = noRewrite


--------------------------------------------------------------------------------
--  helper functions to return results from evaluator and normaliser -----------
--------------------------------------------------------------------------------

infixr 0 ~~$
(~~$) :: MonadWriter [Log] m => Expr -> Expr -> m (Maybe Expr)
(~~$) = rewriteTo "Evaluator "


rewriteTo :: MonadWriter [Log] m => String -> Expr -> Expr -> m (Maybe Expr)
rewriteTo msg old new = do
    let p = render . fromJust . prExpr
    tell [msg ++ ": " ++ p old ++ " ~~> " ++ p new]
    return $ Just new


noRewrite :: Monad m => m (Maybe a)
noRewrite = return Nothing

