{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.UnnamedToInt where

-- Unnamed Types in Essence
--
-- An unnamed type carries only one bit of information, a size.
-- A new unnamed type can be defined by a letting statement:
--
-- letting X be new type of size Y
--      Here, X is a fresh name (previously not bound to anything)
--      and Y is an expression of type integer.
-- 
-- Values of an unnamed type cannot be referred to with a name, thus there
-- cannot be a model parameter with an unnamed type.
-- (How would we even write down a value of an unnamed type?)
--
-- We can easily have decision variables of unnamed type though, and they are
-- implemented using integers in Conjure. Practically, the values assigned to
-- such decision variables after solving a problem instance won't be
-- meaningful.
--
-- find a : X
-- find b : matrix indexed by [int(0..9] of X
-- find a : partition (partSize p) of X
--

import Control.Monad ( forM )
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( runWriterT, tell )
import Data.Maybe ( mapMaybe )

import Stuff.TypeClasses

import Language.E


data StatementKind
    = DeclTypeUnnamed String E
    | DeclTypeEnum String

mkLettingDomain = undefined
intDomain = undefined
rangeFromTo = undefined


unnamedToInt
    :: ( Monad m
       -- , Binding binding thing
       -- , BindingsManager bindingsManager binding
       , Statement statement StatementKind
       -- , Has state (bindingsManager binding)
       -- , MonadError error m
       -- , MonadState state m
       , Program spec statement
       )
    => spec -> m spec
unnamedToInt spec = do
    (statements, unnamedTypeNames)
        <- runWriterT (mapM perStmt $ pStatements spec)
    let spec' = pUpdateStatements spec statements
    return spec'
    where
        perStmt s = case statementKind s of
            DeclTypeUnnamed name size -> do
                let newDecl = mkLettingDomain (name ++ "_unnamed")
                                              (intDomain [rangeFromTo 1 size])
                tell name
                return newDecl
            _ -> return s


