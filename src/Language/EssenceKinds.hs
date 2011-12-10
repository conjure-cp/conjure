{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Language.EssenceKinds ( runKindOf, kindOf, textAfterBe ) where


import Control.Monad.RWS ( evalRWS
                         , MonadReader, ask
                         , MonadWriter, tell
                         )
import Control.Monad.Error ( MonadError, throwError, runErrorT )
import Data.Maybe ( fromJust )

import Language.Essence ( Expr(..), Binding, BindingEnum(..), Log, Kind(..) )
import Language.EssencePrinters ( prExpr )
import PrintUtils ( Doc, empty, render, text )


runKindOf :: [Binding] -> Expr -> (Either String Kind, [Log])
runKindOf bs x = evalRWS (runErrorT (kindOf x)) bs ()


infixr 0 ~~$
(~~$) :: MonadWriter [Log] m => Expr -> Kind -> m Kind
a ~~$ b = do
    let p = render . fromJust . prExpr
    tell ["kindOf " ++ p a ++ " is " ++ show b ]
    return b

infixr 0 ~$$
(~$$) :: MonadError String m => Expr -> String -> m a
x ~$$ msg = case prExpr x of Nothing -> throwError $ "Cannot pretty-print: " ++ show x
                             Just d  -> throwError $ msg ++ ": " ++ render d


-- the kind of the expression defines how it is pretty-printed when occurs
-- in a let-binding
textAfterBe :: [Binding] -> Expr -> Doc
textAfterBe _ (DeclQuantifier {}) = text "quantifier"
textAfterBe bs x = case runKindOf bs x of
    (Right KindDomain,_) -> text "domain"
    _                    -> empty


kindOf ::
    ( MonadReader [Binding] m
    , MonadWriter [Log] m
    , MonadError String m
    ) => Expr -> m Kind

kindOf p@Underscore       = p ~~$ KindUnknown

kindOf p@(GenericNode {}) = p ~~$ KindExpr

kindOf p@(MatrixSlice {}) = p ~$$ "this shouldn't happen."

kindOf p@(ValueBoolean   {}) = p ~~$ KindValue
kindOf p@(ValueInteger   {}) = p ~~$ KindValue
kindOf p@(ValueMatrix    {}) = p ~~$ KindValue
kindOf p@(ValueTuple     {}) = p ~~$ KindValue
kindOf p@(ValueSet       {}) = p ~~$ KindValue
kindOf p@(ValueMSet      {}) = p ~~$ KindValue
kindOf p@(ValueFunction  {}) = p ~~$ KindValue
kindOf p@(ValueRelation  {}) = p ~~$ KindValue
kindOf p@(ValuePartition {}) = p ~~$ KindValue

kindOf p@(DomainBoolean       {}) = p ~~$ KindDomain
kindOf p@(DomainIntegerFromTo {}) = p ~~$ KindDomain
kindOf p@(DomainIntegerList   {}) = p ~~$ KindDomain
kindOf p@(DomainUnnamed       {}) = p ~~$ KindDomain
kindOf p@(DomainEnum          {}) = p ~~$ KindDomain
kindOf p@(DomainMatrix        {}) = p ~~$ KindDomain
kindOf p@(DomainTuple         {}) = p ~~$ KindDomain
kindOf p@(DomainSet           {}) = p ~~$ KindDomain
kindOf p@(DomainMSet          {}) = p ~~$ KindDomain
kindOf p@(DomainFunction      {}) = p ~~$ KindDomain
kindOf p@(DomainRelation      {}) = p ~~$ KindDomain
kindOf p@(DomainPartition     {}) = p ~~$ KindDomain

kindOf p@(Identifier nm) = do
    bs <- ask
    case [ (e,x) | (e,nm',x) <- bs, nm == nm' ] of
        []            -> p ~$$ "identifier not found"
        [(Find   ,_)] -> p ~~$ KindFind
        [(Given  ,_)] -> p ~~$ KindGiven
        [(Letting,x)] -> do t <- kindOf x; p ~~$ t
        _             -> p ~$$ "identifier bound to several things"

kindOf p@(DeclLambda     {}) = p ~~$ KindLambda
kindOf p@(DeclQuantifier {}) = p ~~$ KindExpr

kindOf p@(ExprQuantifier {}) = p ~~$ KindExpr
