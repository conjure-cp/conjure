{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Language.EssenceTypes ( runTypeOf ) where


import Control.Monad.RWS ( evalRWS
                         , MonadReader, ask
                         , MonadWriter, tell
                         )
import Control.Monad.Error ( MonadError, throwError, runErrorT )
import Data.Maybe ( fromJust )

import Language.Essence
import Language.EssencePrinters ( prExpr )
import PrintUtils ( render )


runTypeOf :: [Binding] -> Expr -> (Either String Type, [Log])
runTypeOf bs x = evalRWS (runErrorT (typeOf x)) bs ()


infixr 0 ~~$
(~~$) :: MonadWriter [Log] m => Expr -> Type -> m Type
a ~~$ b = do
    let p = render . fromJust . prExpr
    tell ["typeOf " ++ p a ++ " is " ++ show b ]
    return b

infixr 0 ~$$
(~$$) :: MonadError String m => Expr -> [Char] -> m a
x ~$$ msg = case prExpr x of Nothing -> throwError $ "Cannot pretty-print: " ++ show x
                             Just d  -> throwError $ msg ++ ": " ++ render d


typeOf ::
    ( MonadReader [Binding] m
    , MonadWriter [Log] m
    , MonadError String m
    ) => Expr -> m Type

typeOf p@Underscore       = p ~~$ TypeUnknown

typeOf p@(GenericNode op xs) = do
    ts <- mapM typeOf xs
    case validOpTypes op ts of
        Just t  -> p ~~$ t
        Nothing -> p ~$$ "Type mismatch"

typeOf p@(ValueBoolean _) = p ~~$ TypeBoolean
typeOf p@(ValueInteger _) = p ~~$ TypeInteger
typeOf p@(ValueMatrix []) = p ~~$ TypeMatrix TypeUnknown
typeOf p@(ValueMatrix xs) = do
    t:ts <- mapM typeOf xs
    if all (t==) ts
        then p ~~$ TypeMatrix t
        else p ~$$ "value matrix isn't of uniform types"
typeOf p@(ValueTuple xs) = do
    ts <- mapM typeOf xs
    p ~~$ TypeTuple ts
typeOf p@(ValueSet []) = p ~~$ TypeSet TypeUnknown
typeOf p@(ValueSet xs) = do
    t:ts <- mapM typeOf xs
    if all (t==) ts
        then p ~~$ TypeSet t
        else p ~$$ "value set isn't of uniform types"
typeOf p@(ValueMSet []) = p ~~$ TypeMSet TypeUnknown
typeOf p@(ValueMSet xs) = do
    t:ts <- mapM typeOf xs
    if all (t==) ts
        then p ~~$ TypeMSet t
        else p ~$$ "value mset isn't of uniform types"
typeOf p@(ValueFunction []) = p ~~$ TypeFunction TypeUnknown TypeUnknown
typeOf p@(ValueFunction xs) = do
    f:fs <- mapM (typeOf . fst) xs
    s:ss <- mapM (typeOf . snd) xs
    if all (f==) fs && all (s==) ss
        then p ~~$ TypeFunction f s
        else p ~$$ "value function isn't of uniform types"
typeOf p@(ValueRelation []) = p ~~$ TypeRelation []
typeOf p@(ValueRelation xs) = do
    t:ts <- mapM typeOf xs
    if all (t==) ts
        then case t of TypeTuple is -> p ~~$ TypeRelation is
                       _            -> p ~$$ "value relation doesn't contain tuples"
        else p ~$$ "value relation isn't of uniform types"
typeOf p@(ValuePartition xss) = case concat xss of
    [] -> p ~~$ TypePartition TypeUnknown
    xs -> do
        t:ts <- mapM typeOf xs
        if all (t==) ts
            then p ~~$ TypePartition t
            else p ~$$ "value partition isn't of uniform types"

typeOf p@(DomainBoolean       {}) = p ~~$ TypeBoolean
typeOf p@(DomainIntegerFromTo {}) = p ~~$ TypeInteger
typeOf p@(DomainIntegerList   {}) = p ~~$ TypeInteger
typeOf p@(DomainUnnamed       {}) = p ~~$ TypeUnnamed
typeOf p@(DomainEnum          {}) = p ~~$ TypeEnum
typeOf p@(DomainMatrix {element}) = do
    t <- typeOf element
    p ~~$ TypeMatrix t
typeOf p@(DomainTuple {components}) = do
    ts <- mapM typeOf components
    p ~~$ TypeTuple ts
typeOf p@(DomainSet {element}) = do
    t <- typeOf element
    p ~~$ TypeSet t
typeOf p@(DomainMSet {element}) = do
    t <- typeOf element
    p ~~$ TypeMSet t
typeOf p@(DomainFunction {functionFrom,functionTo}) = do
    f <- typeOf functionFrom
    t <- typeOf functionTo
    p ~~$ TypeFunction f t
typeOf p@(DomainRelation {components}) = do
    ts <- mapM typeOf components
    p ~~$ TypeRelation ts
typeOf p@(DomainPartition {element}) = do
    t <- typeOf element
    p ~~$ TypePartition t

typeOf p@(Identifier nm) = do
    bs <- ask
    case [ x | (_,nm',x) <- bs, nm == nm' ] of
        []  -> p ~$$ "identifier not found"
        [x] -> do t <- typeOf x; p ~~$ t
        _   -> p ~$$ "identifier bound to several things"

-- typeOf p = p ~$$ "not implemented yet"
