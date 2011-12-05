{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Language.EssenceTypes ( typeCheckSpec, typeCheckRuleRepr
                             , runTypeOf, typeOf
                             ) where

import Control.Applicative
import Control.Arrow ( first, second )
import Control.Monad.RWS ( evalRWS
                         , MonadReader, ask
                         , MonadWriter, tell
                         , MonadState, get, gets, put, modify
                         )
import Control.Monad ( forM_, unless )
import Control.Monad.Error ( MonadError, throwError, runErrorT )
import Data.Maybe ( fromJust )
import Data.Default ( def )

import Language.Essence
import Language.EssenceKinds ( kindOf )
import Language.EssencePrinters ( prExpr, prType )
import PrintUtils ( render )


typeCheckSpec :: Spec -> (Maybe String, [Log])
typeCheckSpec sp = first (either Just (const Nothing)) $ evalRWS (runErrorT core) (topLevelBindings sp) def
    where
        -- core :: m ()
        core = do
            case objective sp of
                Nothing -> return ()
                Just (_,o)  -> do
                    ot <- typeOf o
                    unless (typeUnify ot TypeInteger) $ throwError "objective must be of type int, it isn't."
            forM_ (topLevelWheres sp) $ \ x -> do
                t <- typeOf x
                unless (typeUnify TypeBoolean t)
                       (x ~$$ "Where statements must be of type boolean")
            forM_ (constraints sp) $ \ x -> do
                t <- typeOf x
                unless (typeUnify TypeBoolean t)
                       (x ~$$ "Constraints must be of type boolean")            

-- not implemented yet.
typeCheckRuleRepr :: RuleRepr -> (Maybe String, [Log])
typeCheckRuleRepr _rp = (Nothing, [])

runTypeOf :: [Binding] -> Expr -> (Either String Type, [Log])
runTypeOf bs x = evalRWS (runErrorT (typeOf x)) bs def


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
    ( MonadError String m
    , MonadReader [Binding] m
    , MonadWriter [Log] m
    , MonadState ([String],[(String,Type)]) m -- fst component tracks those identifiers already seen, for detecting cyclic definitions
                                              -- snd component tracks already known types
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
    knowns <- gets snd
    case nm `lookup` knowns of
        Just t  -> p ~~$ t
        Nothing -> do
            seens <- gets fst
            if nm `elem` seens
                then throwError $ "cyclic definition: " ++ nm ++ " defined in terms of itself."
                else do
                    put (nm:seens, knowns)
                    bindings <- ask
                    result <- case [ x | (_,nm',x) <- bindings, nm == nm' ] of
                        []  -> p ~$$ "identifier not found"
                        [x] -> do t <- typeOf x; p ~~$ t
                        _   -> p ~$$ "identifier bound to several things"
                    put (seens, knowns) -- restore state
                    return result

typeOf p@(DeclLambda args x) = do
    st <- get
    modify $ second (args ++)
    x' <- typeOf x
    put st
    p ~~$ TypeLambda (map snd args) x'

typeOf p@(DeclQuantifier glueOp skipOp identity) = do
    tGlueOp   <- typeOf glueOp
    tSkipOp   <- typeOf skipOp
    tIdentity <- typeOf identity

    let fail_tGlueOp = case render <$> prType tIdentity of
            Nothing  -> throwError $ "Cannot pretty-print: " ++ show tIdentity
            Just prt -> p ~$$ "glue op must be of type " ++ prt ++ ", " ++ prt ++ " -> " ++ prt

    case tGlueOp of
        (TypeLambda [a,b] c) ->
            unless (typeUnify a b && typeUnify b c) fail_tGlueOp
        _ -> fail_tGlueOp

    let fail_tSkipOp = case render <$> prType tIdentity of
            Nothing  -> throwError $ "Cannot pretty-print: " ++ show tIdentity
            Just prt -> p ~$$ "skip op must be of type bool, " ++ prt ++ " -> " ++ prt

    case tSkipOp of
        (TypeLambda [TypeBoolean,a] b) ->
            unless (typeUnify a b && typeUnify a tIdentity) fail_tSkipOp
        _ -> fail_tSkipOp

    p ~~$ tIdentity

typeOf p@(ExprQuantifier {quanName, quanVar=Identifier quanVar, quanOver, quanGuard, quanBody}) = do
    bindings <- ask
    case [ x | (Letting,nm,x) <- bindings, nm == quanName ] of
        [] -> p ~$$ "unknown quantifier: " ++ quanName
        [q@(DeclQuantifier {})] -> do

            -- is the same as identity of thw quan,
            -- and must be the same as the body of the quantified expression
            tq <- typeOf q

            -- get the expected type of the quantifier variable
            kQuanOver <- kindOf quanOver
            tQuanOver <- typeOf quanOver
            tQuanVar  <- case elementType kQuanOver tQuanOver of
                Nothing -> quanOver ~$$ "Cannot quantify over " ++ show tQuanOver
                Just t  -> return t
            modify $ second $ ((quanVar,tQuanVar):) -- add it to the list of known types

            -- type of "body"
            tQuanBody <- typeOf quanBody

            -- check the guard type == TypeBoolean
            case quanGuard of
                Nothing -> return ()
                Just g  -> do
                    tg <- typeOf g
                    unless (typeUnify TypeBoolean tg) $
                        p ~$$ "Type-error in the guard. Guards must be of type boolean."

            -- check the body type == quantifier.identity
            unless (typeUnify tq tQuanBody) $
                p ~$$ "Type-error in the body of quantified expression"

            return tq
        _  -> p ~$$ "multiple definitons of quantifier: " ++ quanName

typeOf p@(ExprQuantifier {}) = p ~$$ "quantifier has a non-identifier in the quanVar field."
