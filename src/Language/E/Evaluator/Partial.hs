{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.E.Evaluator.Partial ( partialEvaluator ) where

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.TH ( eMatch, eMake )


partialEvaluator :: (Functor m, Monad m) => E -> CompE m (Maybe E)

partialEvaluator [eMatch| @x + 0 |] = ret x -- or ret [eMake| @x |]
partialEvaluator [eMatch| 0 + @x |] = ret x

partialEvaluator [eMatch| @x - 0 |] = ret x

partialEvaluator [eMatch| @_ * 0 |] = ret [eMake| 0  |]
partialEvaluator [eMatch| 0 * @_ |] = ret [eMake| 0  |]

partialEvaluator [eMatch| @_ * 1 |] = ret [eMake| 1  |]
partialEvaluator [eMatch| 1 * @_ |] = ret [eMake| 1  |]

partialEvaluator [eMatch| @x / 1 |] = ret x

partialEvaluator [eMatch| true /\ @a |] = ret a
partialEvaluator [eMatch| @a /\ true |] = ret a
partialEvaluator [eMatch| false /\ @_ |] = ret [eMake| false |]
partialEvaluator [eMatch| @_ /\ false |] = ret [eMake| false |]

partialEvaluator [eMatch| false \/ @a |] = ret a
partialEvaluator [eMatch| @a \/ false |] = ret a
partialEvaluator [eMatch| true \/ @_ |] = ret [eMake| true |]
partialEvaluator [eMatch| @_ \/ true |] = ret [eMake| true |]

partialEvaluator [eMatch| false -> @_ |] = ret [eMake| true |]
partialEvaluator [eMatch| true  -> @a |] = ret a

partialEvaluator
    [xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
           | _        := quantified.quanOverOp.binOp.in
           | [qnVar]  := quantified.quanVar.structural.single
           | vs       := quantified.quanOverExpr.value.set.values
           | qnGuards := quantified.guard
           | [qnBody] := quantified.body
           |] = do
    let
        qnGuards' = case qnGuards of
                        [ [xMatch| [] := emptyGuard |] ] -> []
                        _ -> qnGuards
    let
        identityOp = case quantifier of
                        "forAll" -> return [eMake| true  |]
                        "exists" -> return [eMake| false |]
                        "sum"    -> return [eMake| 0     |]
                        _        -> err ErrFatal $ "Unknown quantifier: " <+> stringToDoc quantifier
        guardOp [] b = return b
        guardOp as b =
            let a = conjunct as
            in  case quantifier of
                    "forAll" -> return [eMake| @a -> @b |]
                    "exists" -> return [eMake| @a /\ @b |]
                    "sum"    -> return [eMake| @a *  @b |]
                    _        -> err ErrFatal $ "Unknown quantifier: " <+> stringToDoc quantifier
        glueOp a b = case quantifier of
                        "forAll" -> return [eMake| @a /\ @b |]
                        "exists" -> return [eMake| @a \/ @b |]
                        "sum"    -> return [eMake| @a +  @b |]
                        _        -> err ErrFatal $ "Unknown quantifier: " <+> stringToDoc quantifier
        conjunct [] = error "Impossible."
        conjunct xs = let f a b = [eMake| @a /\ @b |] in foldr1 f xs
    identity <- identityOp
    case vs of
        [] -> ret identity
        _  -> do
            vs' <- sequence [ case qnGuards of
                                [] -> return $ replace qnVar v qnBody
                                _  -> guardOp qnGuards' (replace qnVar v qnBody)
                            | v <- vs ]
            Just <$> foldM glueOp identity vs'


partialEvaluator _ = return Nothing


ret :: Monad m => E -> CompE m (Maybe E)
ret = return . Just



-- quantificationOverValueSet_In :: (Functor m, Monad m)
--     => Reference
--     -> Core
--     -> [Core]
--     -> Core
--     -> Core
--     -> Core
--     -> WriterT Any (CompT m) Core
-- quantificationOverValueSet_In quantifier qnOverExpr vs qnVar qnGuard qnBody = do
--     tell $ Any True
--     let
--         guardOp (Expr ":empty-guard" []) b = return b
--         guardOp a b = case quantifier of
--                         "forAll" -> return $ Expr ":operator-->"  [a, b]
--                         "exists" -> return $ Expr ":operator-/\\" [a, b]
--                         "sum"    -> return $ Expr ":operator-*"   [a, b]
--                         _        -> lift $ err ErrInvariant
--                                             $ singletonNested
--                                             $ "unknown quantifier in simplify" <+> pretty quantifier
--         glueOp a b = case quantifier of
--                         "forAll" -> return $ Expr ":operator-/\\" [a, b]
--                         "exists" -> return $ Expr ":operator-\\/" [a, b]
--                         "sum"    -> return $ Expr ":operator-+"   [a, b]
--                         _        -> lift $ err ErrInvariant
--                                             $ singletonNested
--                                             $ "unknown quantifier in simplify" <+> pretty quantifier
--     let identity' = qnIdentity quantifier
--     tyOverExpr <- lift $ typeOf qnOverExpr
--     tyInner    <- lift $ innerTypeOf "in simplify" tyOverExpr
--     case quantifier of
--         "sum" -> do
--             vs' <- sequence [ guardOp theGuard
--                                       (replaceCore qnVar v qnBody)
--                             | (v, rest) <- withRestToL vs
--                             , let theGuard =
--                                     if not $ null $ vs \\ [v]
--                                         then Expr ":operator-/\\"
--                                               [ replaceCore qnVar v qnGuard
--                                               , Expr ":operator-toInt" [
--                                                 Expr ":operator-not" [
--                                                 Expr ":operator-in"
--                                                     [ v
--                                                     , Expr ":typed"
--                                                         [ Expr ":value" [ Expr ":value-mset" rest ]
--                                                         , Expr ":domain-in-expr" [
--                                                           Expr ":type" [
--                                                           Expr ":type-mset" [
--                                                           Expr ":type-mset-inner" [
--                                                           tyInner
--                                                           ]]]]]
--                                                     ]
--                                                 ]]
--                                                ]
--                                         else replaceCore qnVar v qnGuard
--                             ]
--             foldM glueOp identity' vs'
--         _     -> do
--             vs' <- sequence [ guardOp (replaceCore qnVar v qnGuard)
--                                       (replaceCore qnVar v qnBody)
--                             | v <- vs ]
--             foldM glueOp identity' vs'