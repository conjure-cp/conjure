{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.E.Evaluator.Partial ( partialEvaluator, guardOp ) where

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.TypeOf
import Language.E.TH ( eMatch, eMake )


partialEvaluator :: (Functor m, Monad m) => E -> CompE m (Maybe E)

partialEvaluator [eMatch| &x + 0 |] = ret x
partialEvaluator [eMatch| 0 + &x |] = ret x

partialEvaluator [eMatch| &x - 0 |] = ret x

partialEvaluator [eMatch| &_ * 0 |] = ret [eMake| 0  |]
partialEvaluator [eMatch| 0 * &_ |] = ret [eMake| 0  |]

partialEvaluator [eMatch| &x * 1 |] = ret x
partialEvaluator [eMatch| 1 * &x |] = ret x

partialEvaluator [eMatch| &x / 1 |] = ret x

partialEvaluator [eMatch| true /\ &a |] = ret a
partialEvaluator [eMatch| &a /\ true |] = ret a
partialEvaluator [eMatch| false /\ &_ |] = ret [eMake| false |]
partialEvaluator [eMatch| &_ /\ false |] = ret [eMake| false |]

partialEvaluator [eMatch| false \/ &a |] = ret a
partialEvaluator [eMatch| &a \/ false |] = ret a
partialEvaluator [eMatch| true \/ &_ |] = ret [eMake| true |]
partialEvaluator [eMatch| &_ \/ true |] = ret [eMake| true |]

partialEvaluator [eMatch| false -> &_ |] = ret [eMake| true |]
partialEvaluator [eMatch| true  -> &a |] = ret a

partialEvaluator [eMatch| max({&a}) |] = ret a
partialEvaluator [eMatch| min({&a}) |] = ret a

partialEvaluator [xMatch| [a] := operator.replace.arg1
                        | [b]  := operator.replace.old
                        | [c]  := operator.replace.new
                        |] =
    let
        helper  old  new now | old == now = new
        helper  old  new (Tagged t xs) = Tagged t $ map (helper old new) xs
        helper _old _new other = other
    in  ret $ helper b c a

partialEvaluator [xMatch| [Prim (S "forAll")] := quantified.quantifier.reference
                        | [Prim (B True)]     := quantified.body.value.literal
                        |] = ret [eMake| true |]
partialEvaluator [xMatch| [Prim (S "forAll")] := quantified.quantifier.reference
                        | [Prim (B False)]     := quantified.guard.value.literal
                        |] = ret [eMake| true |]

-- quantification over an empty set or mset constant.
partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | [] := quantified.quanOverExpr.value.set.values
             |] = do identity <- identityOp quantifier; ret identity

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | [] := quantified.quanOverExpr.value.mset.values
             |] = do identity <- identityOp quantifier; ret identity

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | [] := quantified.quanOverExpr.typed.left.value.set.values
             |] = do identity <- identityOp quantifier; ret identity

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | [] := quantified.quanOverExpr.typed.left.value.mset.values
             |] = do identity <- identityOp quantifier; ret identity

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | _            := quantified.quanOverOp.binOp.in
             | [qnVar]      := quantified.quanVar.structural.single
             | [qnOverExpr] := quantified.quanOverExpr
             | vs           := quantified.quanOverExpr.value.set.values
             | qnGuards     := quantified.guard
             | [qnBody]     := quantified.body
             |] = do
    -- mkLog "debug partialEvaluator" $ prettyAsPaths p
    -- mkLog "debug partialEvaluator" $ stringToDoc $ show vs
    let
        qnGuards' = case qnGuards of
                        [ [xMatch| [] := emptyGuard |] ] -> []
                        _ -> qnGuards
    identity <- identityOp quantifier
    tyOverExpr <- typeOf qnOverExpr
    tyInner    <- innerTypeOf "in simplify" tyOverExpr
    case quantifier of
        "sum" -> do
            vs' <- sequence [ guardOp quantifier
                                      (map (replace qnVar v) theGuard)
                                      (replace qnVar v qnBody)
                            | (v, rest) <- withRestToL vs
                            , let theGuard =
                                    if not $ null $ vs \\ [v]
                                        -- then []
                                        then let 
                                                 typed = case rest of
                                                     [] -> [xMake| typed.left.value.mset.values := rest
                                                                 | typed.right.domainInExpr.type.mset.inner  := [tyInner]
                                                                 |]
                                                     _  -> [xMake| value.mset.values := rest
                                                                 |]
                                                 g =
                                                    [xMake| operator.toInt.unaryOp.not.binOp.operator := [Prim (S "in")]
                                                          | operator.toInt.unaryOp.not.binOp.left  := [v]
                                                          | operator.toInt.unaryOp.not.binOp.right := [typed]
                                                          |]
                                             -- in  trace (show g) $ trace (show $ pretty g) $ g : qnGuards'
                                             in  g : qnGuards'
                                        -- then Expr ":operator-/\\"
                                        --       [ replaceCore qnVar v qnGuard
                                        --       , Expr ":operator-toInt" [
                                        --         Expr ":operator-not" [
                                        --         Expr ":operator-in"
                                        --             [ v
                                        --             , Expr ":typed"
                                        --                 [ Expr ":value" [ Expr ":value-mset" rest ]
                                        --                 , Expr ":domain-in-expr" [
                                        --                   Expr ":type" [
                                        --                   Expr ":type-mset" [
                                        --                   Expr ":type-mset-inner" [
                                        --                   tyInner
                                        --               ]]]]]
                                        --         ]
                                        --             ]]
                                        --            ]
                                        else qnGuards'
                            ]
            Just <$> foldM (glueOp quantifier) identity vs'
        _ ->
            case vs of
                [] -> ret identity
                _  -> do
                    vs' <- sequence [ guardOp quantifier
                                              (map (replace qnVar v) qnGuards')
                                              (replace qnVar v qnBody)
                                    | v <- vs ]
                    Just <$> foldM (glueOp quantifier) identity vs'

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | _                     := quantified.quanOverOp.binOp.in
             | [qnVar]               := quantified.quanVar.structural.single
             | vs                    := quantified.quanOverExpr.value.mset.values
             | qnGuards              := quantified.guard
             | [qnBody]              := quantified.body
             |] = do
    -- mkLog "debug partialEvaluator" $ prettyAsPaths p
    -- mkLog "debug partialEvaluator" $ stringToDoc $ show vs
    let
        qnGuards' = case qnGuards of
                        [ [xMatch| [] := emptyGuard |] ] -> []
                        _ -> qnGuards
    identity <- identityOp quantifier
    -- tyOverExpr <- typeOf qnOverExpr
    -- tyInner    <- innerTypeOf "in simplify" tyOverExpr
    case vs of
        [] -> ret identity
        _  -> do
            vs' <- sequence [ guardOp quantifier
                                      (map (replace qnVar v) qnGuards')
                                      (replace qnVar v qnBody)
                            | v <- vs ]
            Just <$> foldM (glueOp quantifier) identity vs'
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

identityOp :: Monad m => String -> CompE m E
identityOp quantifier = case quantifier of
                "forAll" -> return [eMake| true  |]
                "exists" -> return [eMake| false |]
                "sum"    -> return [eMake| 0     |]
                _        -> err ErrFatal $ "Unknown quantifier: " <+> stringToDoc quantifier

guardOp :: Monad m => String -> [E] -> E -> CompE m E
guardOp _ [] b = return b
guardOp quantifier as b =
    let a = conjunct as
    in  case quantifier of
            "forAll" -> return [eMake| &a -> &b |]
            "exists" -> return [eMake| &a /\ &b |]
            "sum"    -> return [eMake| &a *  &b |]
            _        -> err ErrFatal $ "Unknown quantifier: " <+> stringToDoc quantifier

glueOp :: Monad m => String -> E -> E -> CompE m E
glueOp quantifier a b = case quantifier of
            "forAll" -> return [eMake| &a /\ &b |]
            "exists" -> return [eMake| &a \/ &b |]
            "sum"    -> return [eMake| &a +  &b |]
            _        -> err ErrFatal $ "Unknown quantifier: " <+> stringToDoc quantifier

conjunct :: [E] -> E
conjunct [] = error "Impossible."
conjunct xs = let f a b = [eMake| &a /\ &b |] in foldr1 f xs



-- x = [xMake| a.b.x := [Prim (I 1)]
--           | a.b.y := [Prim (I 2)]
--           | a.b.test := []
--           |]
-- 
-- success [xMatch| q := a.b.test |] = show q
-- failure [xMatch| q := a.b.test.foo |] = show q

