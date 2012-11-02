{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.Partial ( partialEvaluator, guardOp ) where

import Stuff.Generic

import Language.E.Imports
import Language.E.Definition
import Language.E.Helpers
import Language.E.CompE
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

partialEvaluator [eMatch| &a + &b - &c |]
    | [xMatch| [Prim (I bInt)] := value.literal |] <- b
    , [xMatch| [Prim (I cInt)] := value.literal |] <- c
    , bInt == cInt = ret a
partialEvaluator [eMatch| &a - &b + &c |]
    | [xMatch| [Prim (I bInt)] := value.literal |] <- b
    , [xMatch| [Prim (I cInt)] := value.literal |] <- c
    , bInt == cInt = ret a

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
             |] = partialEvaluatorValueSet quantifier qnVar qnOverExpr vs qnGuards qnBody
partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | _            := quantified.quanOverOp.binOp.in
             | [qnVar]      := quantified.quanVar.structural.single
             | [qnOverExpr] := quantified.quanOverExpr
             | vs           := quantified.quanOverExpr.structural.single.value.set.values
             | qnGuards     := quantified.guard
             | [qnBody]     := quantified.body
             |] = partialEvaluatorValueSet quantifier qnVar qnOverExpr vs qnGuards qnBody

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | _                     := quantified.quanOverOp.binOp.in
             | [qnVar]               := quantified.quanVar.structural.single
             | vs                    := quantified.quanOverExpr.value.mset.values
             | qnGuards              := quantified.guard
             | [qnBody]              := quantified.body
             |] = partialEvaluatorValueMSet quantifier qnVar vs qnGuards qnBody

-- inlining a structural variable, tuple here.

partialEvaluator
    [xMatch| quantifiers   := quantified.quantifier
           | ts            := quantified.quanVar.structural.tuple
           | quanOverDoms  := quantified.quanOverDom
           | quanOverOps   := quantified.quanOverOp
           | quanOverExprs := quantified.quanOverExpr
           | guards        := quantified.guard
           | bodys         := quantified.body
           |] = do
    i' <- nextUniqueName
    let
        -- the new quanVar
        i = [xMake| structural.single.reference := [Prim (S i')] |]

        -- index at position j, an integer.
        indexIt j' = let j = [xMake| value.literal := [Prim (I j')] |]
                     in  [eMake| &i[&j] |]

        -- list of indexed quanVars, instead of the input ts
        indexeds = map indexIt [1 .. genericLength ts]

        replacerFunc = replaceAll (zip ts indexeds)

    let
        result = [xMake| quantified.quantifier   := quantifiers
                       | quantified.quanVar      := [i]
                       | quantified.quanOverDom  := quanOverDoms
                       | quantified.quanOverOp   := quanOverOps
                       | quantified.quanOverExpr := quanOverExprs
                       | quantified.guard        := map replacerFunc guards
                       | quantified.body         := map replacerFunc bodys
                       |]
    return $ Just result

partialEvaluator _ = return Nothing


partialEvaluatorValueSet
    :: (Monad m, Functor m)
    => String
    -> E
    -> E
    -> [E]
    -> [E]
    -> E
    -> CompE m (Maybe E)
partialEvaluatorValueSet quantifier qnVar qnOverExpr vs qnGuards qnBody = do
    let
        qnGuards' = case qnGuards of
                        [ [xMatch| [] := emptyGuard |] ] -> []
                        _ -> qnGuards
    identity   <- identityOp quantifier
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
                                        then let 
                                                 typed = case rest of
                                                     [] -> [xMake| typed.left.value.mset.values := rest
                                                                 | typed.right.domainInExpr.type.mset.inner  := [tyInner]
                                                                 |]
                                                     _  -> [xMake| value.mset.values := rest
                                                                 |]
                                                 g =
                                                    [eMake| toInt(!(&v in &typed)) |]
                                             in  g : qnGuards'
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


partialEvaluatorValueMSet
    :: Monad m
    => String
    -> E
    -> [E]
    -> [E]
    -> E
    -> CompE m (Maybe E)
partialEvaluatorValueMSet quantifier qnVar vs qnGuards qnBody = do

    let
        qnGuards' = case qnGuards of
                        [ [xMatch| [] := emptyGuard |] ] -> []
                        _ -> qnGuards
    identity <- identityOp quantifier
    case vs of
        [] -> ret identity
        _  -> do
            vs' <- sequence [ guardOp quantifier
                                      (map (replace qnVar v) qnGuards')
                                      (replace qnVar v qnBody)
                            | v <- vs ]
            Just <$> foldM (glueOp quantifier) identity vs'


ret :: Monad m => E -> CompE m (Maybe E)
ret = return . Just


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



