{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Evaluator.Partial ( partialEvaluator, guardOp ) where

import Stuff.Generic

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Pretty
import Language.E.TypeOf
import Language.E.Evaluator.DataAboutQuantifiers
import Language.E.TH ( eMatch, eMake )


partialEvaluator :: MonadConjure m => E -> m (Maybe E)

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
                        | [Prim (B False)]    := quantified.guard.value.literal
                        |] = ret [eMake| true |]

partialEvaluator p@[eMatch| max({})   |] = err ErrFatal (pretty p <+> "is undefined.")
partialEvaluator   [eMatch| max({&a}) |] = ret a
partialEvaluator   [xMatch| xs := operator.max.value.set.values |] =
    ret $ foldr1 (\ a b -> [xMake| operator.max := [a,b] |] ) xs

partialEvaluator p@[eMatch| min({})   |] = err ErrFatal (pretty p <+> "is undefined.")
partialEvaluator   [eMatch| min({&a}) |] = ret a
partialEvaluator   [xMatch| xs := operator.min.value.set.values |] =
    ret $ foldr1 (\ a b -> [xMake| operator.min := [a,b] |] ) xs

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
             | vs           := quantified.quanOverExpr.operator.toSet.value.relation.values
             | qnGuards     := quantified.guard
             | [qnBody]     := quantified.body
             |] = partialEvaluatorValueSet quantifier qnVar qnOverExpr vs qnGuards qnBody

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | _            := quantified.quanOverOp.binOp.in
             | [qnVar]      := quantified.quanVar.structural.single
             | [qnOverExpr] := quantified.quanOverExpr
             | parts        := quantified.quanOverExpr.operator.parts.value.partition.values
             | qnGuards     := quantified.guard
             | [qnBody]     := quantified.body
             |] = partialEvaluatorValueSet quantifier qnVar qnOverExpr vs qnGuards qnBody
    where vs = map one parts
          one [xMatch| xs := part |] = [xMake| value.set.values := xs |]
          one _ = error "This should never happen. Please report a bug."

partialEvaluator
   _p@[xMatch| [Prim (S quantifier)] := quantified.quantifier.reference
             | _            := quantified.quanOverOp.binOp.in
             | [qnVar]      := quantified.quanVar.structural.single
             | [qnOverExpr] := quantified.quanOverExpr
             | mappings     := quantified.quanOverExpr.operator.toSet.value.function.values
             | qnGuards     := quantified.guard
             | [qnBody]     := quantified.body
             |] = partialEvaluatorValueSet quantifier qnVar qnOverExpr vs qnGuards qnBody
    where vs = map one mappings
          one [xMatch| [i,j] := mapping |] = [xMake| value.tuple.values := [i,j] |]
          one _ = error "This should never happen. Please report a bug."

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

partialEvaluator _ = return Nothing


partialEvaluatorValueSet
    :: MonadConjure m
    => Text
    -> E
    -> E
    -> [E]
    -> [E]
    -> E
    -> m (Maybe E)
partialEvaluatorValueSet quantifier qnVar qnOverExpr vs qnGuards qnBody = do
    let
        qnGuards' = case qnGuards of
                        [ [xMatch| [] := emptyGuard |] ] -> []
                        _ -> qnGuards
    identity   <- identityOp quantifier
    tyOverExpr <- typeOf qnOverExpr
    tyInner    <- case innerTypeOf tyOverExpr of
                    Nothing -> err ErrFatal $ "Cannot get the inner type of:" <+> pretty tyOverExpr
                    Just i  -> return i
    case vs of
        [] -> ret identity
        _  ->
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
                                                             [] -> [xMake| typed.left.value.mset.values             := rest
                                                                         | typed.right.domainInExpr.type.mset.inner := [tyInner]
                                                                         |]
                                                             _  -> [xMake| value.mset.values := rest
                                                                         |]
                                                         g =
                                                            [eMake| !(&v in &typed) |]
                                                     in  g : qnGuards'
                                                else qnGuards'
                                    ]
                    Just <$> foldM (glueOp quantifier) identity vs'
                _ -> do
                    vs' <- sequence [ guardOp quantifier
                                              (map (replace qnVar v) qnGuards')
                                              (replace qnVar v qnBody)
                                    | v <- vs ]
                    Just <$> foldM (glueOp quantifier) identity vs'


partialEvaluatorValueMSet
    :: MonadConjure m
    => Text
    -> E
    -> [E]
    -> [E]
    -> E
    -> m (Maybe E)
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


ret :: Monad m => E -> m (Maybe E)
ret = return . Just

