{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction ) where

import Language.E
import Language.E.Pipeline.FreshNames
import Language.E.Pipeline.RuleRefnToFunction ( localHandler )


-- given a list of RuleReprs, returns a function which
-- accepts a domain and returns a list of domains
ruleReprToFunction :: (Functor m, Monad m)
    => [RuleRepr]
    -> Either
        [CompError]                   -- static errors
        ( (String,E)
          -> CompE m [ ( String       -- rule name
                       , String       -- name of the representation
                       , E            -- replacement domain
                       , [E]          -- structural constraints
                       ) ]
        )
ruleReprToFunction fs =
    let
        mresults = map one fs
        errors   = concat $ lefts mresults
        results  = rights mresults
    in  if null errors
            then Right $ \ e -> concat <$> mapM ($ e) results
            else Left errors


one :: (Functor m, Monad m)
    => RuleRepr
    -> Either
        [CompError]
        ( (String, E)
          -> CompE m [ ( String       -- rule name
                       , String       -- name of the representation
                       , E            -- replacement domain
                       , [E]          -- structural constraints
                       ) ]
        )
one repr@(_ruleName, _reprName, _domTemplate, _mcons, _locals, cases) =
    let
        mresults = map (oneCase repr) cases
        errors   = concat $ lefts mresults
        results  = rights mresults
    in  if null errors
            then Right $ \ x -> concat <$> mapM ($ x) results
            else Left errors


oneCase :: (Functor m, Monad m)
    => RuleRepr
    -> RuleReprCase
    -> Either
        [CompError]
        ( (String, E)                 -- original variable's name
          -> CompE m [ ( String       -- rule name
                       , String       -- name of the representation
                       , E            -- replacement domain
                       , [E]          -- structural constraints
                       ) ]
        )
oneCase (ruleName, reprName, domTemplate, mcons1, locals1, _)
        (domPattern, mcons2, locals2) =
    let
        mcons  = maybeToList mcons1 ++ maybeToList mcons2
        locals = locals1 ++ locals2
    in
        Right $ \ (origName, x) -> do
            bindersBefore <- getsLocal binders
            let restoreState = modifyLocal $ \ st -> st { binders = bindersBefore }
            case x of
                [xMatch| [xIndex] := domain.matrix.index
                       | [xInner] := domain.matrix.inner |] -> do
                    flagMatch <- patternMatch domPattern xInner
                    if flagMatch
                        then do
                            bs <- mapM (localHandler ruleName domPattern) locals
                            if and bs
                                then do
                                    domTemplate' <- freshNames domTemplate
                                    mres         <- runMaybeT $ patternBind domTemplate'
                                    case mres of
                                        Nothing -> restoreState >> errRuleFail
                                        Just res -> do
                                            let resInMatrix = [xMake| domain.matrix.index := [xIndex]
                                                                    | domain.matrix.inner := [res]
                                                                    |]
                                            loopVarName <- nextUniqueName
                                            mcons' <- forM mcons $ \ con -> do
                                                con' <- (freshNames <=<
                                                        renRefn [xMake| operator.index.left .reference := [Prim $ S $ origName ++ "_" ++ reprName]
                                                                      | operator.index.right.reference := [Prim $ S loopVarName]
                                                                      |]
                                                        ) con
                                                let con'' = [xMake| quantified.quantifier.reference                := [Prim $ S "forAll"]
                                                                  | quantified.quanVar.structural.single.reference := [Prim $ S loopVarName]
                                                                  | quantified.quanOverDom                         := [xIndex]
                                                                  | quantified.quanOverOp                          := []
                                                                  | quantified.quanOverExpr                        := []
                                                                  | quantified.guard.emptyGuard                    := []
                                                                  | quantified.body                                := [con']
                                                                  |]
                                                maybeCon <- runMaybeT $ patternBind con''
                                                case maybeCon of
                                                    Nothing -> err ErrFatal $ "Unbound reference in" <+> pretty con''
                                                    Just c  -> return c
                                            restoreState >> return [(ruleName, reprName, resInMatrix, mcons')]
                                else restoreState >> errRuleFail
                        else restoreState >> errRuleFail
                _ -> do
                    flagMatch <- patternMatch domPattern x
                    if flagMatch
                        then do
                            bs <- mapM (localHandler ruleName domPattern) locals
                            if and bs
                                then do
                                    domTemplate' <- freshNames domTemplate
                                    mres         <- runMaybeT $ patternBind domTemplate'
                                    case mres of
                                        Nothing  -> restoreState >> errRuleFail
                                        Just res -> do
                                            mcons' <- forM mcons $ \ con -> do
                                                con' <- (freshNames <=<
                                                         renRefn [xMake| reference := [Prim $ S $ origName ++ "_" ++ reprName] |]
                                                        ) con
                                                maybeCon <- runMaybeT $ patternBind con'
                                                case maybeCon of
                                                    Nothing -> err ErrFatal $ "Unbound reference in" <+> pretty con'
                                                    Just c  -> return c
                                            restoreState >> return [(ruleName, reprName, res, mcons')]
                                else restoreState >> errRuleFail
                        else restoreState >> errRuleFail

renRefn :: Monad m => E -> E -> CompE m E
renRefn newName [xMatch| [Prim (S "refn")] := reference |] = return newName
renRefn newName (Tagged t xs) = Tagged t <$> mapM (renRefn newName) xs
renRefn _ x = return x

errRuleFail :: Monad m => CompE m [a]
errRuleFail = return []





