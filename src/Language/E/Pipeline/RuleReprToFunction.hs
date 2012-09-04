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
        [CompError]                     -- static errors
        ( E -> CompE m [ ( String       -- rule name
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
        ( E -> CompE m [ ( String       -- rule name
                         , String       -- name of the representation
                         , E            -- replacement domain
                         , [E]          -- structural constraints
                         ) ]
        )
one repr@(ruleName, reprName, domTemplate, mcons, locals, cases) =
    let
        mresults = map (oneCase repr) cases
        errors   = concat $ lefts mresults
        results  = rights mresults
    in  if null errors
            then Right $ \ e -> concat <$> mapM ($ e) results
            else Left errors


oneCase :: (Functor m, Monad m)
    => RuleRepr
    -> RuleReprCase
    -> Either
        [CompError]
        ( E -> CompE m [ ( String       -- rule name
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
        Right $ \ x -> do
            bindersBefore <- getsLocal binders
            let restoreState = modifyLocal $ \ st -> st { binders = bindersBefore }
            flagMatch <- patternMatch domPattern x
            if flagMatch
                then do
                    bs        <- mapM (localHandler ruleName domPattern) locals
                    if and bs
                        then do
                            domTemplate' <- freshNames domTemplate
                            mres         <- runMaybeT $ patternBind domTemplate'
                            case mres of
                                Nothing  -> restoreState >> errRuleFail
                                Just res -> restoreState >> return [(ruleName, reprName, res, mcons)]
                        else restoreState >> errRuleFail
                else restoreState >> errRuleFail


errRuleFail :: Monad m => CompE m [a]
errRuleFail = return []





