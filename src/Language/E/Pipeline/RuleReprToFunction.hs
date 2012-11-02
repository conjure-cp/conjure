{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.RuleReprToFunction ( RuleReprResult, ruleReprToFunction ) where

import Language.E
import Language.E.Pipeline.FreshNames
import Language.E.Pipeline.RuleRefnToFunction ( localHandler )


-- given a list of RuleReprs, returns a function which
-- accepts a domain and returns a list of domains
ruleReprToFunction :: (Functor m, Monad m)
    => [RuleRepr]
    -> Either
        [CompError]                   -- static errors
        ( ( String                    -- input: name of the variable
          , E                         -- input: domain
          , E                         -- input: declaration
          )
          -> CompE m [RuleReprResult]
        )
ruleReprToFunction fs =
    let
        mresults = map one fs
        errors   = concat $ lefts mresults
        results  = rights mresults
    in  if null errors
            then Right $ \ e -> concatMapM ($ e) results
            else Left errors


one :: (Functor m, Monad m)
    => RuleRepr
    -> Either
        [CompError]
        ( (String, E, E)
          -> CompE m [RuleReprResult]
        )
one repr@(_ruleName, _reprName, _domTemplate, _mcons, _locals, cases) =
    let
        mresults = map (oneCase repr) cases
        errors   = concat $ lefts mresults
        results  = rights mresults
    in  if null errors
            then Right $ \ x -> concatMapM ($ x) results
            else Left errors


oneCase :: (Functor m, Monad m)
    => RuleRepr
    -> RuleReprCase
    -> Either
        [CompError]
        ( (String, E, E)
          -> CompE m [RuleReprResult]
        )
oneCase (ruleName, reprName, domTemplate, mcons1, locals1, _)
        (domPattern, mcons2, locals2) =
    let
        -- mcons: list of all structural constraints
        mcons  = maybeToList mcons1 ++ maybeToList mcons2

        -- locals: list of all local lettings, wheres and finds
        locals = locals1 ++ locals2

        -- applyToInnerDomain: tries to apply this rule-case to an inner domain.
        -- an inner domain is a domain which isn't a matrix.
        applyToInnerDomain restoreState origName origDecl is x = do
            let errReturn = restoreState >> errRuleFail
            (flagMatch, _) <- patternMatch domPattern x
            if not flagMatch
                then errReturn
                else do
                    bs <- mapM (localHandler ruleName domPattern) locals
                    if not $ and bs
                        then errReturn
                        else do
                            domTemplate' <- freshNames domTemplate
                            mres         <- runMaybeT $ patternBind domTemplate'
                            case mres of
                                Nothing -> errReturn
                                Just res -> do
                                    -- at this point, res is the refinement of the innerDomain
                                    -- mcons is the list of structural constraints
                                    -- if is /= []
                                    --      res needs to be lifted (using is)
                                    --      mcons needs to be lifted (using forAlls)
                                    -- also, mcons are the structural constraints, but they need to be lifted
                                    let
                                        liftedRes = mkMatrixDomain is res
                                    mcons' <- forM mcons $ \ con -> do
                                        -- con' is the constraint, but all "refn"s replaced
                                        con' <- case is of
                                            [] -> do
                                                let renameTo = [xMake| reference := [Prim $ S $ origName ++ "_" ++ reprName] |]
                                                return $ renRefn renameTo con
                                            _  -> do
                                                let renameTo = [xMake| reference := [Prim $ S $ origName ++ "_" ++ reprName] |]
                                                (loopVarStrs, loopVars) <- unzip <$> replicateM (length is) freshQuanVar
                                                let renameToIndexed = mkIndexedExpr loopVars renameTo
                                                return $ inForAlls (zip loopVarStrs is) $ renRefn renameToIndexed con

                                        -- renaming identifiers before we return the constraint
                                        con''    <- freshNames con'
                                        maybeCon <- runMaybeT $ patternBind con''
                                        maybe (err ErrFatal $ "Unbound reference in" <+> pretty con'')
                                              return
                                              maybeCon
                                    restoreState >> return [(origDecl, ruleName, reprName, liftedRes, mcons')]

    in
        Right $ \ (origName, x, origDecl) -> do
            bindersBefore <- getsLocal binders
            let restoreState = modifyLocal $ \ st -> st { binders = bindersBefore }
            let (is,j) = splitMatrixDomain x
            applyToInnerDomain restoreState origName origDecl is j



splitMatrixDomain :: E -> ([E], E)
splitMatrixDomain [xMatch| [xIndex] := domain.matrix.index
                         | [xInner] := domain.matrix.inner
                         |] = first (xIndex:) (splitMatrixDomain xInner)
splitMatrixDomain x = ([], x)


mkMatrixDomain :: [E] -> E -> E
mkMatrixDomain []     j = j
mkMatrixDomain (i:is) j = [xMake| domain.matrix.index := [i]
                                | domain.matrix.inner := [mkMatrixDomain is j]
                                |]


mkIndexedExpr :: [E] -> E -> E
mkIndexedExpr = go . reverse
    where
        go []     x = x
        go (i:is) x = let y = go is x in [eMake| &y[&i] |]


renRefn :: E -> E -> E
renRefn newName [xMatch| [Prim (S "refn")] := reference |] = newName
renRefn newName (Tagged t xs) = Tagged t $ map (renRefn newName) xs
renRefn _ x = x

errRuleFail :: Monad m => CompE m [a]
errRuleFail = return []

