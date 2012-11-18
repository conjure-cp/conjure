{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.RuleReprToFunction ( RuleReprResult, ruleReprToFunction ) where

import Language.E
import Language.E.Pipeline.FreshNames
import Language.E.Pipeline.RuleRefnToFunction ( localHandler )


-- given a list of RuleReprs, returns a function which
-- accepts a domain and returns a list of domains
ruleReprToFunction
    :: MonadConjure m
    => [RuleRepr]
    -> Either
        [ConjureError]                   -- static errors
        ( ( String                    -- input: name of the variable
          , E                         -- input: domain
          , E                         -- input: declaration
          )
          -> m [RuleReprResult]
        )
ruleReprToFunction fs =
    let
        mresults = map one fs
        errors   = concat $ lefts mresults
        results  = rights mresults
    in  if null errors
            then Right $ \ e -> concatMapM ($ e) results
            else Left errors


one :: MonadConjure m
    => RuleRepr
    -> Either
        [ConjureError]
        ( (String, E, E)
          -> m [RuleReprResult]
        )
one repr@(_ruleName, _reprName, _domTemplate, _mcons, _locals, cases) =
    let
        mresults = map (oneCase repr) cases
        errors   = concat $ lefts mresults
        results  = rights mresults
    in  if null errors
            then Right $ \ x -> concatMapM ($ x) results
            else Left errors


oneCase
    :: MonadConjure m
    => RuleRepr
    -> RuleReprCase
    -> Either
        [ConjureError]
        ( (String, E, E)
          -> m [RuleReprResult]
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
        applyToInnerDomain origName origDecl is x = do
            (flagMatch, _) <- patternMatch domPattern x
            if not flagMatch
                then errRuleFail
                else do
                    bs <- mapM (localHandler ruleName domPattern) locals
                    if not $ and bs
                        then errRuleFail
                        else do
                            domTemplate' <- freshNames domTemplate
                            mres         <- runMaybeT $ patternBind domTemplate'
                            case mres of
                                Nothing -> errRuleFail
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
                                    return [(origDecl, ruleName, reprName, liftedRes, mcons')]

    in
        Right $ \ (origName, x, origDecl) -> withBindingScope' $ do
            let (is,j) = splitMatrixDomain x
            applyToInnerDomain origName origDecl is j



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

errRuleFail :: Monad m => m [a]
errRuleFail = return []

