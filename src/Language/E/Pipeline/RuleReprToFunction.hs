{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.RuleReprToFunction ( RuleReprResult, ruleReprToFunction ) where

import Language.E
import Language.E.Pipeline.FreshNames
import Language.E.Pipeline.RuleRefnToFunction ( localHandler )

import Safe ( lastNote )
import qualified Data.Text as T



-- given a list of RuleReprs, returns a function which
-- accepts a domain and returns a list of domains
ruleReprToFunction
    :: ( MonadConjure m
       , RandomM m
       )
    => [RuleRepr]
    -> Either
        [ConjureError]                -- static errors
        ( ( Text                      -- input: name of the variable
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
        ( (Text, E, E)
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
        ( (Text, E, E)
          -> m [RuleReprResult]
        )
oneCase (ruleName, reprName, domTemplate, mcons1, locals1, _)
        (domPattern, mcons2, locals2) =
    let
        -- mcons: list of all structural constraints
        mcons  = maybeToList mcons1 ++ maybeToList mcons2

        -- locals: list of all local lettings, wheres and finds
        locals = locals1 ++ locals2

    in
        Right $ \ (origName, x, origDecl) -> withBindingScope' $ do
            let (is,j) = splitMatrixDomain x
            applyToInnerDomain
                ruleName reprName domPattern domTemplate
                mcons locals
                origName origDecl is j



-- applyToInnerDomain: tries to apply this rule-case to an inner domain.
-- an inner domain is a domain which isn't a matrix.
applyToInnerDomain
    :: MonadConjure m
    => Text -> Text -> E -> E
    -> [E] -> [E]
    -> Text -> E
    -> [E] -> E
    -> m [RuleReprResult]
applyToInnerDomain ruleName reprName domPattern domTemplate mcons locals origName origDecl is x = do
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
                                        let newName  = identifierConstruct (mconcat [origName, "_", reprName])
                                                                           (Just "regionS")
                                                                           Nothing
                                        let renameTo = [xMake| reference := [Prim (S newName)] |]
                                        return $ renRefn renameTo con
                                    _  -> do
                                        let newName  = identifierConstruct (mconcat [origName, "_", reprName])
                                                                           (Just "regionS")
                                                                           Nothing
                                        let renameTo = [xMake| reference := [Prim (S newName)] |]
                                        (loopVarStrs, loopVars) <- unzip <$> replicateM (length is) (freshQuanVar "applyToInnerDomain")
                                        let renameToIndexed = mkIndexedExpr loopVars renameTo

                                        let theGuard | Just base <- "_Set~ExplicitVarSize_tuple2" `T.stripSuffix` origName
                                                     = let b = mkIndexedExpr loopVars [xMake| reference := [Prim (S $ mconcat [base, "_Set~ExplicitVarSize_tuple1"])] |]
                                                       in  [eMake| &b = true |]
                                                     | Just base <- "_Set~ExplicitVarSizeWithMarker_tuple2" `T.stripSuffix` origName
                                                     = let b = [xMake| reference := [Prim (S $ mconcat [base, "_Set~ExplicitVarSizeWithMarker_tuple1"])] |]
                                                           lastLoopVar = lastNote "RuleRefnToFunction" loopVars
                                                       in  [eMake| &lastLoopVar <= &b |]
                                                     | otherwise = error $ show $ vcat [ "don't know which guard to use for structural!"
                                                                                       , pretty origName
                                                                                       ]

                                        mkLog "RuleRefnToFunction theGuard" $ pretty theGuard

                                        mkLog "RuleRefnToFunction 0.0" $ pretty origName
                                        mkLog "RuleRefnToFunction 0.1" $ pretty renameTo
                                        mkLog "RuleRefnToFunction 0.2" $ pretty renameToIndexed
                                        mkLog "RuleRefnToFunction 1" $ vcat $ map pretty loopVarStrs
                                        mkLog "RuleRefnToFunction 2" $ vcat $ map pretty is
                                        mkLog "RuleRefnToFunction 3" $ pretty con
                                        mkLog "RuleRefnToFunction 4" $ pretty $ renRefn renameToIndexed con
                                        mkLog "RuleRefnToFunction 5" $ pretty $ inForAlls (zip loopVarStrs is) 
                                                           ( theGuard
                                                           , renRefn renameToIndexed con
                                                           )
                                        return $ inForAlls (zip loopVarStrs is) 
                                                           ( theGuard
                                                           , renRefn renameToIndexed con
                                                           )

                                -- renaming identifiers before we return the constraint
                                con''    <- freshNames con'
                                maybeCon <- runMaybeT $ patternBind con''
                                maybe (errUndefinedRef "ruleReprCompile" $ pretty con'')
                                      return
                                      maybeCon
                            return [(origDecl, ruleName, reprName, liftedRes, mcons')]



renRefn :: E -> E -> E
renRefn newName [xMatch| [Prim (S "refn")] := reference |] = newName
renRefn newName (Tagged t xs) = Tagged t $ map (renRefn newName) xs
renRefn _ x = x

errRuleFail :: Monad m => m [a]
errRuleFail = return []

