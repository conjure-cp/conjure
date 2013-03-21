{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.RuleRefnToFunction ( ruleRefnToFunction, localHandler ) where

import Language.E
import Language.E.Pipeline.FreshNames

import qualified Data.HashSet as S


-- does the grouping depending on levels and such.
-- for a description on the params and return type see combineRuleRefns
ruleRefnToFunction
    :: MonadConjure m
    => [RuleRefn]
    -> Either
        [ConjureError]
        [E -> m (Maybe [(Text, E)])]
ruleRefnToFunction fs =
    let
        justsFirst :: Ord a => Maybe a -> Maybe a -> Ordering
        justsFirst (Just i) (Just j) = compare i j
        justsFirst Nothing (Just _)  = GT
        justsFirst (Just _) Nothing  = LT
        justsFirst Nothing  Nothing  = EQ

        fsGrouped :: [[RuleRefn]]
        fsGrouped = groupBy (\ (_,a,_) (_,b,_) -> a == b )
                  $ sortBy  (\ (_,a,_) (_,b,_) -> justsFirst a b )
                    fs

        -- mresults :: MonadConjure m => [Either [ConjureError] (E -> m E)]
        mresults = map combineRuleRefns fsGrouped

        -- errors :: [ConjureError]
        errors = concat $ lefts mresults

        -- funcs :: MonadConjure m => [E -> m E]
        funcs = rights mresults
    in
        if null errors
            then Right funcs
            else Left  errors


combineRuleRefns
    :: MonadConjure m
    => [RuleRefn]                                       -- given a list of RuleRefns
    -> Either                                           -- return
        [ConjureError]                                     -- either a list of errors (due to static checking a RuleRefn)
        (E -> m (Maybe [(Text, E)]))                    -- or a (Just list) of functions. the return type contains the rule name in the string.
                                                        --    a Nothing means no rule applications at that level.
combineRuleRefns fs =
    let
        -- mresults :: MonadConjure m => [Either ConjureError (E -> m (Maybe [(String, E)]))]
        mresults = map single fs

        -- errors   :: [ConjureError]
        errors   = lefts  mresults

        -- funcs    :: MonadConjure m => [E -> m (Maybe [(String, E)])]
        funcs    = rights mresults
    in  if null errors
            then Right $ \ x -> do
                mys <- mapM ($ x) funcs
                let ys = concat $ catMaybes mys
                return $ if null ys
                            then Nothing
                            else Just ys
            else Left errors


single
    :: forall m
    .  MonadConjure m
    => RuleRefn
    -> Either
        ConjureError                                       -- static errors in the rule
        (E -> m (Maybe [(Text, E)]))                       -- the rule as a function.
single ( name
       , _
       , [xMatch| [pattern] := rulerefn.pattern
                | templates := rulerefn.templates
                | locals    := rulerefn.locals
                |]
       ) = do
    let
        staticCheck :: Either ConjureError ()
        staticCheck = do
            let metaVarsIn p      = S.fromList [ r | [xMatch| [Prim (S r)] := metavar |] <- universe p ]
            let patternMetaVars   = metaVarsIn pattern
            let templateMetaVars  = S.unions [ metaVarsIn template
                                             | template <- templates ]
            let hasDomainMetaVars = S.unions [ S.unions [ metaVarsIn b
                                                        | [xMatch| [Prim (S "hasDomain")] := binOp.operator
                                                                 | [b] := binOp.right
                                                                 |] <- universe loc
                                                        ]
                                             | loc <- locals
                                             ]
            let lettingMetaVars   = S.unions [ metaVarsIn n
                                             | [xMatch| [n] := topLevel.letting.name |] <- locals
                                             ]
            let i `isSubsetOf` j = S.null (i `S.difference` j)
            unless (templateMetaVars `isSubsetOf` S.unions [patternMetaVars,hasDomainMetaVars,lettingMetaVars])
                $ Left ( ErrFatal
                       , vcat [ "in rule:" <+> pretty name
                              , "Pattern meta variables:"  <+> prettyListDoc id "," (map pretty $ S.toList patternMetaVars)
                              , "Template meta variables:" <+> prettyListDoc id "," (map pretty $ S.toList templateMetaVars)
                              ]
                       , Nothing
                       )
    staticCheck

    return $ \ x -> withBindingScope' $ do
        (flagMatch, _) <- patternMatch pattern x
        if flagMatch
            then do
                mkLog "trying-rule" $ pretty name <+> "on" $$ pretty x
                let
                    localsHandler [] = do
                        mxs <- forM templates $ \ template -> do
                            template' <- freshNames template
                            mres      <- runMaybeT $ patternBind template'
                            case mres of
                                Nothing  -> errRuleFail
                                Just res -> do
                                    res' <- renRefn res
                                    return (Just (name, res'))
                        case catMaybes mxs of
                            [] -> errRuleFail
                            xs ->
                                case [ nm | (nm, x') <- xs, x == x' ] of
                                    (nm:_) -> err ErrFatal $ "Rule returns the same expression:" <+> pretty nm
                                    _      -> return (Just xs)
                    localsHandler (l:ls) = do
                        res <- localHandler name x l
                        if res
                            then localsHandler ls
                            else errRuleFail
                localsHandler locals
            else errRuleFail
single _ = Left (ErrFatal, "This should never happen. (in RuleRefnToFunction.worker)", Nothing)


renRefn :: MonadConjure m => E -> m E
renRefn p@[xMatch| [Prim (S "refn")] := functionApply.actual.reference
                 | [Prim (S i'    )] := functionApply.args.reference
                 |] =
    case identifierSplit i' of
        (i,mregion,Just j) ->
            let name = identifierConstruct (mconcat [i,"_",j]) mregion Nothing
            in  return [xMake| reference := [Prim (S name)] |]
        _ -> err ErrFatal $ "{renRefn} Problem here:" <+> pretty p
renRefn [eMatch| refn(&m[&j]) |] = do
    n <- renRefn [eMake| refn(&m) |]
    return [eMake| &n[&j] |]
renRefn (Tagged t xs) = Tagged t <$> mapM renRefn xs
renRefn x = return x


errRuleFail :: Monad m => m (Maybe a)
errRuleFail = return Nothing


localHandler :: MonadConjure m
    => Text             -- rule name
    -> E                -- containing expression
    -> E
    -> m Bool
localHandler name x lokal@[xMatch| [y] := topLevel.where |] = do
    my' <- runMaybeT $ inlineMetavars y
    y'  <- case my' of
        Nothing    -> err ErrFatal
                $ "where statement cannot be fully evaluated: " <++> vcat [ pretty lokal
                                                                          , "in rule" <+> pretty name
                                                                          , "at expression" <+> pretty x
                                                                          ]
        Just i -> return i
    xBool <- toBool y'
    case xBool of
        Just (True, newBindings) -> do
            modify $ \ st -> st { binders = newBindings ++ binders st }
            return True
        Just (False, _) -> do
            mkLog "rule-fail"
                $ "where statement evaluated to false: " <++> vcat [ pretty lokal
                                                                   , "in rule" <+> pretty name
                                                                   , "at expression" <+> pretty x
                                                                   ]
            return False
        Nothing    -> err ErrFatal
                $ "where statement cannot be fully evaluated: " <++> vcat [ pretty lokal
                                                                          , "in rule" <+> pretty name
                                                                          , "at expression" <+> pretty x
                                                                          ]
localHandler _ _ lokal = introduceStuff lokal >> return True


inlineMetavars :: MonadConjure m => E -> MaybeT m E
inlineMetavars [xMatch| [Prim (S nm)] := metavar |] = lookupMetaVar nm
inlineMetavars [eMatch| &x hasDomain `&y`        |] = do xNew <- inlineMetavars x
                                                         return [eMake| &xNew hasDomain `&y` |]
inlineMetavars [eMatch| &x hasType   `&y`        |] = do xNew <- inlineMetavars x
                                                         return [eMake| &xNew hasType   `&y` |]
inlineMetavars (Tagged t xs) = Tagged t <$> mapM inlineMetavars xs
inlineMetavars x = return x

