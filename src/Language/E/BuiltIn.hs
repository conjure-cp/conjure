{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.BuiltIn
    ( ReprFunc, builtInRepr, mergeReprFunc
    , RefnFunc, builtInRefn
    ) where

import Language.E



type ReprFunc m =
    ( Text                                  -- input: name of the variable
    , E                                     -- input: domain
    , E                                     -- input: decl
    )
    -> m [RuleReprResult]



mergeReprFunc :: MonadConjure m => [ReprFunc m] -> ReprFunc m
mergeReprFunc [ ] =  error "mergeReprFunc []"
mergeReprFunc [f] = f
mergeReprFunc fs = \ param -> concat <$> mapM ($ param) fs

builtInRepr :: MonadConjure m => [ReprFunc m]
builtInRepr = [relationRepr]

relationRepr :: MonadConjure m => ReprFunc m
relationRepr ( _name, [xMatch| ts := domain.relation.inners |], decl) = do
    let t = [xMake| domain.tuple.inners := ts |]
    return [( decl
            , "builtIn.relationRepr"
            , "RelationAsSet"
            , [xMake| domain.set.attributes.attrCollection := []
                    | domain.set.inner := [t]
                    |]
            , []
            )]

relationRepr ( _, [xMatch| _ := domain.function |], _ ) = return []
relationRepr ( _name, _dom, _ ) = do
    mkLog "missing:relationRepr" $ vcat [ pretty _name
                                        , prettyAsPaths _dom
                                        ]
    return []



type RefnFunc m =
    E                                   -- the expression
    -> m (Maybe [(Text, E)])            -- Nothing if rule doesn't apply
                                        -- returns a list of rewrites, fst being rulename
                                        --                           , snd being E

ret :: Monad m => Text -> E -> m (Maybe [(Text, E)])
ret name result = return $ Just [(name, result)]

builtInRefn :: MonadConjure m => [RefnFunc m]
builtInRefn = [relationApply, tupleExplode]

relationApply :: MonadConjure m => RefnFunc m
relationApply [xMatch| [actual]             := functionApply.actual
                     | [Prim (S actualRef)] := functionApply.actual.reference
                     |  args                := functionApply.args
                     |] =
    case identifierSplit actualRef of
        (actualName, mregion, Just "RelationAsSet") -> do
            actualTy <- typeOf actual
            argsTy   <- mapM typeOf args
            case actualTy of
                [xMatch| actualInners := type.relation.inners |] | actualInners == argsTy -> do
                    let theTuple = [xMake| value.tuple.values := args |]
                    let theSet   = [xMake| reference := [Prim $ S $ identifierConstruct (actualName `mappend` "_RelationAsSet")
                                                                                        mregion
                                                                                        Nothing
                                                        ] |]
                    ret "builtIn.relationApply" [eMake| &theTuple in &theSet |]
                _ -> return Nothing
        _ -> return Nothing
relationApply _ = return Nothing

tupleExplode :: MonadConjure m => RefnFunc m
tupleExplode [xMatch| values       := operator.index.left .value.tuple.values
                    | [Prim (I i)] := operator.index.right.value.literal
                    |]
                    | i >= 1 && i <= genericLength values
                    = ret "builtIn.tupleExplode" $ values `genericIndex` (i - 1)
tupleExplode [eMatch| &a = &b |] = do
    aTy <- typeOf a
    case aTy of
        [xMatch| is := type.tuple.inners |] -> do
            let result = conjunct [ [eMake| &a[&j] = &b[&j] |]
                                  | i <- [1 .. genericLength is]
                                  , let j = [xMake| value.literal := [Prim (I i)] |]
                                  ]
            ret "builtIn.tupleExplode" result
        _ -> return Nothing
tupleExplode [eMatch| &a != &b |] = do
    aTy <- typeOf a
    case aTy of
        [xMatch| is := type.tuple.inners |] -> do
            let result = disjunct [ [eMake| &a[&j] != &b[&j] |]
                                  | i <- [1 .. genericLength is]
                                  , let j = [xMake| value.literal := [Prim (I i)] |]
                                  ]
            ret "builtIn.tupleExplode" result
        _ -> return Nothing
tupleExplode _ = return Nothing

