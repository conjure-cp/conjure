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
relationRepr ( name
             , [xMatch| ts := domain.relation.inners
                      | as := domain.relation.attributes.attrCollection
                      |]
             , decl) = do
    let t = [xMake| domain.tuple.inners := ts |]
    let domOut = [xMake| domain.set.attributes.attrCollection := []
                       | domain.set.inner := [t]
                       |]
    let refn = [xMake| reference := [Prim (S $ identifierConstruct
                                                    name
                                                    (Just "regionS")
                                                    (Just "RelationAsSet")
                                          )] |]
    let structurals = flip mapMaybe as $ \ a -> case a of
            [xMatch| [Prim (S "size")]    := attribute.nameValue.name.reference
                   | [num]                := attribute.nameValue.value
                   |] -> Just [eMake| |toSet(&refn)| = &num |]
            [xMatch| [Prim (S "minSize")] := attribute.nameValue.name.reference
                   | [num]                := attribute.nameValue.value
                   |] -> Just [eMake| |toSet(&refn)| >= &num |]
            [xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference
                   | [num]                := attribute.nameValue.value
                   |] -> Just [eMake| |toSet(&refn)| <= &num |]
            _ -> Nothing
    return [( decl
            , "builtIn.relationRepr"
            , "RelationAsSet"
            , domOut
            , structurals
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

ret :: MonadConjure m => E -> String -> E -> m (Maybe [(Text, E)])
ret orig name result = do
    mkLog name $ vcat [pretty orig, "~~>", pretty result]
    return $ Just [(stringToText name, result)]

builtInRefn :: MonadConjure m => [RefnFunc m]
builtInRefn = [relationApply, tupleExplode, functionLiteralApply]

relationApply :: MonadConjure m => RefnFunc m
relationApply p@[xMatch| [actual]             := functionApply.actual
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
                    ret p "builtIn.relationApply" [eMake| &theTuple in &theSet |]
                _ -> return Nothing
        _ -> return Nothing
relationApply _ = return Nothing

tupleExplode :: MonadConjure m => RefnFunc m
tupleExplode p@[xMatch| values       := operator.index.left .value.tuple.values
                      | [Prim (I i)] := operator.index.right.value.literal
                      |]
                      | i >= 1 && i <= genericLength values
                      = ret p "builtIn.tupleExplode" $ values `genericIndex` (i - 1)
tupleExplode p@[eMatch| &a = &b |] = do
    aTy <- typeOf a
    case aTy of
        [xMatch| is := type.tuple.inners |] -> do
            let result = conjunct [ [eMake| &a[&j] = &b[&j] |]
                                  | i <- [1 .. genericLength is]
                                  , let j = [xMake| value.literal := [Prim (I i)] |]
                                  ]
            ret p "builtIn.tupleExplode" result
        _ -> return Nothing
tupleExplode p@[eMatch| &a != &b |] = do
    aTy <- typeOf a
    case aTy of
        [xMatch| is := type.tuple.inners |] -> do
            let result = disjunct [ [eMake| &a[&j] != &b[&j] |]
                                  | i <- [1 .. genericLength is]
                                  , let j = [xMake| value.literal := [Prim (I i)] |]
                                  ]
            ret p "builtIn.tupleExplode" result
        _ -> return Nothing
tupleExplode _ = return Nothing

functionLiteralApply :: MonadConjure m => RefnFunc m
functionLiteralApply
    p@[xMatch| mappings := functionApply.actual.value.function.values
             | [arg]    := functionApply.args
             |] = do
    (quanVarStr, quanVar) <- freshQuanVar
    let overs = [ [xMake| value.tuple.values := [i,j] |]
                | [xMatch| [i,j] := mapping |] <- mappings
                ]
    let over  = [xMake| value.set.values := overs |]
    let guard = [eMake| &quanVar[1] = &arg |]
    let body  = [eMake| &quanVar[2] |]
    let out   = [xMake| quantified.quantifier.reference                := [Prim $ S "sum" ]
                      | quantified.quanVar.structural.single.reference := [Prim $ S quanVarStr ]
                      | quantified.quanOverDom                         := []
                      | quantified.quanOverOp.binOp.in                 := []
                      | quantified.quanOverExpr                        := [over]
                      | quantified.guard                               := [guard]
                      | quantified.body                                := [body]
                      |]
    ret p "builtIn.functionLiteralApply" out
functionLiteralApply _ = return Nothing

