{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.BuiltIn
    ( ReprFunc, builtInRepr, mergeReprFunc
    , RefnFunc, builtInRefn
    ) where

import Language.E
import Language.E.Pipeline.FreshNames



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
builtInRepr = [applyToInnerDomain' relationRepr]


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
relationRepr ( _, [xMatch| _ := domain.set      |], _ ) = return []
relationRepr ( _, [xMatch| _ := domain.mset     |], _ ) = return []
relationRepr ( _, [xMatch| _ := domain.function |], _ ) = return []
relationRepr ( _name, _dom, _ ) = do
    mkLog "missing:relationRepr" $ vcat [ pretty _name
                                        , prettyAsPaths _dom
                                        ]
    return []



applyToInnerDomain'
    :: MonadConjure m
    => ((Text, E, E) -> m [RuleReprResult])
    -> (Text, E, E)
    -> m [RuleReprResult]
applyToInnerDomain' f (origName, origDomain, origDecl) = do
    let (is,x) = splitMatrixDomain origDomain
    results <- f (origName, x, origDecl)
    liftM concat $ forM results $ \ (_, ruleName, reprName, res, mcons) -> do
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
                    (loopVarStrs, loopVars) <- unzip <$> replicateM (length is) freshQuanVar
                    let renameToIndexed = mkIndexedExpr loopVars renameTo
                    return $ inForAlls (zip loopVarStrs is)
                                       ( [xMake| emptyGuard := [] |]
                                       , renRefn renameToIndexed con
                                       )

            -- renaming identifiers before we return the constraint
            con''    <- freshNames con'
            maybeCon <- runMaybeT $ patternBind con''
            maybe (errUndefinedRef "builtIn.ruleReprCompile" $ pretty con'')
                  return
                  maybeCon
        return [(origDecl, ruleName, reprName, liftedRes, mcons')]

renRefn :: E -> E -> E
renRefn newName [xMatch| [Prim (S "refn")] := reference |] = newName
renRefn newName (Tagged t xs) = Tagged t $ map (renRefn newName) xs
renRefn _ x = x



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
builtInRefn = [ relationApply, tupleExplode, functionLiteralApply
              , tupleDomInQuantification
              ]

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

tupleDomInQuantification :: MonadConjure m => RefnFunc m
tupleDomInQuantification p@[eMatch| &quan &i : &dom , &guard . &body |] =
    case dom of
        [xMatch| xs := domain.tuple.inners |] -> do
            ys <- forM xs $ \ x -> do
                (_, quanvar) <- freshQuanVar
                return (quanvar, x)
            let quanvars = [xMake| value.tuple.values := map fst ys |]
            ret p "builtIn.tupleDomInQuantification" (helper quanvars ys)
        _ -> return Nothing
    where
        helper _ [] = error "this should never happen, tupleDomInQuantification"
        helper allQuanVars [(quanvar,quandom)] = replace i allQuanVars [eMake| &quan &quanvar : &quandom , &guard . &body |]
        helper allQuanVars ((quanvar,quandom):rest) =
            let body' = helper allQuanVars rest
            in  [eMake| &quan &quanvar : &quandom . &body' |]
tupleDomInQuantification _ = return Nothing

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

