{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.BuiltIn
    ( ReprFunc, builtInRepr, mergeReprFunc
    , RefnFunc, builtInRefn
    ) where

import Bug
import Language.E
import Language.E.Pipeline.FreshNames

import Language.E.BuiltIn.SetStructural ( setStructural )



mergeReprFunc :: MonadConjure m => [ReprFunc m] -> ReprFunc m
mergeReprFunc [ ] =  bug "mergeReprFunc []"
mergeReprFunc [f] = f
mergeReprFunc fs = \ param -> concat <$> mapM ($ param) fs

builtInRepr :: MonadConjure m => [ReprFunc m]
builtInRepr = [applyToInnerDomain' relationRepr]


relationRepr :: MonadConjure m => ReprFunc m
relationRepr ( _name
             , [xMatch| ts := domain.relation.inners
                      | as := domain.relation.attributes.attrCollection
                      |]
             , decl) = do
    let t = [xMake| domain.tuple.inners := ts |]
    let domOut = [xMake| domain.set.attributes.attrCollection := as
                       | domain.set.inner := [t]
                       |]
    -- let refn = [xMake| reference := [Prim (S $ identifierConstruct
                                                    -- name
                                                    -- (Just "regionS")
                                                    -- (Just "Relation~AsSet")
                                          -- )] |]
    -- let structurals = flip mapMaybe as $ \ a -> case a of
            -- [xMatch| [Prim (S "size")]    := attribute.nameValue.name.reference
                   -- | [num]                := attribute.nameValue.value
                   -- |] -> Just [eMake| |toSet(&refn)| = &num |]
            -- [xMatch| [Prim (S "minSize")] := attribute.nameValue.name.reference
                   -- | [num]                := attribute.nameValue.value
                   -- |] -> Just [eMake| |toSet(&refn)| >= &num |]
            -- [xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference
                   -- | [num]                := attribute.nameValue.value
                   -- |] -> Just [eMake| |toSet(&refn)| <= &num |]
            -- _ -> Nothing
    return [( decl
            , "builtIn.relationRepr"
            , "Relation~AsSet"
            , domOut
            -- , structurals
            , []
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
                    (loopVarStrs, loopVars) <- unzip <$> replicateM (length is) (freshQuanVar "applyToInnerDomain'")
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



ret :: MonadConjure m => E -> String -> E -> m (Maybe [(Text, E)])
ret orig name result = do
    mkLog name $ vcat [pretty orig, "~~>", pretty result]
    return $ Just [(stringToText name, result)]

builtInRefn :: MonadConjure m => [RefnFunc m]
builtInRefn = [ relationApply, tupleExplode, functionLiteralApply
              , quanOverToSetRelationProject
              , tupleDomInQuantification
              , intDomFromSet
              , setStructural
              ]

intDomFromSet :: MonadConjure m => RefnFunc m
intDomFromSet p@[xMatch| xs := domain.int.ranges.range.single.value.set.values |]
    = ret p "builtIn.intDomFromSet" [xMake| domain.int.ranges := (map toSingleRange xs) |]
    where toSingleRange i = [xMake| range.single := [i] |]
intDomFromSet p@[xMatch| [r] := domain.int.ranges.range.single |] =
    case r of
        [eMatch| &a union &b |] ->
            ret p "builtIn.intDomFromSet"
                [xMake| domain.binOp.operator := [Prim (S "union")]
                      | domain.binOp.left     := [a]
                      | domain.binOp.right    := [b]
                      |]
        [eMatch| &a intersect &b |] ->
            ret p "builtIn.intDomFromSet"
                [xMake| domain.binOp.operator := [Prim (S "intersect")]
                      | domain.binOp.left     := [a]
                      | domain.binOp.right    := [b]
                      |]
        [eMatch| &a - &b |] ->
            ret p "builtIn.intDomFromSet"
                [xMake| domain.binOp.operator := [Prim (S "-")]
                      | domain.binOp.left     := [a]
                      | domain.binOp.right    := [b]
                      |]
        _ -> return Nothing
intDomFromSet _ = return Nothing

relationApply :: MonadConjure m => RefnFunc m
relationApply p@[xMatch| [actual] := functionApply.actual
                       |  args    := functionApply.args
                       |]
    | hasRepr "Relation~AsSet" actual = do
        actualTy <- typeOf actual
        argsTy   <- mapM typeOf args
        case actualTy of
            [xMatch| actualInners := type.relation.inners |] | actualInners == argsTy -> do
                let theTuple = [xMake| value.tuple.values := args |]
                let theSet   = refnOf "Relation~AsSet" actual
                ret p "builtIn.relationApply" [eMake| &theTuple in &theSet |]
            _ -> return Nothing
relationApply _ = return Nothing

quanOverToSetRelationProject :: MonadConjure m => RefnFunc m
quanOverToSetRelationProject
  p@[xMatch| [relation]                 := quantified.quanOverExpr.operator.toSet.functionApply.actual
           | args                       := quantified.quanOverExpr.operator.toSet.functionApply.args
           | []                         := quantified.quanOverDom
           | []                         := quantified.quanOverOp.binOp.in
           | [quantifier]               := quantified.quantifier
           | [quanVar]                  := quantified.quanVar.structural.single
           | [guard]                    := quantified.guard
           | [body]                     := quantified.body
           |]
    | hasRepr "Relation~AsSet" relation = do
        let theSet   = refnOf "Relation~AsSet" relation
        (_newQuanVarStr, newQuanVar) <- freshQuanVar "quanOverToSetRelationProject"

        -- the rest of the items are available as output. sort of.
        let projectedElems = catMaybes
                [ if arg == [eMake| _ |]
                    then Just i
                    else Nothing
                | (i,arg) <- zip [ 1::Integer .. ] args
                ]

        let projectedElems_Values =
                [ [eMake| &newQuanVar[&i] |]
                | i' <- projectedElems
                , let i = [xMake| value.literal := [Prim (I i')] |]
                ]

        let projectedElems_Expr =
                [xMake| value.tuple.values := projectedElems_Values |]

        let newGuard = conjunct
                $ replace quanVar projectedElems_Expr guard
                : [ [eMake| &newQuanVar[&i] = &arg |]
                  | (i',arg) <- zip [ 1::Integer .. ] args
                  , arg /= [eMake| _ |]
                  , let i = [xMake| value.literal := [Prim (I i')] |]
                  ]
                -- : [ [eMake| &newQuanVar[&i] = &args_Expr[&i] |]
                  -- | i' <- projectedEqs
                  -- , let i = [xMake| value.literal := [Prim (I i')] |]
                  -- ]
        let newBody  = replace quanVar projectedElems_Expr body

        ret p "builtIn.quanOverToSetRelationProject"
            [xMake| quantified.quantifier           := [quantifier]
                  | quantified.quanVar              := [newQuanVar]
                  | quantified.quanOverDom          := []
                  | quantified.quanOverOp.binOp.in  := []
                  | quantified.quanOverExpr         := [theSet]
                  | quantified.guard                := [newGuard]
                  | quantified.body                 := [newBody]
                  |]
quanOverToSetRelationProject _ = return Nothing

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
tupleDomInQuantification p@[eMatch| &quan &i : &dom , &guard . &body |] = do
    let
        replacer out inp@[xMatch| [Prim (S nm)] := reference |] =
            let (base, _, _) = identifierSplit nm
            in  if i `elem` [ [xMake| reference := [Prim (S base)] |]
                            , [xMake| structural.single.reference := [Prim (S base)] |]
                            ]
                    then out
                    else inp
        replacer _ inp = inp

        helper [] = bug "this should never happen, tupleDomInQuantification"
        helper [(quanvar,quandom)] = [eMake| &quan &quanvar : &quandom , &guard . &body |]
        helper ((quanvar,quandom):rest) =
            let body' = helper rest
            in  [eMake| &quan &quanvar : &quandom . &body' |]
    case dom of
        [xMatch| xs := domain.tuple.inners |] -> do
            ys <- forM xs $ \ x -> do
                (_, quanvar) <- freshQuanVar "tupleDomInQuantification"
                return (quanvar, x)
            let quanvars = [xMake| value.tuple.values := map fst ys |]
            let out = transform (replacer quanvars) $ helper ys
            ret p "builtIn.tupleDomInQuantification" out
        _ -> return Nothing
tupleDomInQuantification _ = return Nothing

functionLiteralApply :: MonadConjure m => RefnFunc m
functionLiteralApply
    p@[xMatch| mappings := functionApply.actual.value.function.values
             | [actual] := functionApply.actual
             | [arg]    := functionApply.args
             |] = do
    tyActual <- typeOf actual
    case tyActual of
        [xMatch| [] := type.function.innerTo.type.int |] -> do
            (quanVarStr, quanVar) <- freshQuanVar "functionLiteralApply"
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
        _ -> return Nothing
functionLiteralApply p@[eMatch| &quan &i in &quanOverExpr , &guard . &body |] =
    case quanOverExpr of
        [xMatch| vsMappings := functionApply.actual.value.function.values
               | [actual]   := functionApply.actual
               | [arg]      := functionApply.args
               |] -> do
            tyActual <- typeOf actual
            continue <- case tyActual of
                [xMatch| _ := type.function.innerTo.type.set  |] -> return True
                [xMatch| _ := type.function.innerTo.type.mset |] -> return True
                _                          -> return False
            if continue
                then do
                    vs <- forM vsMappings $ \ vsMapping -> case vsMapping of
                        [xMatch| [a,b] := mapping |] -> return [xMake| value.tuple.values := [a,b] |]
                        _ -> err ErrFatal "builtIn.functionLiteralApply"
                    let newQuanOverExpr = [xMake| value.set.values := vs |]
                    let newGuard = conjunct [ replace i [eMake| &i[2] |] guard
                                            , [eMake| &i[1] = &arg |]
                                            ]
                    (_, j) <- freshQuanVar "functionLiteralApply"
                    let newBody  = replace i j body
                    let out = [eMake| &quan &i in &newQuanOverExpr
                                        , &newGuard
                                        . &quan &j in &i[2] . &newBody
                                    |]
                    ret p "builtIn.functionLiteralApply" out
                else return Nothing
        _ -> return Nothing
functionLiteralApply _ = return Nothing


hasRepr :: Text -> E -> Bool
hasRepr repr [xMatch| [Prim (S ref)] := reference |] =
    case identifierSplit ref of
        (_, _, Just repr2) | repr == repr2 -> True
        _ -> False
hasRepr repr [eMatch| &m[&_] |] = hasRepr repr m
hasRepr _ _ = False


refnOf :: Text -> E -> E
refnOf repr p@[xMatch| [Prim (S ref)] := reference |] =
    case identifierSplit ref of
        (actualName, mregion, Just repr2) | repr == repr2 ->
            let
                ref' = identifierConstruct (mconcat [actualName, "_", repr]) mregion Nothing
            in
                [xMake| reference := [Prim (S ref')] |]
        _ -> p
refnOf repr [eMatch| &m[&i] |] =
    let
        n = refnOf repr m
    in
        [eMake| &n[&i] |]
refnOf _ p = p

