{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.ApplyRepr ( applyRepr ) where

import Language.E
import Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction )
import Language.E.BuiltIn ( builtInRepr, mergeReprFunc )

import qualified Data.Map as M
import qualified Data.Set as S


applyRepr
    :: ( MonadConjure m
       , MonadList m
       )
    => [RuleRepr]
    -> Spec
    -> m Spec
applyRepr rules spec = withBindingScope' $ let mfunc = ruleReprToFunction rules in case mfunc of
    Left es     -> err ErrFatal $ vcat $ map (prettyError "repr") es
    Right func' -> do

        let func = mergeReprFunc (func' : builtInRepr)

        let Spec _ statements = spec

        let topLevels'
                =  [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.find .name.reference
                                        | [d]          := topLevel.declaration.find .domain
                                        |] <- statementAsList statements ]
                ++ [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.given.name.reference
                                        | [d]          := topLevel.declaration.given.domain
                                        |] <- statementAsList statements ]

        let topLevels = [ (x,n,d) | (x,n,d) <- topLevels', domainNeedsRepresentation d ]

        candidates :: [(Text,[RuleReprResult])]
            <- forM topLevels $ \ (x,n,d) -> do
            ys <- func (n,d,x)
            case ys of
                [] -> err ErrFatal $ "No representation rule matches domain:" <+> pretty x
                _  -> do
                    let ysNames = flip map ys $ \ (_origDecl, _ruleName, reprName, _newDom, _cons) -> reprName
                    mkLog "representation" $ sep [ pretty x
                                                 , "(#" <> pretty (length ys) <> ")"
                                                 , prettyList id "," ysNames
                                                 ] 
                    return (n,ys)

        let
            allRegioned :: [(Text,Text)]
            allRegioned =
                let
                    findsandgivens = S.fromList $ map fst candidates
                in
                    nub [ (base, reg)
                        | [xMatch| [Prim (S nm)] := reference |] <- universeSpecNoFindGiven spec
                        , (base, Just reg, _) <- [identifierSplit nm]
                        , base `S.member` findsandgivens
                        ]

            nbOccurrence :: (Text, Text) -> Int
            nbOccurrence (base,reg) = length
                [ ()
                | [xMatch| [Prim (S nm')] := reference |] <- universeSpecNoFindGiven spec
                , (base', Just reg', _) <- [identifierSplit nm']
                , base == base'
                , reg  == reg'
                ]

            lookupTables :: [ M.Map (Text,Text) RuleReprResult ]
            lookupTables = map M.fromList $ allCombinations
                [ ((nm,region), results)
                | (nm, region) <- allRegioned
                , let Just results = lookup nm candidates
                , let cnt = nbOccurrence (nm, region)
                , cnt > 0
                ]

        table <- returns lookupTables
        if M.null table
            then returns []
            else do
                let configStr = hsep [ pretty (identifierConstruct nm (Just region) (Just rName))
                                     | ((nm, region), (_,_,rName,_,_)) <- M.toList table
                                     ]
                mkLog "configuration" configStr
                catchError
                    (applyCongfigToSpec spec table)
                    (\ (eu, em, _) ->
                        throwError ( eu
                                   , vcat [ "Error in applyConfigToSpec", pretty em ]
                                   , Just spec
                                   )
                    )


applyCongfigToSpec
    :: MonadConjure m
    => Spec
    -> M.Map (Text,Text) RuleReprResult
    -> m Spec
applyCongfigToSpec spec config = withBindingScope' $ do
    void $ recordSpec spec
    initialiseSpecState spec
    let
        isFindOrGiven [xMatch| _ := topLevel.declaration.find  |] = True
        isFindOrGiven [xMatch| _ := topLevel.declaration.given |] = True
        isFindOrGiven _ = False

        f p@[xMatch| [Prim (S nm)] := reference |] =
            case identifierSplit nm of
                (base, Just region, Nothing) ->
                    case M.lookup (base,region) config of
                        Nothing -> return p
                        Just (origDecl, _ruleName, reprName, newDom, cons) -> do
                            modify $ \ st -> st { representationLog = (base, reprName, origDecl, newDom)
                                                                    : representationLog st
                                                , structuralConsLog = cons ++ structuralConsLog st
                                                }
                            let nm' = identifierConstruct base (Just region) (Just reprName)
                            return [xMake| reference := [Prim (S nm')] |]
                _ -> return p
        f p = return p
    spec' <- bottomUpSpecExcept' isFindOrGiven f spec
    let pipeline = addChannellingFromLog >=>
                   addStructuralFromLog
    pipeline spec'


addStructuralFromLog :: MonadConjure m => Spec -> m Spec
addStructuralFromLog (Spec v xs) = do
    cs' <- gets structuralConsLog
    cs  <- mapM (liftM fst . runWriterT . simplify) cs'
    modify $ \ st -> st { structuralConsLog = [] }
    let mk i = [xMake| topLevel.suchThat := [i] |]
    return $ Spec v $ listAsStatement $ statementAsList xs ++ map mk cs


addChannellingFromLog :: MonadConjure m => Spec -> m Spec
addChannellingFromLog (Spec v xs) = do
    mapM_ introduceStuff (statementAsList xs)
    rlogs <- gets representationLog
    modify $ \ st -> st { representationLog = [] }

    let grouped = filter ( (>1) . length )
                $ groupBy ((==) `on` fst)
                $ sortBy (comparing fst)
                $ nub
                  [ (nm, reprName) | (nm, reprName, _, _) <- rlogs ]

    let newCons = [ [ [xMake| topLevel.suchThat := [theCons] |]
                    | ((nm1,r1),(nm2,r2)) <- allPairs one
                    , let x1 = [xMake| reference := [ Prim $ S $ mconcat [nm1, "#", r1] ] |]
                    , let x2 = [xMake| reference := [ Prim $ S $ mconcat [nm2, "#", r2] ] |]
                    , let theCons = [eMake| &x1 = &x2 |]
                    ]
                  | one <- grouped
                  ]

    let
        insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.suchThat  |] : _) = toInsert ++ rest
        insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.objective |] : _) = toInsert ++ rest
        insertBeforeSuchThat toInsert (i:is) = i : insertBeforeSuchThat toInsert is
        insertBeforeSuchThat toInsert []     = toInsert

    let
        mkWithNewDom :: (Text, Text, E, E) -> E
        mkWithNewDom (origName, reprName, [xMatch| _ := topLevel.declaration.find |], newDom ) =
            [xMake| topLevel.declaration.find.name.reference := [Prim (S $ mconcat [origName, "_", reprName])]
                  | topLevel.declaration.find.domain         := [newDom]
                  |]
        mkWithNewDom (origName, reprName, [xMatch| _ := topLevel.declaration.given |], newDom ) =
            [xMake| topLevel.declaration.given.name.reference := [Prim (S $ mconcat [origName, "_", reprName])]
                  | topLevel.declaration.given.domain         := [newDom]
                  |]
        mkWithNewDom _ = error "Impossible: addChannellingFromLog.mkWithNewDom"

    let newDecls = nub $ map mkWithNewDom rlogs
    mapM_ introduceStuff newDecls

    newDecls' <- mapM (liftM fst . runWriterT . simplify) newDecls
    mapM_ introduceStuff newDecls'

    newCons'  <- mapM (liftM fst . runWriterT . simplify) (concat newCons)

    mapM_ (mkLog "addedDecl" . pretty) newDecls'

    return $ Spec v $ listAsStatement $ insertBeforeSuchThat newDecls' (statementAsList xs) ++ newCons'


allPairs :: [a] -> [(a,a)]
allPairs [ ] = []
allPairs [_] = []
allPairs (x:xs) = map (x,) xs ++ allPairs xs

