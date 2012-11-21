{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.ApplyRepr ( applyRepr ) where

import Language.E
import Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction )
import Language.E.BuiltIn ( builtInRepr, mergeReprFunc )

import qualified Data.Map as M


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
            nbOccurrence :: Text -> Int
            nbOccurrence nm = length [ ()
                                     | [xMatch| [Prim (S nm')] := reference |] <- universeSpecNoFindGiven spec
                                     , nm == nm'
                                     ]

            foo :: [(a,[b])] -> [[(a,b)]]
            foo [] = [[]]
            foo ((x,ys):qs) = concat [ [ (x,y) : ws | y <- ys ] |  ws <- foo qs ]

            lookupTables :: [ M.Map Text [RuleReprResult] ]
            lookupTables = map M.fromList $ foo
                [ (nm, allCombs)
                | (nm, results) <- candidates
                , let cnt = nbOccurrence nm
                , cnt > 0
                , let allCombs = replicateM cnt results
                ]

        table <- returns lookupTables
        if M.null table
            then returns []
            else do
                let configStr = hsep $ concat [ map (\ i -> pretty nm <> "#" <> i ) nms
                                              | (nm, rs) <- M.toList table
                                              , let nms = map (\ (_,_,i,_,_) -> pretty i ) rs
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
    -> M.Map Text [RuleReprResult]
    -> m Spec
applyCongfigToSpec spec initConfig = withBindingScope' $ do
    void $ recordSpec spec
    initialiseSpecState spec
    let
        isFindOrGiven [xMatch| _ := topLevel.declaration.find  |] = True
        isFindOrGiven [xMatch| _ := topLevel.declaration.given |] = True
        isFindOrGiven _ = False

        f p@[xMatch| [Prim (S nm)] := reference |] = do
            config <- gets representationConfig
            case M.lookup nm config of
                Nothing -> return p
                Just [] -> err ErrFatal "applyCongfigToSpec.f -- empty list"
                Just ((origDecl, _ruleName, reprName, newDom, cons):rest) -> do
                    modify $ \ st -> st { representationLog = (nm, reprName, origDecl, newDom)
                                                            : representationLog st
                                        , structuralConsLog = cons ++ structuralConsLog st
                                        , representationConfig = M.insert nm rest config
                                        }
                    return [xMake| reference := [Prim (S $ mconcat [nm, "#", reprName])] |]
        f p = return p
    modify $ \ st -> st { representationConfig = initConfig }
    spec' <- bottomUpSpecExcept' isFindOrGiven f spec
    modify $ \ st -> st { representationConfig = def }
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


domainNeedsRepresentation :: E -> Bool
domainNeedsRepresentation [xMatch|  _  := domain.set          |] = True
domainNeedsRepresentation [xMatch|  _  := domain.mset         |] = True
domainNeedsRepresentation [xMatch|  _  := domain.function     |] = True
domainNeedsRepresentation [xMatch|  _  := domain.relation     |] = True
domainNeedsRepresentation [xMatch|  _  := domain.partition    |] = True
domainNeedsRepresentation [xMatch| [i] := domain.matrix.inner |] = domainNeedsRepresentation i
domainNeedsRepresentation _ = False


allPairs :: [a] -> [(a,a)]
allPairs [ ] = []
allPairs [_] = []
allPairs (x:xs) = map (x,) xs ++ allPairs xs

