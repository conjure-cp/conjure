{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.ApplyRepr ( applyRepr ) where

import Language.E
import Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction )
import Language.E.BuiltIn ( builtInRepr, mergeReprFunc )

import qualified Data.Map as M


applyRepr :: (Functor m, Monad m) => [RuleRepr] -> Spec -> CompE m Spec
applyRepr rules spec = let mfunc = ruleReprToFunction rules in case mfunc of
    Left es     -> err ErrFatal $ prettyErrors "There were errors." $ map (,Nothing) es
    Right func' -> do

        let func = mergeReprFunc (func' : builtInRepr)

        let Spec _ statements = spec

        mapM_ processStatement statements

        let topLevels' = [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.find .name.reference
                                              | [d]          := topLevel.declaration.find .domain |] <- statements ]
                      ++ [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.given.name.reference
                                              | [d]          := topLevel.declaration.given.domain |] <- statements ]
        let topLevels = [ (x,n,d) | (x,n,d) <- topLevels', domainNeedsRepresentation d ]

        candidates :: [(String,[RuleReprResult])]
            <- forM topLevels $ \ (x,n,d) -> do
            ys <- func (n,d,x)
            case ys of
                [] -> err ErrFatal $ "No representation rule matches domain:" <+> pretty x
                _  -> do
                    let ysNames = flip map ys $ \ (_origDecl, _ruleName, reprName, _newDom, _cons) -> reprName
                    mkLog "representation" $ vcat [ pretty x
                                                  , "(#" <> pretty (length ys) <> ")" <+> prettyList id "," ysNames
                                                  ] 
                    return (n,ys)

        let
            nbOccurrence :: String -> Int
            nbOccurrence nm = length [ ()
                                     | [xMatch| [Prim (S nm')] := reference |] <- universeSpecNoFindGiven spec
                                     , nm == nm'
                                     ]

            foo :: [(a,[b])] -> [[(a,b)]]
            foo [] = [[]]
            foo ((x,ys):qs) = concat [ [ (x,y) : ws | y <- ys ] |  ws <- foo qs ]

            lookupTables :: [ M.Map String [RuleReprResult] ]
            lookupTables = map M.fromList $ foo $
                [ (nm, allCombs)
                | (nm, results) <- candidates
                , let cnt = nbOccurrence nm
                , cnt > 0
                , let allCombs = replicateM cnt results
                ]

        table <- returns lookupTables
        applyCongfigToSpec spec table


applyCongfigToSpec :: (Functor m, Monad m) => Spec -> M.Map String [RuleReprResult] -> CompE m Spec
applyCongfigToSpec spec initConfig = do
    let
        f p@[xMatch| [Prim (S nm)] := reference |] = do
            config <- getsLocal representationConfig
            case M.lookup nm config of
                Nothing -> return p
                Just [] -> err ErrFatal "applyCongfigToSpec.f -- empty list"
                Just ((origDecl, _ruleName, reprName, newDom, cons):rest) -> do
                    modifyLocal $ \ st -> st { representationLog = (nm, reprName, origDecl, newDom) : representationLog st
                                             , structuralConsLog = cons ++ structuralConsLog st
                                             , representationConfig = M.insert nm rest config
                                             }
                    return [xMake| reference := [Prim (S $ nm ++ "#" ++ reprName)] |]
        f p = return p
    modifyLocal $ \ st -> st { representationConfig = initConfig }
    spec' <- traverseSpecNoFindGiven Nothing f Nothing spec
    modifyLocal $ \ st -> st { representationConfig = def }
    let pipeline = addStructuralFromLog >=>
                   addChannellingFromLog
    pipeline spec'


addStructuralFromLog :: (Functor m, Monad m) => Spec -> CompE m Spec
addStructuralFromLog (Spec v xs) = do
    cs' <- getsLocal structuralConsLog
    cs  <- mapM trySimplifyE cs'
    modifyLocal $ \ st -> st { structuralConsLog = [] }
    let mk i = [xMake| topLevel.suchThat := [i] |]
    return $ Spec v $ xs ++ map mk cs


addChannellingFromLog :: (Functor m, Monad m) => Spec -> CompE m Spec
addChannellingFromLog (Spec v xs) = do
    rlogs <- getsLocal representationLog
    modifyLocal $ \ st -> st { representationLog = [] }

    let grouped = filter ( (>1) . length )
                $ groupBy ((==) `on` fst)
                $ sortBy (comparing fst)
                $ nub
                  [ (nm, reprName) | (nm, reprName, _, _) <- rlogs ]

    let newCons = [ [ [xMake| topLevel.suchThat.binOp.operator        := [Prim (S "=")]
                            | topLevel.suchThat.binOp.left .reference := [ Prim $ S $ nm1 ++ "#" ++ r1 ]
                            | topLevel.suchThat.binOp.right.reference := [ Prim $ S $ nm2 ++ "#" ++ r2 ]
                            |]
                    | ((nm1,r1),(nm2,r2)) <- allPairs one
                    ]
                  | one <- grouped
                  ]

    let
        mk (origName, reprName, [xMatch| _ := topLevel.declaration.find |], newDom ) =
            [xMake| topLevel.declaration.find.name.reference := [Prim (S $ origName ++ "_" ++ reprName)]
                  | topLevel.declaration.find.domain         := [newDom]
                  |]
        mk (origName, reprName, [xMatch| _ := topLevel.declaration.given |], newDom ) =
            [xMake| topLevel.declaration.given.name.reference := [Prim (S $ origName ++ "_" ++ reprName)]
                  | topLevel.declaration.given.domain         := [newDom]
                  |]
        mk _ = error "Impossible: addChannellingFromLog.mk"

    let
        insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.suchThat  |] : _) = toInsert ++ rest
        insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.objective |] : _) = toInsert ++ rest
        insertBeforeSuchThat toInsert (i:is) = i : insertBeforeSuchThat toInsert is
        insertBeforeSuchThat toInsert []     = toInsert

    let
        newDecls = nub $ map mk rlogs

    newDecls' <- mapM trySimplifyE newDecls
    newCons'  <- mapM trySimplifyE (concat newCons)

    return $ Spec v $ insertBeforeSuchThat newDecls' xs ++ newCons'


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
