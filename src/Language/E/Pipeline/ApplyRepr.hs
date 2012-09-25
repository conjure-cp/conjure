{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Language.E.Pipeline.ApplyRepr where

import Language.E
import Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.BuiltInRepr


applyRepr :: (Functor m, Monad m) => Spec -> [RuleRepr] -> CompE m Spec
applyRepr spec rules = let mfunc = ruleReprToFunction rules in case mfunc of
    Left es     -> err ErrFatal $ prettyErrors "There were errors." es
    Right func' -> do

        let func = mergeReprFunc [func',builtInRepr]

        let Spec _ statements = spec

        mapM_ processStatement statements

        let topLevels' = [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.find .name.reference
                                              | [d]          := topLevel.declaration.find .domain |] <- statements ]
                      ++ [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.given.name.reference
                                              | [d]          := topLevel.declaration.given.domain |] <- statements ]
        let topLevels = [ (x,n,d) | (x,n,d) <- topLevels', domainNeedsRepresentation d ]

        candidates <- forM topLevels $ \ (x,n,d) -> do
            ys <- func (n,d)
            case ys of
                [] -> err ErrFatal $ "No representation rule matches domain:" <+> pretty x
                _  -> do
                    let ysNames = flip map ys $ \ (_ruleName, reprName, _newDom, _cons) -> reprName
                    mkLog "representation" $ vcat [ pretty x
                                                  , "(#" <> pretty (length ys) <> ")" <+> prettyList id "," ysNames
                                                  ] 
                    -- forM_ ys $ \ (ruleName, reprName, newDom, cons) -> do
                    --     mkLog "newDom" $ pretty newDom
                    --     forM_ cons $ \ con -> mkLog "cons" $ pretty con
                    return (n,(x,ys))

        let f p@[xMatch| [Prim (S nm)] := reference |] = case lookup nm candidates of
                Nothing     -> return p
                Just (originalDecl,ys) -> do
                    (_ruleName, reprName, newDom, cons) <- returns ys
                    modifyLocal $ \ st -> st { representationLog = (nm, reprName, originalDecl, newDom) : representationLog st }
                    modifyLocal $ \ st -> st { structuralConsLog = cons ++ structuralConsLog st }
                    return [xMake| reference := [Prim (S $ nm ++ "#" ++ reprName)] |]

                    -- news <- forM ys $ \ (ruleName, reprName, newDom, cons) -> do
                    --             modifyLocal $ \ st -> st { representationLog = (nm,reprName, newDom) : representationLog st }
                    --             modifyLocal $ \ st -> st { structuralConsLog = cons ++ structuralConsLog st }
                    --             return [xMake| reference := [Prim (S $ nm ++ "#" ++ reprName)] |]
                    -- returns news
            f p = return p

        spec' <- traverseSpecNoFindGiven Nothing f Nothing spec

        let addStructuralFromLog (Spec v xs) = do
                cs <- getsLocal structuralConsLog
                modifyLocal $ \ st -> st { structuralConsLog = [] }
                let mk i = [xMake| topLevel.suchThat := [i] |]
                return $ Spec v $ xs ++ map mk cs

        let addChannellingFromLog (Spec v xs) = do
                rlogs <- getsLocal representationLog
                modifyLocal $ \ st -> st { representationLog = [] }

                let grouped = filter ( (>1) . length )
                            $ groupBy ((==) `on` fst)
                            $ sortBy (comparing fst)
                            $ nub
                            $ [ (nm, reprName) | (nm, reprName, _, _) <- rlogs ]

                let newCons = [ [ [xMake| topLevel.suchThat.binOp.operator        := [Prim (S "=")]
                                        | topLevel.suchThat.binOp.left .reference := [ Prim $ S $ nm1 ++ "#" ++ r1 ]
                                        | topLevel.suchThat.binOp.right.reference := [ Prim $ S $ nm2 ++ "#" ++ r2 ]
                                        |]
                                | ((nm1,r1),(nm2,r2)) <- allPairs one
                                ]
                              | one <- grouped
                              ]

                -- mkLog "addChannellingFromLog" $ vcat $ map pretty newCons

                let
                    mk (origName, reprName, [xMatch| _ := topLevel.declaration.find |], newDom ) =
                        [xMake| topLevel.declaration.find.name   := [Prim (S $ origName ++ "_" ++ reprName)]
                              | topLevel.declaration.find.domain := [newDom]
                              |]
                    mk (origName, reprName, [xMatch| _ := topLevel.declaration.given |], newDom ) =
                        [xMake| topLevel.declaration.given.name   := [Prim (S $ origName ++ "_" ++ reprName)]
                              | topLevel.declaration.given.domain := [newDom]
                              |]
                    mk _ = error "Impossible: addChannellingFromLog.mk"

                let
                    insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.suchThat  |] : _) = toInsert ++ rest
                    insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.objective |] : _) = toInsert ++ rest
                    insertBeforeSuchThat toInsert (i:is) = i : insertBeforeSuchThat toInsert is
                    insertBeforeSuchThat toInsert []     = toInsert

                let
                    newDecls = nub $ map mk rlogs

                return $ Spec v $ insertBeforeSuchThat newDecls xs ++ concat newCons

        ( trySimplifySpec <=< return . atMostOneSuchThat
                          <=< addChannellingFromLog
                          <=< addStructuralFromLog ) spec'

        -- forM_ topLevels $ \ (i,_,_) -> mkLog "applyRepr" $ prettyAsPaths i
        -- ys <- func $ head [ (n,d) | (_,n,d) <- topLevels ]
        -- case ys of
        --     [] -> return spec
        --     _  -> return spec
        -- -- return spec


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
