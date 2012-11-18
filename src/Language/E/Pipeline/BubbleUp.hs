{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.BubbleUp where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )


conjureBubbleUp
    :: MonadConjure m
    => Spec
    -> m Spec
conjureBubbleUp = pipeline
    where
        pipeline = bubbleUpSpec
                >=> return . atMostOneSuchThat


bubbleUpSpec :: MonadConjure m => Spec -> m Spec
bubbleUpSpec (Spec v xsOrig) = withBindingScope' $ do
    let xs = statementAsList xsOrig
    (xs', locals) <- unzip <$> mapM bubbleUpE xs
    let (newDecls, newCons) = partition isDeclaration $ concat locals
    return $ Spec v $ listAsStatement $ insertBeforeSuchThat newDecls xs' ++ newCons


bubbleUpE :: MonadConjure m => E -> m (E, [E])
bubbleUpE [xMatch| [a] := withLocals.actual
                 | ls  := withLocals.locals
                 |] = do
    (a', aLocals) <- bubbleUpE a
    return (a', aLocals ++ ls)
bubbleUpE [xMatch| [quantifier] := quantified.quantifier
                 | [quanVar]    := quantified.quanVar.structural.single
                 | [dom]        := quantified.quanOverDom
                 | []           := quantified.quanOverOp
                 | []           := quantified.quanOverExpr
                 | [guard]      := quantified.guard
                 | [body]       := quantified.body
                 |] = do
    (body', locals) <- bubbleUpE body
    let (newDecls, newCons) = partition isDeclaration locals
    let declFix [xMatch| [Prim (S nm)] := topLevel.declaration.find.name.reference
                       | [d]           := topLevel.declaration.find.domain
                       |] = let ma = [xMake| domain.matrix.index := [dom]
                                           | domain.matrix.inner := [d]
                                           |]
                            -- in  ( nm
                            --     , [xMake| topLevel.declaration.dim.name.reference := [Prim (S nm)]
                            --             | topLevel.declaration.dim.domain := [ma]
                            --             |]
                            --     )
                            in  ( nm
                                , [xMake| topLevel.declaration.find.name.reference := [Prim (S nm)]
                                        | topLevel.declaration.find.domain := [ma]
                                        |]
                                )

        declFix x = error $ show $ "bubbleUpE.declFix" <+> pretty x
    let (names,newDecls') = unzip $ map declFix newDecls

    let liftName x = let f p@[xMatch| [Prim (S s)] := reference |]
                            | s `elem` names = [xMake| operator.index.left  := [p]
                                                     | operator.index.right := [quanVar]
                                                     |]
                         f p = p
                     in  transform f x

    let
        inLoop [xMatch| [x] := topLevel.suchThat |]
            = [xMake| topLevel.suchThat := [inLoop x]
                    |]
        inLoop b
            = [xMake| quantified.quantifier                := [quantifier]
                    | quantified.quanVar.structural.single := [quanVar]
                    | quantified.quanOverDom               := [dom]
                    | quantified.quanOverOp                := []
                    | quantified.quanOverExpr              := []
                    | quantified.guard                     := [guard]
                    | quantified.body                      := [b]
                    |]
    return ( (inLoop . liftName) body'
           , map (inLoop . liftName) newCons ++ newDecls'
           )
bubbleUpE p@[xMatch| _ := quantified |] = return (p, [])
bubbleUpE (Tagged t xs) = do
    (xs', locals) <- unzip <$> mapM bubbleUpE xs
    return (Tagged t xs', concat locals)
bubbleUpE x = return (x, [])




isDeclaration :: E -> Bool
isDeclaration [xMatch| _ := topLevel.declaration |] = True
isDeclaration _ = False


insertBeforeSuchThat :: [E] -> [E] -> [E]
insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.suchThat  |] : _) = toInsert ++ rest
insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.objective |] : _) = toInsert ++ rest
insertBeforeSuchThat toInsert (i:is) = i : insertBeforeSuchThat toInsert is
insertBeforeSuchThat toInsert []     = toInsert

