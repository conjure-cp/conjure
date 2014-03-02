{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.BubbleUp ( bubbleUpSpec ) where

import Language.E



bubbleUpSpec :: MonadConjure m => Spec -> m Spec
bubbleUpSpec spec = do
    (Spec v stmt, vars) <- runWriterT $ bottomUpSpec (logged bubbleUpE) spec
    return $ Spec v $ listAsStatement $ insertBeforeSuchThat vars $ statementAsList stmt

logged :: (Monad (t m), MonadTrans t, MonadConjure m) => (E -> t m (Maybe E)) -> E -> t m E
logged f x = do
    my <- f x
    case my of
        Nothing -> return x
        Just y  -> do
            -- lift $ mkLog "bubbleUp" $ sep [pretty x, "~~>", pretty y]
            return y


-- {a @ vars,cons} ~~> {a /\ cons @ vars}           where a is bool:    cons turned into a conjunction, vars stay in the bubble
-- op {a @ vars,cons} ~~> {op a @ vars,cons}        where a isn't bool: vars&cons bubble up together
-- op {a @ vars} ~~> {op a @ vars}                  when no cons is left, vars still bubble up

bubbleUpE :: MonadConjure m => E -> WriterT [E] m (Maybe E)
bubbleUpE (Tagged t xs) = do
    ys <- lift $ mapM (\ x -> do y <- onChildren x
                                 -- mkLog "onChildren" $ pretty y
                                 return y
                      )
                      xs
    let allKeep = all isKeep ys
    let (zs,locals',vars) = unzip3
            [ case y of
                Keep z           -> (z,[],[])
                ReplacedChild z  -> (z,[],[])
                BubbleUp z ls    -> (z,ls,[])
                DeclareVars z vs -> (z,[],vs)
            | y <- ys
            ]
    let locals = concat locals'
    tell $ concat vars
    let out = Tagged t zs
    if allKeep
        then return Nothing
        else if null locals
                then return $ Just out
                else return $ Just [xMake| withLocals.actual := [out]
                                         | withLocals.locals := locals
                                         |]
bubbleUpE _ = return Nothing

data OnChildrenResult = Keep E | ReplacedChild E | BubbleUp E [E] | DeclareVars E [E]

instance Pretty OnChildrenResult where
    pretty (Keep x) = "Keep:" <+> pretty x
    pretty (ReplacedChild x) = "ReplacedChild:" <+> pretty x
    pretty (BubbleUp x ys) = "BubbleUp:" <+> vcat (pretty x : map pretty ys)
    pretty (DeclareVars x ys) = "DeclareVars:" <+> vcat (pretty x : map pretty ys)

isKeep :: OnChildrenResult -> Bool
isKeep Keep{} = True
isKeep _ = False

onChildren :: MonadConjure m => E -> m OnChildrenResult
onChildren [xMatch| [actual] := withLocals.actual
                  | locals   := withLocals.locals
                  |]
    | let (varsList,consList) = partition isDeclaration locals
    , consList /= []
    = do
        let cons = conjunct (map outOfSuchThat consList)
        tyActual <- typeOf actual `catchError` (const $ return [xMake| type.unknown := [] |])
        case tyActual of
            [xMatch| [] := type.bool |] ->
                let a = [eMake| &actual /\ &cons |]
                in  if null varsList
                        then return $ ReplacedChild a
                        else return $ ReplacedChild [xMake| withLocals.actual := [a]
                                                          | withLocals.locals := varsList
                                                          |]
            _ -> return $ BubbleUp actual locals
onChildren [xMatch| [actual] := withLocals.actual
                  | locals   := withLocals.locals
                  |] = return $ BubbleUp actual locals
onChildren [xMatch| [actual] := suchThat.withLocals.actual
                  | locals   := suchThat.withLocals.locals
                  |]
    | let (varsList,consList) = partition isDeclaration locals
    , consList == []
    = return $ DeclareVars [xMake| suchThat := [actual] |] varsList
onChildren p = return $ Keep p




isDeclaration :: E -> Bool
isDeclaration [xMatch| _ := topLevel.declaration |] = True
isDeclaration _ = False

outOfSuchThat :: E -> E
outOfSuchThat [xMatch| [x] := topLevel.suchThat |] = x
outOfSuchThat x = x

insertBeforeSuchThat :: [E] -> [E] -> [E]
insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.suchThat  |] : _) = toInsert ++ rest
insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.objective |] : _) = toInsert ++ rest
insertBeforeSuchThat toInsert (i:is) = i : insertBeforeSuchThat toInsert is
insertBeforeSuchThat toInsert []     = toInsert

