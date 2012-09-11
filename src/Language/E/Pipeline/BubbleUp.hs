{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.Pipeline.BubbleUp where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )


conjureBubbleUp :: (Monad m, Functor m)
    => (FilePath, Text)
    -> CompE m Spec
conjureBubbleUp spectobe = do
    spec  <- readSpec spectobe
    (bubbleUpSpec >=> return . atMostOneSuchThat) spec

readSpec :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompE m Spec
readSpec (fp,con) =
    case runLexerAndParser parseSpec fp con of
        Left  e -> err ErrFatal e
        Right x -> return $ atMostOneSuchThat x




bubbleUpSpec :: (Functor m, Monad m) => Spec -> CompE m Spec
bubbleUpSpec (Spec v xs) = do
    (xs', locals) <- unzip <$> mapM bubbleUpE xs
    let
        isDeclaration [xMatch| _ := topLevel.declaration |] = True
        isDeclaration _ = False
    let
        insertBeforeSuchThat toInsert rest@([xMatch| _ := topLevel.suchThat |] : _) = toInsert ++ rest
        insertBeforeSuchThat toInsert (i:is) = i : insertBeforeSuchThat toInsert is
        insertBeforeSuchThat toInsert []     = toInsert

    let (newDecls, newCons) = partition isDeclaration $ concat locals

    return $ Spec v $ insertBeforeSuchThat newDecls xs' ++ newCons


bubbleUpE :: (Functor m, Monad m) => E -> CompE m (E, [E])
bubbleUpE [xMatch| [a] := withLocals.actual
                 | ls  := withLocals.locals
                 |] = do
    (a', aLocals) <- bubbleUpE a
    return (a', aLocals ++ ls)
bubbleUpE (Tagged t xs) = do
    (xs', locals) <- unzip <$> mapM bubbleUpE xs
    return $ (Tagged t xs', concat locals)
bubbleUpE x = return (x, [])

