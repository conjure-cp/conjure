{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.Traversals where

import Stuff.Generic
import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty



labelOf :: Pretty primitive => Generic primitive -> Doc
labelOf (Prim   p  ) = pretty p
labelOf (Tagged s _) = stringToDoc s


traverseSpec :: (Monad m)
    => Maybe (E -> CompE m E)
    -> (E -> CompE m E)
    -> Maybe (E -> CompE m E)
    -> Spec
    -> CompE m Spec
traverseSpec mpre func mpost (Spec v xs) = do
    forM_ xs $ \ x -> mkLog "debug:traverseSpec" $ prettyAsPaths x
    xs' <- mapM (traverse mpre func mpost) xs
    return $ Spec v xs'

traverseSpecNoFindGiven :: (Monad m)
    => Maybe (E -> CompE m E)
    -> (E -> CompE m E)
    -> Maybe (E -> CompE m E)
    -> Spec
    -> CompE m Spec
traverseSpecNoFindGiven mpre func mpost (Spec v xs) = do
    forM_ xs $ \ x -> mkLog "debug:traverseSpec" $ prettyAsPaths x
    xs' <- forM xs $ \ x -> case x of
            [xMatch| _ := topLevel.declaration.find  |] -> return x
            [xMatch| _ := topLevel.declaration.given |] -> return x
            _ -> traverse mpre func mpost x
    return $ Spec v xs'


traverse :: (Monad m)
    => Maybe (E -> CompE m E)
    -> (E -> CompE m E)
    -> Maybe (E -> CompE m E)
    -> E
    -> CompE m E
traverse mpre func mpost t = do
    mkLog "debug: ==>    " $ labelOf t
    -- printAllBound "1"
    bindersBefore <- getsLocal binders
    introduceStuff t
    result <- case mpre of
        Nothing  -> do
            -- mkLog "traverse" "no preorder modification"
            afterPre mpre func mpost t
        Just pre -> do
            t' <- pre t
            mkLog "traverse" $ "after pre:" <+> labelOf t
            afterPre mpre func mpost t'
    modifyLocal $ \ st -> st { binders = bindersBefore }
    -- printAllBound "2"
    return result


afterPre :: (Monad m)
    => Maybe (E -> CompE m E)
    -> (E -> CompE m E)
    -> Maybe (E -> CompE m E)
    -> E
    -> CompE m E
afterPre mpre func mpost t = do
    -- mkLog "afterPre" $ labelOf t
    t' <- case t of
        Tagged s xs -> do
            xs' <- mapM (traverse mpre func mpost) xs
            func (Tagged s xs')
        _ -> func t
    mkLog "debug:    ==> " $ labelOf t'
    -- printAllBound ()
    case mpost of 
        Nothing -> do
            -- mkLog "traverse" "no postorder modification"
            return t'
        Just post -> do
            t'' <- post t'
            mkLog "traverse" $ "after post:" <+> labelOf t''
            return t''


introduceStuff :: Monad m => E -> CompE m ()
introduceStuff
    s@[xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
             | [      _      ] := topLevel.declaration.find.domain
             |] = do
                 mkLog "debug:introduceStuff" $ "find" <+> stringToDoc name
                 addBinder name s
introduceStuff
    s@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference
             | [      _      ] := topLevel.declaration.given.domain
             |] = addBinder name s
introduceStuff
      [xMatch| [Prim (S name)] := topLevel.declaration.letting.name.reference
             | [ expression ]  := topLevel.declaration.letting.expr
             |] = addBinder name expression
introduceStuff
    [xMatch| ls := withLocals.locals |] = do
        mapM_ introduceStuff ls
        mkLog "debug:introduceStuff" $ vcat $ map pretty ls
introduceStuff _ = do
    -- mkLog "introduceStuff" $ pretty x
    return ()


printAllBound :: Monad m => String -> CompE m ()
printAllBound s = do
    bs <- getsLocal binders
    let names = [ i | Binder i _ <- bs ]
    mkLog ("----- ----- ----- ----- " ++ s) $ prettyList id "," names
    return ()

