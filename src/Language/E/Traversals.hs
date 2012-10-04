{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Traversals where

import Stuff.Generic
import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty



labelOf :: Pretty primitive => Generic primitive -> Doc
labelOf (Prim   p  ) = pretty p
labelOf (Tagged s _) = pretty s


traverseSpec :: (Monad m)
    => Maybe (E -> CompE m E)
    -> (E -> CompE m E)
    -> Maybe (E -> CompE m E)
    -> Spec
    -> CompE m Spec
traverseSpec mpre func mpost (Spec v xs) = do
    -- forM_ xs $ \ x -> mkLog "debug:traverseSpec" $ prettyAsPaths x
    mapM_ processStatement xs
    xs' <- mapM (traverse mpre func mpost) xs
    return $ Spec v xs'

traverseSpecNoFindGiven :: (Monad m)
    => Maybe (E -> CompE m E)
    -> (E -> CompE m E)
    -> Maybe (E -> CompE m E)
    -> Spec
    -> CompE m Spec
traverseSpecNoFindGiven mpre func mpost (Spec v xs) = do
    xs' <- forM xs $ \ x -> case x of
            [xMatch| _ := topLevel.declaration.find  |] -> return x
            [xMatch| _ := topLevel.declaration.given |] -> return x
            _ -> traverse mpre func mpost x
    return $ Spec v xs'

universeSpecNoFindGiven :: Spec -> [E]
universeSpecNoFindGiven (Spec _ is) = concatMap f is
    where
        f [xMatch| _ := topLevel.declaration.find  |] = []
        f [xMatch| _ := topLevel.declaration.given |] = []
        f t@(Tagged _ xs) = t : concatMap universe xs
        f t               = [t]

traverse :: (Monad m)
    => Maybe (E -> CompE m E)
    -> (E -> CompE m E)
    -> Maybe (E -> CompE m E)
    -> E
    -> CompE m E
traverse mpre func mpost t = do
    bindersBefore <- getsLocal binders
    introduceStuff t
    result <- case mpre of
        Nothing  ->
            afterPre mpre func mpost t
        Just pre -> do
            t' <- pre t
            mkLog "traverse" $ "after pre:" <+> labelOf t
            afterPre mpre func mpost t'
    modifyLocal $ \ st -> st { binders = bindersBefore }
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
    -- mkLog "debug:    ==> " $ labelOf t'
    -- printAllBound ()
    case mpost of 
        Nothing ->
            return t'
        Just post -> do
            t'' <- post t'
            mkLog "traverse" $ "after post:" <+> labelOf t''
            return t''


introduceStuff :: Monad m => E -> CompE m ()
introduceStuff
    s@[xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
             | [      _      ] := topLevel.declaration.find.domain
             |] = addBinder name s
introduceStuff
    s@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference
             | [      _      ] := topLevel.declaration.given.domain
             |] = addBinder name s
introduceStuff
      [xMatch| [Prim (S name)] := topLevel.declaration.letting.name.reference
             | [ expression ]  := topLevel.declaration.letting.expr
             |] = addBinder name expression
introduceStuff
    [xMatch| ls := withLocals.locals |] = mapM_ introduceStuff ls
introduceStuff _ = return ()


printAllBound :: Monad m => String -> CompE m ()
printAllBound s = do
    bs <- getsLocal binders
    let names = [ i | Binder i _ <- bs ]
    mkLog ("----- ----- ----- ----- " ++ s) $ prettyList id "," names
    return ()

