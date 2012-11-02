{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Traversals where

import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Pretty

import Data.Set as S



labelOf :: Pretty primitive => Generic primitive -> Doc
labelOf (Prim   p  ) = pretty p
labelOf (Tagged s _) = pretty s



universeSpecNoFindGiven :: Spec -> [E]
universeSpecNoFindGiven (Spec _ is) = concatMap f is
    where
        f [xMatch| _ := topLevel.declaration.find  |] = []
        f [xMatch| _ := topLevel.declaration.given |] = []
        f t = g t

        g t@(Tagged _ xs) = t : concatMap g xs
        g t               = [t]



traverseSpec'
    :: Monad m
    => (E -> CompE m E)
    -> Spec
    -> CompE m Spec
traverseSpec' func spec = runIdentityT $ traverseSpec (lift . func) spec



traverseSpec
    :: ( Monad (t (CompEMOnly m))
       , Monad m
       , MonadTrans t
       )
    => (E -> t (CompEMOnly m) E)
    -> Spec
    -> t (CompEMOnly m) Spec
traverseSpec func spec@(Spec v xs) = do
    lift $ initialiseSpecState spec
    lift $ mapM_ introduceStuff xs
    xs' <- mapM (traverseE func) xs
    return $ Spec v xs'



initialiseSpecState :: Monad m => Spec -> CompE m ()
initialiseSpecState (Spec _ statements) = do
    let names = [ nm
                | statement <- statements
                , [xMatch| [Prim (S nm)] := reference |] <- universe statement
                , "v__" `isPrefixOf` nm
                ]
    modifyLocal $ \ st -> st { allNamesPreConjure = S.fromList names }


traverseSpecNoFindGiven'
    :: Monad m
    => (E -> CompE m E)
    -> Spec
    -> CompE m Spec
traverseSpecNoFindGiven' func spec = runIdentityT $ traverseSpecNoFindGiven (lift . func) spec



traverseSpecNoFindGiven
    :: ( Monad (t (CompEMOnly m))
       , Monad m
       , MonadTrans t
       )
    => (E -> t (CompEMOnly m) E)
    -> Spec
    -> t (CompEMOnly m) Spec
traverseSpecNoFindGiven func spec@(Spec v xs) = do
    lift $ initialiseSpecState spec
    lift $ mapM_ introduceStuff xs
    xs' <- mapM (\ x -> case x of
                    [xMatch| _ := topLevel.declaration.find  |] -> return x
                    [xMatch| _ := topLevel.declaration.given |] -> return x
                    _                                           -> traverseE func x
                ) xs
    return $ Spec v xs'



traverseE
    :: ( Monad (t (CompEMOnly m))
       , Monad m
       , MonadTrans t
       )
    => (E -> t (CompEMOnly m) E)
    -> E
    -> t (CompEMOnly m) E
traverseE func t = do
    bindersBefore <- lift $ getsLocal binders
    lift $ introduceStuff t
    result <- case t of
        Tagged s xs -> do xs' <- mapM (traverseE func) xs ; func (Tagged s xs')
        _           ->                                      func t
    lift $ modifyLocal $ \ st -> st { binders = bindersBefore }
    return result



introduceStuff :: Monad m => E -> CompE m ()

introduceStuff x@[xMatch| [Prim (S name)] := topLevel.declaration.find .name.reference |] = addBinder name x

introduceStuff x@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference |] = addBinder name x

introduceStuff   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.expr           |] = addBinder name x
introduceStuff   [xMatch| [Prim (S name)] := topLevel.letting.name.metavar
                        | [ x ]           := topLevel.letting.expr           |] = addBinder ('&':name) x

introduceStuff   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.domain         |] = addBinder name x
introduceStuff   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.domain         |] = addBinder ('&':name) x

introduceStuff x@[xMatch| [Prim (S name)] := topLevel.letting.name.reference |] = addBinder name x
introduceStuff x@[xMatch| [Prim (S name)] := topLevel.letting.name.metavar   |] = addBinder ('&':name) x

introduceStuff   [xMatch| _ := topLevel.suchThat  |] = return ()
introduceStuff   [xMatch| _ := topLevel.objective |] = return ()
introduceStuff   [xMatch| _ := topLevel.where     |] = return ()

introduceStuff x@[xMatch| [Prim (S name)] := topLevel.declaration.dim  .name.reference |] = addBinder name x
introduceStuff   [xMatch| _  := topLevel.declaration.nestedDimFind |] = return ()

introduceStuff x@[xMatch| _ := topLevel |]
    = err ErrFatal $ "not handled in processStatement" <+> prettyAsPaths x

introduceStuff   [xMatch| ls := withLocals.locals |] = mapM_ introduceStuff ls

introduceStuff x@[xMatch| [Prim (S r)] := quantified.quanVar.structural.single.reference |]
    = addBinder r [xMake| quanVar.name   := [Prim (S r)]
                        | quanVar.within := [x]
                        |]

introduceStuff _ = return ()



printAllBound :: Monad m => String -> CompE m ()
printAllBound s = do
    bs <- getsLocal binders
    let names = [ i | Binder i _ <- bs ]
    mkLog ("----- ----- ----- ----- " ++ s) $ prettyList id "," names
    return ()



