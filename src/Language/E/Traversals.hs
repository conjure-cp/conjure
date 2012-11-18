{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Traversals where

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Pretty

import qualified Data.Set as S



labelOf :: Pretty primitive => Generic primitive -> Doc
labelOf (Prim   p  ) = pretty p
labelOf (Tagged s _) = pretty s



universeExcept :: (E -> Bool) -> Spec -> [E]
universeExcept p (Spec _ s) = f s
    where
        f t | p t         = []
        f t@(Tagged _ xs) = t : concatMap f xs
        f t               = [t]

universeSpecNoFindGiven :: Spec -> [E]
universeSpecNoFindGiven = universeExcept p
    where
        p [xMatch| _ := topLevel.declaration.find  |] = True
        p [xMatch| _ := topLevel.declaration.given |] = True
        p _ = False



withBindingScope'
    :: MonadConjure m
    => m a
    -> m a
withBindingScope' = runIdentityT . withBindingScope . lift



withBindingScope
    :: ( Monad m
       , MonadTrans t
       , Monad (t m)
       , MonadState ConjureState m
       )
    => t m a
    -> t m a
withBindingScope comp = do
    bindersBefore <- lift $ gets binders
    result <- comp
    -- bindersAfter  <- lift $ gets binders
    -- lift $ modify $ \ st -> st { binders = bindersBefore }
    -- let removed = nub [ nm | Binder nm _ <- bindersAfter  , head nm /= '&' ] \\
                  -- nub [ nm | Binder nm _ <- bindersBefore ]
    -- case removed of
        -- [] -> return result
        -- _  -> trace ("removed: " ++ show removed) $ return result
    lift $ modify $ \ st -> st { binders = bindersBefore }
    return result



bottomUpSpec'
    :: MonadConjure m
    => (E -> m E)
    -> Spec
    -> m Spec
bottomUpSpec' = bottomUpSpecExcept' (const False)



bottomUpSpecExcept'
    :: MonadConjure m
    => (E -> Bool)
    -> (E -> m E)
    -> Spec
    -> m Spec
bottomUpSpecExcept' p func spec = runIdentityT $ bottomUpSpecExcept p (lift . func) spec



bottomUpSpec
    :: ( MonadConjure m
       , MonadTrans t
       , Monad (t m)
       )
    => (E -> t m E)
    -> Spec
    -> t m Spec
bottomUpSpec = bottomUpSpecExcept (const False)



bottomUpSpecExcept
    :: ( MonadConjure m
       , MonadTrans t
       , Monad (t m)
       )
    => (E -> Bool)
    -> (E -> t m E)
    -> Spec
    -> t m Spec
bottomUpSpecExcept p func spec@(Spec v xs) = withBindingScope $ do
    lift $ initialiseSpecState spec
    xs' <- bottomUpEExcept p func xs
    return $ Spec v xs'



bottomUpE'
    :: MonadConjure m
    => (E -> m E)
    -> E
    -> m E
bottomUpE' = bottomUpEExcept' (const False)



bottomUpEExcept'
    :: MonadConjure m
    => (E -> Bool)
    -> (E -> m E)
    -> E
    -> m E
bottomUpEExcept' p func x = runIdentityT $ bottomUpEExcept p (lift . func) x



bottomUpE
    :: ( MonadConjure m
       , MonadTrans t
       , Monad (t m)
       )
    => (E -> t m E)
    -> E
    -> t m E
bottomUpE = bottomUpEExcept (const False)



bottomUpEExcept
    :: ( MonadConjure m
       , MonadTrans t
       , Monad (t m)
       )
    => (E -> Bool)
    -> (E -> t m E)
    -> E
    -> t m E
bottomUpEExcept p func x = withBindingScope $ helper x
    where
        helper [xMatch| [this] := statement.this
                      | [next] := statement.next
                      |] = do
            lift $ introduceStuff this
            this' <- func =<< bottomUpEExcept p func this
            lift $ introduceStuff this'
            next' <- func =<< bottomUpEExcept p func next
            return [xMake| statement.this := [this']
                         | statement.next := [next']
                         |]
        helper t | p t = return t
        helper t = do
            lift $ introduceStuff t
            case t of
                Tagged s xs -> do
                    xs' <- mapM helper xs
                    let t' = Tagged s xs'
                    lift $ introduceStuff t'
                    func t'
                _           -> func t



initialiseSpecState :: MonadConjure m => Spec -> m ()
initialiseSpecState (Spec _ statements) = do
    let names = [ nm
                | [xMatch| [Prim (S nm)] := reference |] <- universe statements
                , "v__" `isPrefixOf` nm
                ]
    modify $ \ st -> st { allNamesPreConjure = S.fromList names }

introduceStuff :: MonadConjure m => E -> m ()
introduceStuff = helper
    where

        helper x@[xMatch| [Prim (S name)] := topLevel.declaration.find .name.reference |] = addBinder name x

        helper x@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference |] = addBinder name x

        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.expr           |] = addBinder name x
        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.metavar
                        | [ x ]           := topLevel.letting.expr           |] = addBinder ('&':name) x

        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.domain         |] = addBinder name x
        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.domain         |] = addBinder ('&':name) x

        helper x@[xMatch| [Prim (S name)] := topLevel.letting.name.reference |] = addBinder name x
        helper x@[xMatch| [Prim (S name)] := topLevel.letting.name.metavar   |] = addBinder ('&':name) x

        helper   [xMatch| _ := topLevel.suchThat  |] = return ()
        helper   [xMatch| _ := topLevel.objective |] = return ()
        helper   [xMatch| _ := topLevel.where     |] = return ()

        helper x@[xMatch| [Prim (S name)] := topLevel.declaration.dim  .name.reference |] = addBinder name x
        helper   [xMatch| _  := topLevel.declaration.nestedDimFind |] = return ()

        helper x@[xMatch| _ := topLevel |]
            = err ErrFatal $ "not handled in processStatement" <+> prettyAsPaths x

        helper   [xMatch| ls := withLocals.locals |] = mapM_ introduceStuff ls

        helper x@[xMatch| [Prim (S r)] := quantified.quanVar.structural.single.reference |]
            = addBinder r [xMake| quanVar.name   := [Prim (S r)]
                                | quanVar.within := [x]
                                |]

        helper _ = return ()

