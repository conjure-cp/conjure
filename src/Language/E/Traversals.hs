{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Traversals where

import Stuff.NamedLog ( nubKeepOrderBy )

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Pretty

import qualified Data.HashSet as S
import qualified Data.Text as T
import qualified Data.IntSet as IntSet ( insert, member )
import qualified Data.IntMap as IntMap ( insert, lookup )



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


bottomUpERefn
    :: MonadConjureList m
    => (E -> WriterT Any m E)
    -> E
    -> WriterT Any m E
bottomUpERefn func = withBindingScope . helper
    where
        helper [xMatch| [this] := statement.this
                      | [next] := statement.next
                      |] = do
            lift $ introduceStuff this
            this' <- func =<< bottomUpERefn func this
            lift $ introduceStuff this'
            next' <- func =<< bottomUpERefn func next
            return [xMake| statement.this := [this']
                         | statement.next := [next']
                         |]
        helper i = do
            lift $ introduceStuff i
            checkingMemo i $ \ t ->
                case t of
                    Tagged s xs -> do
                        xs' <- mapM helper xs
                        let t' = Tagged s xs'
                        lift $ introduceStuff t'
                        func t'
                    _           -> func t


checkingMemo
    :: MonadConjureList m
    => E
    -> (E -> WriterT Any m E)
    -> WriterT Any m E
checkingMemo x f = do
    bs <- lift $ gets (binders >>> nubKeepOrderBy binderName >>> sortOn binderName)
    let hashX = hash (x, bs) -- hash x
    memoSame    <- lift $ getsGlobal memoRefnStaysTheSame
    memoChanged <- lift $ getsGlobal memoRefnChanged
    case (IntSet.member hashX memoSame, IntMap.lookup hashX memoChanged) of
        (True, _  ) -> do
            -- lift $ mkLog "reuse-memo-same" $ pretty x
            return x
        (_, Just y) -> do
            -- lift $ mkLog "reuse-memo-diff" $ sep [pretty x, "~~>", pretty y]
            lift $ mkLog "from-cached" $ sep [pretty x, "~~>", pretty y]
            return y
        _ -> do
            (y, Any flag) <- listen $ withBindingScope $ f x
            if flag
                then do
                    -- lift $ mkLog "add-memo-diff" $ sep [pretty x, "~~>", pretty y]
                    lift $ modifyGlobal $ \ st ->
                        st { memoRefnChanged      = IntMap.insert hashX y memoChanged }
                    return y
                else do
                    -- lift $ mkLog "add-memo-same" $ pretty x
                    lift $ modifyGlobal $ \ st ->
                        st { memoRefnStaysTheSame = IntSet.insert hashX   memoSame    }
                    return x


foreachStatement
    :: forall t m
    .  ( MonadConjure m
       , MonadTrans t
       , Monad (t m)
       , Functor (t m)
       )
    => (E -> t m [E])
    -> Spec
    -> t m Spec
foreachStatement f (Spec v s) = Spec v . listAsStatement <$> helper s
    where
        helper :: E -> t m [E]
        helper [xMatch| [this] := statement.this
                      | [next] := statement.next
                      |] = do
            lift $ introduceStuff this
            this' <- f this
            lift $ mapM_ introduceStuff this'
            next' <- withBindingScope $ helper next
            return $ this' ++ next'
        helper this = f this

initialiseSpecState :: MonadConjure m => Spec -> m ()
initialiseSpecState (Spec _ statements) = do
    let names = [ nm
                | [xMatch| [Prim (S nm)] := reference |] <- universe statements
                , "v__" `T.isPrefixOf` nm
                ]
    modify $ \ st -> st { allNamesPreConjure = S.fromList names }

introduceStuff :: MonadConjure m => E -> m ()
introduceStuff = helper
    where

        helper x@[xMatch| [Prim (S name)] := topLevel.declaration.find .name.reference |] =
            let (base, _, _) = identifierSplit name in addReference base x

        helper x@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference |] =
            let (base, _, _) = identifierSplit name in addReference base x

        helper   [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
                        | xs            := topLevel.letting.typeEnum.values
                        |] = do
            let ty = [xMake| type.typeEnum := [Prim (S nm)] |]
            addReference nm ty
            forM_ xs $ \ y -> case y of
                [xMatch| [Prim (S nm')] := reference |] -> addReference nm' ty
                _ -> return ()

        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.expr           |] = addReference name x
        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.metavar
                        | [ x ]           := topLevel.letting.expr           |] = addMetaVar name x

        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.domain         |] = addReference name x
        helper   [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [ x ]           := topLevel.letting.domain         |] = addMetaVar name x

        helper x@[xMatch| [Prim (S name)] := topLevel.letting.name.reference |] = addReference name x
        helper x@[xMatch| [Prim (S name)] := topLevel.letting.name.metavar   |] = addMetaVar name x

        helper   [xMatch| _ := topLevel.suchThat  |] = return ()
        helper   [xMatch| _ := topLevel.objective |] = return ()
        helper   [xMatch| _ := topLevel.where     |] = return ()

        helper x@[xMatch| [Prim (S name)] := topLevel.declaration.dim  .name.reference |] = addReference name x
        helper   [xMatch| _  := topLevel.declaration.nestedDimFind |] = return ()

        helper x@[xMatch| _ := topLevel |]
            = err ErrFatal $ "not handled in processStatement" <+> prettyAsPaths x

        helper   [xMatch| ls := withLocals.locals |] = mapM_ introduceStuff ls

        helper x@[xMatch| [Prim (S r)] := quantified.quanVar.structural.single.reference |]
            = addReference r [xMake| quanVar.name   := [Prim (S r)]
                                   | quanVar.within := [x]
                                   |]

        helper _ = return ()

