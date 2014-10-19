{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.NameResolution where

import Conjure.Prelude
import Conjure.Language.Definition


resolveNames :: MonadFail m => Model -> m Model
resolveNames model = flip evalStateT [] $ do
    statements <- forM (mStatements model) $ \ st ->
        case st of
            Declaration decl ->
                case decl of
                    FindOrGiven t nm dom -> do
                        modify ((nm, DeclNoRepr t nm dom) :)
                        return st
                    Letting nm x -> do
                        modify ((nm, Alias x) :)
                        return st
                    _ -> return st
            SearchOrder{} -> return st
            Where xs -> Where <$> mapM (transformM insert) xs
            Objective obj x -> Objective obj <$> transformM insert x
            SuchThat xs -> SuchThat <$> mapM (transformM insert) xs
    return model { mStatements = statements }


insert :: (MonadFail m, MonadState [(Name, ReferenceTo)] m) => Expression -> m Expression
insert (Reference nm Nothing) = do
    refTo <- gets (lookup nm)
    return (Reference nm refTo)
insert (Lambda pat@(Single nm _) body) = do
    modify ((nm, InLambda pat) :)
    body' <- insert body
    modify tail
    return (Lambda pat body')
insert x = return x

