{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.NameResolution ( resolveNames ) where

import Conjure.Prelude
import Conjure.Language.Definition


resolveNames :: MonadFail m => Model -> m Model
resolveNames model = flip evalStateT [] $ do
    statements <- forM (mStatements model) $ \ st ->
        case st of
            Declaration decl -> do
                case decl of
                    FindOrGiven forg nm dom -> modify ((nm, DeclNoRepr forg nm dom) :)
                    Letting nm x            -> modify ((nm, Alias x) :)
                    _ -> return ()
                return st
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

