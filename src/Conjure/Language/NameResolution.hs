{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Conjure.Language.NameResolution ( resolveNames ) where

import Conjure.Prelude
import Conjure.Language.Definition


resolveNames :: (MonadFail m, MonadLog m) => Model -> m Model
resolveNames model = flip evalStateT [] $ do
    statements <- forM (mStatements model) $ \ st' -> do
        st <- transformBiM insert st'
        case st of
            Declaration decl -> do
                case decl of
                    FindOrGiven forg nm dom       -> modify ((nm, RefTo (DeclNoRepr forg nm dom)) :)
                    Letting nm x                  -> modify ((nm, RefTo (Alias x)) :)
                    _ -> return ()
                return st
            SearchOrder{} -> return st
            Where xs -> Where <$> mapM (transformM insert) xs
            Objective obj x -> Objective obj <$> transformM insert x
            SuchThat xs -> SuchThat <$> mapM (transformM insert) xs
    return model { mStatements = statements }


data ToLookUp
    = RefTo ReferenceTo
    deriving Show


insert :: MonadState [(Name, ToLookUp)] m => Expression -> m Expression
insert (Reference nm Nothing) = do
    mval <- gets (lookup nm)
    return $ case mval of
        Nothing                        -> Reference nm Nothing
        Just (RefTo r)                 -> Reference nm (Just r)
insert (Lambda pat@(Single nm _) body) = do
    modify ((nm, RefTo (InLambda pat)) :)
    body' <- insert body
    modify tail
    return (Lambda pat body')
insert x = return x
