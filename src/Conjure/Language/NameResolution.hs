{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Conjure.Language.NameResolution ( resolveNames ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


resolveNames :: (MonadFail m, MonadLog m) => Model -> m Model
resolveNames model = flip evalStateT [] $ do
    statements <- forM (mStatements model) $ \ st' -> do
        st <- insert st'
        case st of
            Declaration decl -> do
                case decl of
                    FindOrGiven forg nm dom       -> modify ((nm, RefTo (DeclNoRepr forg nm dom)) :)
                    Letting nm x                  -> modify ((nm, RefTo (Alias x)) :)
                    _ -> return ()
                return st
            SearchOrder{} -> return st
            Where xs -> Where <$> mapM insert xs
            Objective obj x -> Objective obj <$> insert x
            SuchThat xs -> SuchThat <$> mapM insert xs
    duplicateNames <- gets (map fst >>> histogram >>> filter (\ (_,n) -> n > 1 ) >>> map fst)
    unless (null duplicateNames) $
        userErr ("Some names are defined multiple times:" <+> prettyList id "," duplicateNames)
    return model { mStatements = statements }


data ToLookUp
    = RefTo ReferenceTo
    deriving Show


insertX
    :: MonadState [(Name, ToLookUp)] m
    => Expression
    -> m Expression
insertX (Reference nm Nothing) = do
    mval <- gets (lookup nm)
    return $ case mval of
        Nothing                        -> Reference nm Nothing
        Just (RefTo r)                 -> Reference nm (Just r)
insertX (Lambda pat@(Single nm _) body) = do
    modify ((nm, RefTo (InLambda pat)) :)
    body' <- insertX body
    modify tail
    return (Lambda pat body')
insertX x = return x


insertD
    :: MonadState [(Name, ToLookUp)] m
    => Domain () Expression
    -> m (Domain () Expression)
insertD (DomainReference _ (Just d)) = insertD d
insertD (DomainReference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr ("Undefined reference to a domain:" <+> pretty nm)
        Just (RefTo (Alias (Domain r))) -> return r
        Just (RefTo x) -> userErr ("Expected a domain, but got an expression:" <+> pretty x)
insertD d = return d


insert
    :: (MonadState [(Name, ToLookUp)] m, Data a)
    => a
    -> m a
insert = transformBiM insertX >=> transformBiM insertD

