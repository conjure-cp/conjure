{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Conjure.Language.NameResolution
    ( resolveNames
    , resolveNamesX
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


resolveNames :: (MonadLog m, MonadFail m) => Model -> m Model
resolveNames model = flip evalStateT [] $ do
    statements <- forM (mStatements model) $ \ st ->
        case st of
            Declaration decl ->
                case decl of
                    FindOrGiven forg nm dom       -> do
                        dom' <- resolveD dom
                        modify ((nm, DeclNoRepr forg nm dom') :)
                        return (Declaration (FindOrGiven forg nm dom'))
                    Letting nm x                  -> do
                        x' <- resolveX x
                        modify ((nm, Alias x') :)
                        return (Declaration (Letting nm x'))
                    _ -> fail ("Unexpected declaration:" <+> pretty st)
            SearchOrder{} -> return st
            Where xs -> Where <$> mapM resolveX xs
            Objective obj x -> Objective obj <$> resolveX x
            SuchThat xs -> SuchThat <$> mapM resolveX xs
    duplicateNames <- gets (map fst >>> histogram >>> filter (\ (_,n) -> n > 1 ) >>> map fst)
    unless (null duplicateNames) $
        userErr ("Some names are defined multiple times:" <+> prettyList id "," duplicateNames)
    return model { mStatements = statements }


resolveNamesX :: MonadFail m => Expression -> m Expression
resolveNamesX x = evalStateT (resolveX x) []


resolveX
    :: (MonadFail m, MonadState [(Name, ReferenceTo)] m)
    => Expression
    -> m Expression

resolveX (Reference nm Nothing) = do
    ctxt <- gets id
    mval <- gets (lookup nm)
    case mval of
        Nothing -> fail $ vcat $ ("Undefined reference:" <+> pretty nm)
                               : ("Bindings in context:" : prettyContext ctxt)
        Just r  -> return (Reference nm (Just r))

resolveX p@(Reference nm Just{}) = do                   -- this is for re-resolving
    mval <- gets (lookup nm)
    case mval of
        Nothing -> return p                             -- hence, do not fail if not in the context
        Just r  -> return (Reference nm (Just r))

resolveX (Domain x) = Domain <$> resolveD x
resolveX (Comprehension x is) = scope $ do
    is' <- forM is $ \ i -> case i of
        Generator gen -> do
            gen' <- case gen of
                GenDomain       pat dom  -> GenDomain       pat <$> resolveD dom
                GenInExpr       pat expr -> GenInExpr       pat <$> resolveX expr
            forM_ (universeBi (generatorPat gen)) $ \ nm ->
                modify ((nm, InComprehension gen') :)
            return (Generator gen')
        Filter y -> Filter <$> resolveX y
    x' <- resolveX x
    return (Comprehension x' is')
resolveX x = descendM resolveX x


resolveD
    :: (Functor m, MonadState [(Name, ReferenceTo)] m)
    => Domain () Expression
    -> m (Domain () Expression)
resolveD (DomainReference _ (Just d)) = resolveD d
resolveD (DomainReference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr ("Undefined reference to a domain:" <+> pretty nm)
        Just (Alias (Domain r)) -> return r
        Just x -> userErr ("Expected a domain, but got an expression:" <+> pretty x)
resolveD d = descendM resolveD d


scope :: MonadState st m => m a -> m a
scope ma = do
    st <- gets id
    a <- ma
    modify (const st)
    return a

