{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.NameResolution ( resolveNames ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain


resolveNames :: (Functor m, Monad m) => Model -> m Model
resolveNames model = flip evalStateT [] $ do
    statements <- forM (mStatements model) $ \ st' -> do
        st <- transformBiM insert st'
        case st of
            Declaration decl -> do
                case decl of
                    FindOrGiven forg nm dom       -> modify ((nm, RefTo (DeclNoRepr forg nm dom)) :)
                    Letting nm x                  -> modify ((nm, RefTo (Alias x)) :)
                    GivenDomainDefnEnum nm        -> modify ((nm, EnumTypeDefGiven) :)
                    LettingDomainDefnEnum nm vals -> do
                        modify ((nm, EnumTypeDefLetting vals) :)
                        forM_ vals $ \ val -> do
                            let c = Constant (ConstantEnum nm vals val)
                            modify ((val, EnumConstant c) :)
                    _ -> return ()
                return st
            SearchOrder{} -> return st
            Where xs -> Where <$> mapM (transformM insert) xs
            Objective obj x -> Objective obj <$> transformM insert x
            SuchThat xs -> SuchThat <$> mapM (transformM insert) xs
    model { mStatements = statements }
        |> transformBiM removeDomainHack


data ToLookUp
    = RefTo ReferenceTo
    | EnumConstant Expression
    | EnumTypeDefGiven
    | EnumTypeDefLetting [Name]
    deriving Show


insert :: MonadState [(Name, ToLookUp)] m => Expression -> m Expression
insert (Reference nm Nothing) = do
    mval <- gets (lookup nm)
    return $ case mval of
        Nothing                        -> Reference nm Nothing
        Just (RefTo r)                 -> Reference nm (Just r)
        Just (EnumConstant c)          -> c
        Just EnumTypeDefGiven          -> Domain (DomainEnum nm Nothing)
        Just (EnumTypeDefLetting vals) -> Domain (DomainEnum nm (Just (vals, [])))
insert (Lambda pat@(Single nm _) body) = do
    modify ((nm, RefTo (InLambda pat)) :)
    body' <- insert body
    modify tail
    return (Lambda pat body')
insert x = return x


removeDomainHack :: MonadState [(Name, ToLookUp)] m => Domain () Expression -> m (Domain () Expression)
removeDomainHack (DomainHack (Domain d)) = removeDomainHack d
removeDomainHack d@(DomainEnum nm (Just ([], rs))) = do
    mval <- gets (lookup nm)
    return $ case mval of
        Just (EnumTypeDefLetting vals) -> DomainEnum nm (Just (vals, rs))
        _ -> d
removeDomainHack d = return d

