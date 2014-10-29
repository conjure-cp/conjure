{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Conjure.Language.NameResolution ( resolveNames ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


resolveNames :: (MonadFail m, MonadLogger m) => Model -> Model -> m Model
resolveNames defn model = do
    logDebug $ "resolveNames, before:" <++> pretty (vcat $ map (pretty . show) (mStatements model))
    model' <- resolveNamesInternal defn model
    logDebug $ "resolveNames, after: " <++> pretty (vcat $ map (pretty . show) (mStatements model'))
    return model'

-- resolveNamesInternal :: MonadFail m => Model -> Model -> m Model
resolveNamesInternal defn model = flip evalStateT [] $ do
    let allStatements = map (False,) (mStatements defn)
                     ++ map (True ,) (mStatements model)
    statements <- forM allStatements $ \ (keep,st') -> do
        stOut <- do
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
                                logDebug $ "resolveNames: LettingDomainDefnEnum" <+> pretty (val, c)
                                modify ((val, EnumConstant c) :)
                        _ -> return ()
                    return st
                SearchOrder{} -> return st
                Where xs -> Where <$> mapM (transformM insert) xs
                Objective obj x -> Objective obj <$> transformM insert x
                SuchThat xs -> SuchThat <$> mapM (transformM insert) xs
        if keep
            then return (Just stOut)
            else return Nothing
    model { mStatements = catMaybes statements }
        |> transformBiM massageDomains


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


massageDomains :: (MonadFail m, MonadState [(Name, ToLookUp)] m) => Domain () Expression -> m (Domain () Expression)
massageDomains (DomainReference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Just EnumTypeDefGiven          -> return $ DomainEnum nm Nothing
        Just (EnumTypeDefLetting vals) -> return $ DomainEnum nm (Just (vals, []))
        _ -> fail ("Unbound domain reference:" <+> pretty nm)
massageDomains d@(DomainEnum nm (Just ([], rs))) = do
    mval <- gets (lookup nm)
    return $ case mval of
        Just (EnumTypeDefLetting vals) -> DomainEnum nm (Just (vals, rs))
        _ -> d
massageDomains d = return d

