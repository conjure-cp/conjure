{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Language.NameResolution ( resolveNames ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Ops
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf


resolveNames :: (Functor m, MonadLog m) => Model -> m Model
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
    check statements
    return model { mStatements = statements }


data ToLookUp
    = RefTo ReferenceTo
    deriving Show


insertX
    :: (Functor m, MonadState [(Name, ToLookUp)] m)
    => Expression
    -> m Expression
insertX (Reference nm Nothing) = do
    mval <- gets (lookup nm)
    return $ case mval of
        Nothing                        -> Reference nm Nothing
        Just (RefTo r)                 -> Reference nm (Just r)

insertX (viewLambda -> Just (Lambda pat body, over, reconstruct, calculateType))
    | patternNeedsType pat = do
    over' <- insert over
    mty   <- runExceptT (calculateType over')
    case mty of
        Left e -> bug e
        Right ty -> do
            outPat <- giveTypeToPat ty pat
            body'  <- insert body
            let l = Lambda outPat body'
            return (reconstruct l over')

insertX x = return x


insertD
    :: (Functor m, MonadState [(Name, ToLookUp)] m)
    => Domain () Expression
    -> m (Domain () Expression)
insertD (DomainReference _ (Just d)) = insert d
insertD (DomainReference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr ("Undefined reference to a domain:" <+> pretty nm)
        Just (RefTo (Alias (Domain r))) -> return r
        Just (RefTo x) -> userErr ("Expected a domain, but got an expression:" <+> pretty x)
insertD d = return d


insert
    :: (Functor m, MonadState [(Name, ToLookUp)] m, Data a)
    => a
    -> m a
insert = transformBiM insertX >=> transformBiM insertD


patternNeedsType :: AbstractPattern -> Bool
patternNeedsType (Single _ Nothing) = True
patternNeedsType (Single _ Just{} ) = False
patternNeedsType (AbsPatTuple  ts ) = any patternNeedsType ts
patternNeedsType (AbsPatMatrix ts ) = any patternNeedsType ts
patternNeedsType (AbsPatSet    ts ) = any patternNeedsType ts
patternNeedsType pat = bug ("patternNeedsType:" <+> pretty (show pat))


giveTypeToPat
    :: (Functor m, MonadState [(Name, ToLookUp)] m)
    => Type
    -> AbstractPattern
    -> m AbstractPattern
giveTypeToPat ty (Single nm _) = do
    let pat = Single nm (Just ty)
    modify ((nm, RefTo (InLambda pat)) :)
    return pat
giveTypeToPat (TypeTuple tys) (AbsPatTuple ts) = AbsPatTuple <$> zipWithM giveTypeToPat tys ts
giveTypeToPat (TypeMatrix _ ty) (AbsPatMatrix ts) = AbsPatMatrix <$> zipWithM giveTypeToPat (repeat ty) ts
giveTypeToPat ty pat = bug ("giveTypeToPat:" <++> vcat [pretty (show ty), pretty (show pat)])


viewLambda
    :: (MonadFail m, TypeOf a)
    => Expression
    -> Maybe ( Expression
             , Expression
             , Expression -> Expression -> Expression
             , a -> m Type
             )
viewLambda (        Op (MkOpMapOverDomain  (OpMapOverDomain    l@Lambda{} over))) =
    Just ( l
         , over
         , \ a b -> Op (MkOpMapOverDomain   (OpMapOverDomain   a b))
         , typeOf
         )
viewLambda (        Op (MkOpMapInExpr      (OpMapInExpr        l@Lambda{} over))) =
    Just ( l
         , over
         , \ a b -> Op (MkOpMapInExpr       (OpMapInExpr       a b))
         , typeOf >=> innerTypeOf
         )
viewLambda (        Op (MkOpMapSubsetExpr   (OpMapSubsetExpr   l@Lambda{} over))) =
    Just ( l
         , over
         , \ a b -> Op (MkOpMapSubsetExpr   (OpMapSubsetExpr   a b))
         , typeOf
         )
viewLambda (        Op (MkOpMapSubsetEqExpr (OpMapSubsetEqExpr l@Lambda{} over))) =
    Just ( l
         , over
         , \ a b -> Op (MkOpMapSubsetEqExpr (OpMapSubsetEqExpr a b))
         , typeOf
         )
viewLambda _ = Nothing


check :: (Monad m, Data a) => a -> m ()
check = mapM_ f . universeBi
    where
        f (Reference nm Nothing) = userErr ("Undefined reference:" <+> pretty nm)
        f _ = return ()

