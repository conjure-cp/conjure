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


resolveNames :: (MonadLog m, MonadFail m) => Model -> m Model
resolveNames model = flip evalStateT [] $ do
    statements <- forM (mStatements model) $ \ st -> do
        case st of
            Declaration decl -> do
                case decl of
                    FindOrGiven forg nm dom       -> do
                        dom' <- resolveD dom
                        modify ((nm, RefTo (DeclNoRepr forg nm dom')) :)
                        return (Declaration (FindOrGiven forg nm dom'))
                    Letting nm x                  -> do
                        x' <- resolveX x
                        modify ((nm, RefTo (Alias x')) :)
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


data ToLookUp
    = RefTo ReferenceTo
    deriving Show


resolveX
    :: (MonadFail m, MonadState [(Name, ToLookUp)] m)
    => Expression
    -> m Expression
resolveX (Reference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Nothing        -> fail ("Undefined reference:" <+> pretty nm)
        Just (RefTo r) -> return (Reference nm (Just r))
resolveX (viewLambda -> Just (Lambda pat body, over, reconstruct, calculateType))
    | patternNeedsType pat = scope $ do
    over' <- resolveX over
    mty   <- runExceptT (calculateType over')
    case mty of
        Left e -> bug ("calculateType:" <+> e <++> vcat [pretty over, pretty over'])
        Right ty -> scope $ do
            outPat <- giveTypeToPat ty pat
            body'  <- resolveX body
            let l = Lambda outPat body'
            return (reconstruct l over')
resolveX (Domain x) = Domain <$> resolveD x
resolveX x = descendM resolveX x


resolveD
    :: (Functor m, MonadState [(Name, ToLookUp)] m)
    => Domain () Expression
    -> m (Domain () Expression)
resolveD (DomainReference _ (Just d)) = resolveD d
resolveD (DomainReference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr ("Undefined reference to a domain:" <+> pretty nm)
        Just (RefTo (Alias (Domain r))) -> return r
        Just (RefTo x) -> userErr ("Expected a domain, but got an expression:" <+> pretty x)
resolveD d = descendM resolveD d


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


scope :: MonadState st m => m a -> m a
scope ma = do
    st <- gets id
    a <- ma
    modify (const st)
    return a

