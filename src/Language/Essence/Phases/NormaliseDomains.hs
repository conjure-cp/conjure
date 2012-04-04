{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.NormaliseDomains ( normaliseDomains ) where

import Control.Applicative
import Control.Monad ( (>=>) )
import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.State ( MonadState, State, runState, runStateT )
import Data.Default ( def )
import Data.Maybe ( catMaybes )
import Data.Traversable ( forM )
import qualified Control.Monad.State as S

import Has
import PrintUtils ( Doc, (<+>) )
import ParsePrint ( pretty )

import Language.Essence



normaliseDomains ::
    ( Applicative m
    , MonadError Doc m
    ) => Spec -> m Spec
normaliseDomains spec = do

    (topLevels', newWheres) <-
        flip runStateT (def :: [Where]) $
            forM (topLevels spec) $ \ tl -> case tl of
                Left (Find  i d) -> Left . Find  i <$> (multipleValuesCheck >=> normaliseDomain i) d
                Left (Given i d) -> Left . Given i <$> (multipleValuesCheck >=> normaliseDomain i) d
                _ -> return tl

    return spec { topLevels = topLevels' ++ map Right (reverse newWheres) }


multipleValuesCheck :: MonadError Doc m => Domain -> m Domain
multipleValuesCheck dom@(AnyDom de es (DomainAttrs attrs)) = do
    attrs' <- flip S.evalStateT [] $ forM attrs $ \ attr -> case attr of
        OnlyName ae -> do
            aes <- S.get
            if ae `elem` aes
                then return (Just attr)
                else S.modify (ae :) >> return (Just attr)
        NameValue ae _ -> do
            aes <- S.get
            if ae `elem` aes
                then throwError $ "Domain has multiple values for an attribute:" <+> pretty dom
                else S.modify (ae :) >> return (Just attr)
        DontCare -> return Nothing
    return $ AnyDom de es $ DomainAttrs $ catMaybes attrs' ++ [ DontCare | DontCare `elem` attrs ]
multipleValuesCheck dom = return dom


normaliseDomain ::
    ( Has st [Where]
    , MonadState st m
    , MonadError Doc m
    ) => Identifier -> Domain -> m Domain
normaliseDomain _ pDom@(AnyDom TSet [e] (DomainAttrs attrs)) =
    case [ s | NameValue AttrSize s <- attrs ] of

        -- set has size attribute set.
        -- remove minSize and maxSize
        -- create where statements for those.
        [s] -> do
            let ((minS, maxS), attrs') = runState ( (,) <$> getAttrVal AttrMinSize
                                                        <*> getAttrVal AttrMaxSize
                                                  ) (attrs :: [DomainAttr])
            case minS of
                Nothing -> return ()
                Just x  -> modifyM ( Where (EOp Leq [ x , s ]) : )
            case maxS of
                Nothing -> return ()
                Just x  -> modifyM ( Where (EOp Geq [ x , s ]) : )
            return $ AnyDom TSet [e] (DomainAttrs attrs')
        _ -> return pDom
normaliseDomain _ d = return d


-- given an attribute enum for a name-value kind of attribute,
-- return the value of that attribute and the rest of the attributes.
getAttrVal :: DomainAttrEnum -> State [DomainAttr] (Maybe Expr)
getAttrVal ae = do
    st <- S.get
    S.put []
    mresult <- forM st $ \ t -> case t of
                                    NameValue ae' x | ae == ae' -> return (Just x)
                                    attr                        -> S.modify (attr:) >> return Nothing
    case catMaybes mresult of
        [found] -> S.modify reverse >> return (Just found)
        _       -> S.put st         >> return Nothing
