{-# LANGUAGE FlexibleContexts #-}

module Language.Essence.Phases.NormaliseDomains ( normaliseDomains ) where

import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState, State, runState, runStateT )
import Data.Default ( def )
import Data.Maybe ( catMaybes )
import Data.Traversable ( forM )
import qualified Control.Monad.State as S

import Has
import PrintUtils ( Doc )

import Language.Essence



normaliseDomains ::
    ( Applicative m
    , MonadError Doc m
    ) => Spec -> m Spec
normaliseDomains spec = do

    (topLevels', newWheres) <-
        flip runStateT (def :: [Where]) $
            forM (topLevels spec) $ \ tl -> case tl of
                Left (Find  i d) -> Left . Find  i <$> normaliseDomain i d
                Left (Given i d) -> Left . Given i <$> normaliseDomain i d
                _ -> return tl

    return spec { topLevels = topLevels' ++ map Right (reverse newWheres) }



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
