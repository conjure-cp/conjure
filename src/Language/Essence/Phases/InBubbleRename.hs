{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.InBubbleRename where

import Control.Applicative
import Control.Monad ( forM )
import Control.Monad.State ( MonadState, get, put )
import Data.Maybe
import qualified Data.Map as M

import Has
import Constants ( FreshName, getFreshName, isFreshName )
import GenericOps.Core ( GPlate, topDownM, bottomUp )

import Language.Essence.Binding
import Language.Essence.Expr
import Language.Essence.Identifier



inBubbleRename :: forall a f m st .
    ( GPlate a
    , Applicative m
    , Has st [FreshName]
    , MonadState st m
    ) => a -> m a
inBubbleRename = topDownM worker
    where
        supply :: String -> m (Maybe String)
        supply old = do
            if isFreshName old
                then return Nothing
                else Just <$> getFreshName

        worker :: Expr -> m Expr
        worker p@(Bubble a b bs) = do

            mlu <- forM bs $ \ i -> case i of
                Left (Find (Identifier old) _) -> do
                    mnew <- supply old
                    return $ case mnew of
                        Nothing -> Nothing
                        Just new -> Just $ M.singleton old new
                _ -> return Nothing

            let lu = M.unions $ catMaybes mlu

            let f i@(Identifier nm) = case M.lookup nm lu of
                                        Nothing  -> i
                                        Just new -> Identifier new

            return $ if M.null lu
                        then p
                        else Bubble (bottomUp f a)
                                    (bottomUp f b)
                                    (map (bottomUp f) bs)
        worker p = return p
