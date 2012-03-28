{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.EnumIdentifiers where

import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.State ( MonadState, get, modify, evalStateT )
import Data.Foldable ( forM_ )
import Data.Maybe ( mapMaybe )

import GenericOps.Core ( bottomUpM )
import ParsePrint ( pretty )
import PrintUtils ( Doc, nest, vcat )

import Language.Essence.Binding
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Spec
import Language.Essence.Type
import Language.Essence.Value



enumIdentifiers :: MonadError Doc m => Spec -> m Spec
enumIdentifiers spec = flip evalStateT [] $ do
    
    forM_ (topLevels spec) $ \ b -> case b of
        Left (LettingType _ t) -> modify (t:)
        _ -> return ()

    let
        chck :: Identifier -> Type -> Maybe Type
        chck i t@(TEnum (Just is)) | i `elem` is = Just t
        chck _ _ = Nothing

    let
        f :: (MonadError Doc m, MonadState [Type] m) => Expr -> m Expr
        f p@(EHole i) = do
            ts <- get
            let rs = mapMaybe (chck i) ts
            case rs of
                [ ] -> return p
                [_] -> return $ V $ VEnum i
                _   -> throwError $ vcat $ "Same enum value used in multiple enumerated types: "
                                         : map (nest 4 . pretty) rs
        f p = return p

    bottomUpM f spec
