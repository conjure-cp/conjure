{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Phases.EnumIdentifiers where

import Control.Monad.Error ( MonadError, throwError )
import Data.Maybe ( mapMaybe )

import GenericOps.Core ( bottomUp, bottomUpM )
import ParsePrint ( pretty )
import PrintUtils ( Doc, nest, vcat )

import Language.Essence.Binding
import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Range
import Language.Essence.Spec
import Language.Essence.Type
import Language.Essence.Value



-- an identifier in a spec is always parsed as "EHole (Identifier nm)"
-- when it refers to an element of an enum type it should be: "V (VEnum nm)"
-- see: enumIdentifiersExpr

-- a domain identifier is always parsed as "DHole (Identifier nm)"
-- when it refers to an enum type, it should be: "DEnum (Identifier nm) RAll"
-- see: enumIdentifiersDom

enumIdentifiers :: MonadError Doc m => Spec -> m Spec
enumIdentifiers = enumIdentifiersExpr . enumIdentifiersDom


enumIdentifiersExpr :: MonadError Doc m => Spec -> m Spec
enumIdentifiersExpr spec = bottomUpM f spec
    where
        bindings :: [Type]
        bindings = flip mapMaybe (topLevels spec) $ \ b -> case b of
                    Left (LettingType _ t) -> Just t
                    _ -> Nothing

        chck :: Identifier -> Type -> Maybe Type
        chck i t@(TEnum (Just is)) | i `elem` is = Just t
        chck _ _ = Nothing

        f :: MonadError Doc m => Expr -> m Expr
        f p@(EHole i) = do
            let rs = mapMaybe (chck i) bindings
            case rs of
                [ ] -> return p
                [_] -> return $ V $ VEnum i
                _   -> throwError $ vcat $ "Same enum value used in multiple enumerated types: "
                                         : map (nest 4 . pretty) rs
        f p = return p



enumIdentifiersDom :: Spec -> Spec
enumIdentifiersDom spec = bottomUp f spec
    where
        bindings :: [String]
        bindings = flip mapMaybe (topLevels spec) $ \ b -> case b of
                    Left (LettingType (Identifier nm) _) -> Just nm
                    _ -> Nothing

        f :: Domain -> Domain
        f p@(DHole i@(Identifier nm)) = if nm `elem` bindings
                                            then DEnum i RAll
                                            else p
        f p = p


