{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Phases.PostParse where

import Control.Applicative
import Control.Monad ( (>=>) )
import Control.Monad.Error ( MonadError, throwError )
import Data.Maybe ( mapMaybe )

import GenericOps.Core ( bottomUp, bottomUpM )
import ParsePrint ( pretty )
import PrintUtils ( Doc, nest, vcat )

import Language.Essence
import Language.Essence.Phases.NormaliseDomains ( normaliseDomains )



-- [enumIdentifiers]
-- an identifier in a spec is always parsed as "EHole (Identifier nm)"
-- when it refers to an element of an enum type it should be: "V (VEnum nm)"

-- [enumDomains]
-- a domain identifier is always parsed as "DHole (Identifier nm)"
-- when it refers to an enum type, it should be: "DEnum (Identifier nm) RAll"
-- see: enumIdentifiersDom

postParse :: forall m . (Applicative m, MonadError Doc m) => Spec -> m Spec
postParse spec = (normaliseDomains >=> bottomUpM enumIdentifiers) (bottomUp enumDomains spec)
    where

        typeBindings :: ([String],[Type])
        typeBindings = unzip $ flip mapMaybe (topLevels spec) $ \ b ->
            case b of
                Left (LettingType (Identifier i) t) -> Just (i,t)
                Left (GivenType   (Identifier i) t) -> Just (i,t)
                _ -> Nothing

        enumIdentifiers :: Expr -> m Expr
        enumIdentifiers p@(EHole i) = do
            let
                check :: Identifier -> Type -> Maybe Type
                check j t@(TEnum (Just js)) | j `elem` js = Just t
                check _ _ = Nothing

                rs = mapMaybe (check i) (snd typeBindings)
            case rs of
                [ ] -> return p
                [_] -> return $ V $ VEnum i
                _   -> throwError $ vcat $ "Same enum value used in multiple enumerated types: "
                                         : map (nest 4 . pretty) rs
        enumIdentifiers p = return p

        enumDomains :: Domain -> Domain
        enumDomains p@(DHole i@(Identifier nm)) =
            if nm `elem` fst typeBindings
                then DEnum i RAll
                else p
        enumDomains p = p
