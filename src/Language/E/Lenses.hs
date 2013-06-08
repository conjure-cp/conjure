{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Lenses where

-- a module hoping to replace most uses of xMatch and xMake

import Language.E.Definition

import Data.Text ( Text )


viewReference :: E -> Maybe Text
viewReference [xMatch| [Prim (S nm)] := reference |] = Just nm
viewReference _ = Nothing

mkReference :: Text -> E
mkReference nm = [xMake| reference := [Prim (S nm)] |]


viewMetaVar :: E -> Maybe Text
viewMetaVar [xMatch| [Prim (S nm)] := metavar |] = Just nm
viewMetaVar _ = Nothing

mkMetaVar :: Text -> E
mkMetaVar nm = [xMake| metavar := [Prim (S nm)] |]


