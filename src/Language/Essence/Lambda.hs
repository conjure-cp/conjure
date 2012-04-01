{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Lambda where

import Control.Applicative
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty, prettyListDoc )
import PrintUtils ( (<+>), (<>), text )
import qualified PrintUtils as Pr

import Language.Essence.Type
import {-# SOURCE #-} Language.Essence.Expr



data Lambda = Lambda [(String, Type)] Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Lambda

instance Hole Lambda

instance GPlate Lambda -- everything is a leaf!

instance MatchBind Lambda

instance ParsePrint Lambda where
    parse = braces $ do
        args <- sepBy1 ((,) <$> identifier <*> (colon *> parse)) comma
        reservedOp "-->"
        x <- parse
        return $ Lambda args x
    pretty (Lambda args x) = Pr.braces (prettyListDoc id Pr.comma argsDoc <+> "-->" <+> pretty x)
        where argsDoc = map (\ (i,t) -> text i <> Pr.colon <+> pretty t ) args

instance TypeOf Lambda where
    typeOf (Lambda xs x) = TLambda (map snd xs) <$> typeOf x
