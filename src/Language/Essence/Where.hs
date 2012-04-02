{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Where where

import Control.Applicative
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole, GPlate, gplate, gplateSingle, MatchBind )
import ParsecUtils ( (<?>), comma, many1, reserved, sepBy1 )
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>) )
import qualified PrintUtils as Pr

import {-# SOURCE #-} Language.Essence.Expr



newtype Where = Where { whereExpr :: Expr }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Where

instance Hole Where

instance GPlate Where where
    gplate (Where x) = gplateSingle Where x

instance MatchBind Where

instance ParsePrint Where where
    parse = error "do not use this one directly. use it via (parse :: [Where])"
    pretty (Where x) = "where" <+> pretty x

instance ParsePrint [Where] where
    parse = do
        let one = do reserved "where"
                     map Where <$> parse `sepBy1` comma
        concat <$> many1 one
        <?> "where statement"
    pretty = Pr.vcat . map pretty
