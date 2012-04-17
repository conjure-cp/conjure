{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Objective where

import Control.Applicative
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole, GPlate, gplate, gplateSingle, MatchBind )
import Language.EssenceLexer
import Language.EssenceLexerP
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>) )

import Language.Essence.Expr



data Objective = OMin { objExpr :: Expr }
               | OMax { objExpr :: Expr }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Objective

instance Hole Objective

instance GPlate Objective where
    gplate (OMin x) = gplateSingle OMin x
    gplate (OMax x) = gplateSingle OMax x

instance MatchBind Objective

instance ParsePrint Objective where
    parse = msum1 [ OMin <$> (lexeme L_minimising *> parse)
                  , OMax <$> (lexeme L_maximising *> parse)
                  ]
    pretty (OMin x) = "minimising" <+> pretty x
    pretty (OMax x) = "maximising" <+> pretty x
