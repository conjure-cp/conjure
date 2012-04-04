{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Lambda where

import Control.Applicative
import Data.Foldable ( forM_ )
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified Control.Monad.State as S

import GenericOps.Core ( NodeTag
                       , Hole
                       , GPlate, fromGs, mkG, gplate, gplateError
                       , MatchBind, addBinding )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty, prettyListDoc )
import PrintUtils ( (<+>), (<>) )
import qualified PrintUtils as Pr

import Language.Essence.Type
import {-# SOURCE #-} Language.Essence.Expr
import {-# SOURCE #-} Language.Essence.Identifier



data Lambda = Lambda [(Identifier, Type)] Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Lambda

instance Hole Lambda

instance GPlate Lambda where
    gplate (Lambda is j) =
        ( mkG j : map (mkG . fst) is ++ map (mkG . snd) is
        , \ xs -> let j'   = fromGs $ take 1 xs
                      len  = length is
                      is1' = fromGs $ take len $ drop 1 xs
                      is2' = fromGs $ take len $ drop len $ drop 1 xs
                  in  if length j' == 1 &&
                         length is1' == len &&
                         length is2' == len
                         then Lambda (zip is1' is2') (head j')
                         else gplateError "Lambda"
        )

instance MatchBind Lambda

instance ParsePrint Lambda where
    parse = braces $ do
        args <- sepBy1 ((,) <$> parse <*> (colon *> parse)) comma
        reservedOp "-->"
        x <- parse
        return $ Lambda args x
    pretty (Lambda args x) = Pr.braces (prettyListDoc id Pr.comma argsDoc <+> "-->" <+> pretty x)
        where argsDoc = map (\ (i,t) -> pretty i <> Pr.colon <+> pretty t ) args

instance TypeOf Lambda where
    typeOf (Lambda xs x) = do
        st  <- S.get
        forM_ xs $ \ (Identifier i,t) -> addBinding i t
        res <- TLambda (map snd xs) <$> typeOf x
        S.put st
        return res
