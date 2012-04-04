{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.QuantifierDecl where

import Control.Applicative
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind, inScope, mkG )
import ParsecUtils ( braces, reserved )
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>) )
import qualified PrintUtils as Pr
import Utils ( allEq )

import Language.Essence.Lambda
import {-# SOURCE #-} Language.Essence.Expr
import Language.Essence.Type



data QuantifierDecl = QuantifierDecl Lambda Lambda Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag QuantifierDecl

instance Hole QuantifierDecl

instance GPlate QuantifierDecl -- everything is a leaf!

instance MatchBind QuantifierDecl

instance ParsePrint QuantifierDecl where
    parse = do
        reserved "quantifier"
        braces $ QuantifierDecl
            <$> (reserved "append"   >> parse)
            <*> (reserved "guard"    >> parse)
            <*> (reserved "identity" >> parse)
    pretty (QuantifierDecl app gua ide) =
        "quantifier" Pr.$$ Pr.braces (
            Pr.nest 4 ("append  " <+> pretty app) Pr.$$
            Pr.nest 4 ("guard   " <+> pretty gua) Pr.$$
            Pr.nest 4 ("identity" <+> pretty ide)
            )

instance TypeOf QuantifierDecl where
    typeOf p@(QuantifierDecl glueOp guardOp iden) = inScope (mkG p) $ do
        tGlueOp  <- typeOf glueOp
        tGuardOp <- typeOf guardOp
        tIden    <- typeOf iden
        case tGlueOp of
            TLambda [a,a'] a'' | allEq [tIden,a,a',a''] -> return ()
            _ -> inScope (mkG glueOp ) $ typeError "Type error in Quantifier declaration."
        case tGuardOp of
            TLambda [b,a] a' | b == TBool && allEq [tIden,a,a'] -> return ()
            _ -> inScope (mkG guardOp) $ typeError "Type error in Quantifier declaration."
        return tIden

