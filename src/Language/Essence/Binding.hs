{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Binding where

import Control.Applicative
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter )
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import Constants
import Nested ( Nested )
import Has
import GenericOps.Core ( NodeTag
                       , Hole
                       , GPlate, gplate, gplateTwo, gplateSingle
                       , GNode
                       , MatchBind, BindingsMap, addBinding, getBinding )
import Language.EssenceLexer
import Language.EssenceLexerP
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), (<>), Doc)
import qualified PrintUtils as Pr

import Language.Essence.Domain
import {-# SOURCE #-} Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Lambda
import Language.Essence.QuantifierDecl
import {-# SOURCE #-} Language.Essence.QuantifiedExpr
import Language.Essence.Type
import Language.Essence.Where


isOrderedType ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [GNode]
    , MonadError (Nested Doc) m
    , MonadState st m
    , MonadWriter [Doc] m
    ) => Type -> m Bool
isOrderedType TBool {}  = return True
isOrderedType TInt  {}  = return True
isOrderedType TEnum {}  = return True
isOrderedType (THole (Identifier i)) = do
    b <- getBinding i
    case b of
        Just (LettingType _ (TEnum {})) -> return True
        Just (GivenType   _ (TEnum {})) -> return True
        _ -> return False
isOrderedType _ = return False

addBinding' ::
    ( MonadError (Nested Doc) m
    , MonadState st m
    , Has st BindingsMap
    , Has st [(GNode,GNode)]
    ) => Binding -> m ()
addBinding' b@(Find        (Identifier i) _) = addBinding i b
addBinding' b@(Given       (Identifier i) _) = addBinding i b
addBinding' b@(LettingType (Identifier i) _) = addBinding i b
addBinding' b@(GivenType   (Identifier i) _) = addBinding i b
addBinding' (LettingDomain (Identifier i) j) = addBinding i j
addBinding' (LettingExpr   (Identifier i) j) = addBinding i j
addBinding' (LettingLambda (Identifier i) j) = addBinding i j
addBinding' (LettingQuan   (Identifier i) j) = addBinding i j
addBinding' b@(BindingDim  (Identifier i) _) = addBinding i b
addBinding'    BindingDimFind {}             = return ()


data Binding
    = Find          Identifier Domain
    | Given         Identifier Domain
    | LettingType   Identifier Type
    | GivenType     Identifier Type
    | LettingDomain Identifier Domain
    | LettingExpr   Identifier Expr
    | LettingLambda Identifier Lambda
    | LettingQuan   Identifier QuantifierDecl
    | BindingDim    Identifier Domain
    | BindingDimFind Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Binding

instance Hole Binding

instance GPlate Binding where
    gplate (Find          i j) = gplateTwo Find          i j
    gplate (Given         i j) = gplateTwo Given         i j
    gplate (LettingType   i j) = gplateTwo LettingType   i j
    gplate (GivenType     i j) = gplateTwo GivenType     i j
    gplate (LettingDomain i j) = gplateTwo LettingDomain i j
    gplate (LettingExpr   i j) = gplateTwo LettingExpr   i j
    gplate (LettingLambda i j) = gplateTwo LettingLambda i j
    gplate (LettingQuan   i j) = gplateTwo LettingQuan   i j
    gplate (BindingDim    i j) = gplateTwo BindingDim    i j
    gplate (BindingDimFind  i) = gplateSingle BindingDimFind i

instance MatchBind Binding

instance ParsePrint Binding where
    parse = error "do not use this one directly. use it via (parse :: [Binding])"
    pretty (Find          i j) = "find"    <+> pretty i <> Pr.colon <+> pretty j
    pretty (Given         i j) = "given"   <+> pretty i <> Pr.colon <+> pretty j
    pretty (LettingType   i j) = "letting" <+> pretty i <+> "be new type" <+> pretty j
    pretty (GivenType     i j) = "given"   <+> pretty i <+> "new type" <+> pretty j
    pretty (LettingDomain i j) = "letting" <+> pretty i <+> "be" <+> "domain" <+> pretty j
    pretty (LettingExpr   i j) = "letting" <+> pretty i <+> "be"              <+> pretty j
    pretty (LettingLambda i j) = "letting" <+> pretty i <+> "be" <+> "lambda" <+> pretty j
    pretty (LettingQuan   i j) = "letting" <+> pretty i <+> "be"              <+> pretty j
    pretty (BindingDim    i j) = "dim"     <+> pretty i <> Pr.colon <+> pretty j
    pretty (BindingDimFind  i) = prettyExprTopLevel i

instance ParsePrint [Binding] where
    parse = do
        let one = msum1
                    [ do
                        lexeme L_find
                        decls <- flip sepBy1 comma $ do
                            is <- parse `sepBy1` comma
                            colon
                            j  <- parse
                            return [ Find i j | i <- is ]
                        return $ concat decls
                        <?> "find statement"
                    , do
                        lexeme L_given
                        decls <- flip sepBy1 comma $ do
                            is <- parse `sepBy1` comma
                            msum1
                                [ do
                                    colon
                                    j <- parse
                                    return [ Given i j | i <- is ]
                                , do
                                    lexeme L_new
                                    lexeme L_type
                                    lexeme L_enum
                                    return [ GivenType i (TEnum Nothing) | i <- is ]
                                ]
                        return $ concat decls
                        <?> "given statement"
                    , do
                        lexeme L_letting
                        decls <- flip sepBy1 comma $ do
                            is <- parse `sepBy1` comma
                            lexeme L_be
                            msum1
                                [ do
                                    lexeme L_new
                                    lexeme L_type
                                    j <- parse
                                    case j of
                                        TEnum {}    -> return ()
                                        TUnnamed {} -> return ()
                                        _           -> fail ""
                                    return [ LettingType i j | i <- is ]
                                , do
                                    lexeme L_domain
                                    j <- parse
                                    return [ LettingDomain i j | i <- is ]
                                , do
                                    lexeme L_lambda
                                    j <- parse
                                    return [ LettingLambda i j | i <- is ]
                                , do
                                    j <- parse
                                    return [ LettingExpr i j | i <- is ]
                                , do
                                    j <- parse
                                    return [ LettingQuan i j | i <- is ]
                                ]
                        return $ concat decls
                        <?> "letting statement"
                    , do
                        lexeme L_dim
                        is <- parse `sepBy1` comma
                        colon
                        j  <- parse
                        return [ BindingDim i j | i <- is ]
                    , do
                        let p = pDimExpr <||> pQuantifiedExprAsExpr p
                        return . BindingDimFind <$> p
                    ]
        concat <$> some one
    pretty = Pr.vcat . map pretty

instance ParsePrint [Either Binding Where] where
    parse = concatMap (\ t -> case t of Left  bs -> map Left  bs
                                        Right ws -> map Right ws
                      ) <$> many parse
    pretty = Pr.vcat . map pretty

instance TypeOf Binding where
    typeOf p | trace TypeChecking ("typeOf Binding: " ++ show (pretty p)) False = undefined
    typeOf (Find          _ d) = typeOf d
    typeOf (Given         _ d) = typeOf d
    typeOf (LettingDomain _ d) = typeOf d
    typeOf (LettingExpr   _ x) = typeOf x
    typeOf (LettingLambda _ x) = typeOf x
    typeOf (LettingQuan   _ x) = typeOf x
    typeOf (LettingType   i (TEnum {})) = return (THole i)
    typeOf (LettingType   _ t         ) = return t
    typeOf (GivenType     i (TEnum {})) = return (THole i)
    typeOf (GivenType     _ t         ) = return t
    typeOf (BindingDim    _ d) = typeOf d
    typeOf BindingDimFind {}   = return TBool
