{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Value where

import Control.Applicative
import Control.Monad ( unless )
import Control.Monad.Error ( throwError )
import Data.Generics ( Data )
import Data.Map ( elems )
import Data.Maybe ( mapMaybe )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary )

import Has
import GenericOps.Core ( NodeTag
                       , Hole, hole
                       , HoleStatus(..)
                       , GPlate, gplate, gplateLeaf, gplateSingle, gplateUniList
                       , fromGs
                       , MatchBind, BindingsMap )
import ParsecUtils
import ParsePrint
import PrintUtils ( (<+>), text )
import qualified PrintUtils as Pr

import Language.Essence.Binding
import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Range
import Language.Essence.Type



toVTuple :: [Expr] -> Expr
toVTuple = V . VTuple

data Value = VHole Identifier
    | VBool   Bool
    | VInt   Integer
    | VEnum  Identifier
    | VMatrix    [Expr]         -- uniform type.
    | VTuple     [Expr]
    | VSet       [Expr]         -- uniform type. unique.
    | VMSet      [Expr]         -- uniform type.
    | VFunction  [Expr]         -- VTuple#2. uniform type.
    | VRelation  [Expr]         -- VTuple. uniform type.
    | VPartition [Expr]         -- VSet. uniform type.
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Value

instance Hole Value where
    hole (VHole (Identifier "_")) = UnnamedHole
    hole (VHole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Value where
    gplate p@(VHole {}) = gplateLeaf p
    gplate p@(VBool {}) = gplateLeaf p
    gplate p@(VInt  {}) = gplateLeaf p
    gplate   (VEnum       i) = gplateSingle  VEnum      i
    gplate   (VMatrix    xs) = gplateUniList VMatrix    xs
    gplate   (VTuple     xs) = gplateUniList VTuple     xs
    gplate   (VSet       xs) = gplateUniList VSet       xs
    gplate   (VMSet      xs) = gplateUniList VMSet      xs
    gplate   (VFunction  xs) = gplateUniList VFunction  xs
    gplate   (VRelation  xs) = gplateUniList VRelation  xs
    gplate   (VPartition xs) = gplateUniList VPartition xs

instance MatchBind Value

instance ParsePrint Value where
    parse = choiceTry
                [ pHole, pBool, pInt
                , pMatrix, pTuple, pSet, pMSet
                , pFunction, pRelation, pPartition
                ] <?> "value"
        where
            pHole = VHole <$> parse

            pBool = VBool False <$ reserved "false"
                    <|>
                    VBool True  <$ reserved "true"

            pInt = VInt <$> integer

            pMatrix = VMatrix <$> brackets (sepBy parse comma)

            pTuple = try (do reserved "tuple"; VTuple <$> parens (sepBy1 parse comma))
                     <|>
                     VTuple <$> parens (countSepAtLeast 2 parse comma)

            pSet = do VSet <$> braces (sepBy parse comma)

            pMSet = do reserved "mset"; VMSet <$> parens (sepBy parse comma)

            pFunction = do reserved "function"; VFunction <$> parens (sepBy pTuple2 comma)
                where
                    pTuple2 :: Parser Expr
                    pTuple2 = do
                        i <- parse
                        reservedOp "-->"
                        j <- parse
                        return $ V (VTuple [i,j])

            pRelation = do reserved "relation"; VRelation <$> parens (sepBy (V <$> pTuple) comma)

            pPartition = do reserved "partition"; VPartition <$> parens (sepBy aPart comma)
                where
                    aPart :: Parser Expr
                    aPart = (V . VSet) <$> braces (sepBy parse comma)

    pretty (VHole (Identifier nm)) = text nm
    pretty (VBool False) = "false"
    pretty (VBool True ) = "true"
    pretty (VInt  i    ) = Pr.integer i
    pretty (VEnum i    ) = pretty i
    pretty (VMatrix xs) = prettyList Pr.brackets Pr.comma xs
    pretty (VTuple [] ) = "tuple ()"
    pretty (VTuple [x]) = "tuple" <+> Pr.parens (pretty x)
    pretty (VTuple xs ) = prettyList Pr.parens Pr.comma xs
    pretty (VSet  xs) = prettyList Pr.braces Pr.comma xs
    pretty (VMSet xs) = "mset" <+> prettyList Pr.parens Pr.comma xs
    pretty (VFunction xs) = "function" <+> prettyListDoc Pr.parens Pr.comma (map prE xs)
        where
            prE (V (VTuple [i,j])) = pretty i <+> "-->" <+> pretty j
            prE p = pretty p
    pretty (VRelation  xs) = "relation"  <+> prettyList Pr.parens Pr.comma xs
    pretty (VPartition xs) = "partition" <+> prettyListDoc Pr.parens Pr.comma (map prE xs)
        where
            prE (V (VSet vs)) = prettyList Pr.braces Pr.comma vs
            prE p = pretty p

instance Arbitrary Value where
    arbitrary = {-deepPromote . -}VHole <$> arbitrary

instance TypeOf Value where
    typeOf (VHole    i) = typeOf i
    typeOf (VBool    _) = return TBool
    typeOf (VInt     _) = return TInt
    typeOf (VEnum    i) = do
        st :: BindingsMap <- getM
        let
            rs = flip mapMaybe (fromGs (elems st)) $ \ t -> case t of
                    LettingType _ ty@(TEnum (Just is)) | i `elem` is -> Just ty
                    _ -> Nothing
        case rs of
            [t] -> return t
            [ ] -> throwError $ Pr.vcat $ "Undefined enum value:"
                                        : map (Pr.nest 4 . pretty) rs
            _   -> throwError $ Pr.vcat $ "Same enum value used in multiple enumerated types: "
                                        : map (Pr.nest 4 . pretty) rs

    typeOf (VMatrix []) = return $ TMatrix TInt TUnknown
    typeOf p@(VMatrix xs) = do
        t:ts <- mapM typeOf xs
        unless (all (==t) ts) $ throwError $ Pr.vcat [ "Elements of a matrix literal has to be of uniform type."
                                                     , Pr.nest 4 $ "in:" <+> pretty p
                                                     ]
        return $ TMatrix TInt t

    typeOf (VTuple xs) = do
        ts <- mapM typeOf xs
        return $ AnyType TTuple ts

    typeOf   (VSet []) = return $ AnyType TSet [TUnknown]
    typeOf p@(VSet xs) = do
        t:ts <- mapM typeOf xs
        unless (all (==t) ts) $ throwError $ Pr.vcat [ "Elements of a set literal has to be of uniform type."
                                                     , Pr.nest 4 $ "in:" <+> pretty p
                                                     ]
        return $ AnyType TSet [t]

    typeOf   (VMSet []) = return $ AnyType TMSet [TUnknown]
    typeOf p@(VMSet xs) = do
        t:ts <- mapM typeOf xs
        unless (all (==t) ts) $ throwError $ Pr.vcat [ "Elements of a multi-set literal has to be of uniform type."
                                                     , Pr.nest 4 $ "in:" <+> pretty p
                                                     ]
        return $ AnyType TMSet [t]

    typeOf   (VFunction []) = return $ AnyType TFunction [TUnknown,TUnknown]
    typeOf p@(VFunction xs) = do
        t:ts <- mapM typeOf xs
        unless (all (==t) ts) $ throwError $ Pr.vcat [ "Elements of a function literal has to be of uniform type."
                                                     , Pr.nest 4 $ "in:" <+> pretty p
                                                     ]
        case t of
            AnyType TTuple [a,b] -> return $ AnyType TFunction [a,b]
            _ -> throwError $ Pr.vcat [ "Elements of a function literal has to be 2-tuples."
                                      , Pr.nest 4 $ "in:" <+> pretty p
                                      ]

    typeOf   (VRelation []) = return $ AnyType TRelation [TUnknown]
    typeOf p@(VRelation xs) = do
        t:ts <- mapM typeOf xs
        unless (all (==t) ts) $ throwError $ Pr.vcat [ "Elements of a relation literal has to be of uniform type."
                                                     , Pr.nest 4 $ "in:" <+> pretty p
                                                     ]
        case t of
            AnyType TTuple as -> return $ AnyType TRelation as
            _ -> throwError $ Pr.vcat [ "Elements of a relation literal has to be tuples."
                                      , Pr.nest 4 $ "in:" <+> pretty p
                                      ]

    typeOf   (VPartition []) = return $ AnyType TPartition [TUnknown]
    typeOf p@(VPartition xs) = do
        t:ts <- mapM typeOf xs
        unless (all (==t) ts) $ throwError $ Pr.vcat [ "Elements of a partition literal has to be of uniform type."
                                                     , Pr.nest 4 $ "in:" <+> pretty p
                                                     ]
        case t of
            AnyType TSet [a] -> return $ AnyType TPartition [a]
            _ -> throwError $ Pr.vcat [ "Elements of a partition literal has to be sets."
                                      , Pr.nest 4 $ "in:" <+> pretty p
                                      ]

instance DomainOf Value where
    domainOf (VBool False) = return $ DInt $ RList [ V $ VInt 0 ]
    domainOf (VBool True ) = return $ DInt $ RList [ V $ VInt 1 ]
    domainOf (VInt  i    ) = return $ DInt $ RList [ V $ VInt i ]
    domainOf _ = throwError "domainOf Value not implemented."
