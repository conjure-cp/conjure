{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Type where

import Control.Applicative
import Control.Arrow ( first )
import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter )
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary, elements )
import Test.QuickCheck.Gen ( oneof )

import GenericOps.Core ( NodeTag
                       , Hole, hole
                       , HoleStatus(..)
                       , GPlate, gplate, gplateLeaf, gplateSingle, gplateUniList
                       , GNode(..)
                       , MatchBind, BindingsMap )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty, prettyList, fromPairs )
import PrintUtils ( (<+>), Doc )
import qualified PrintUtils as Pr
import Has

import {-# SOURCE #-} Language.Essence.Expr
import {-# SOURCE #-} Language.Essence.Identifier



class TypeOf a where
    typeOf ::
        ( Applicative m
        , Has st BindingsMap
        , Has st [GNode]
        , Has st [(GNode,GNode)]
        , MonadError Doc m
        , MonadState st m
        , MonadWriter [Doc] m
        ) => a -> m Type

typeUnify :: Type -> Type -> Bool
typeUnify TUnknown _ = True
typeUnify _ TUnknown = True
typeUnify TBool TInt = True
typeUnify TInt TBool = True
typeUnify (AnyType e1 t1) (AnyType e2 t2) | e1 == e2  = and $ zipWith typeUnify t1 t2
                                          | otherwise = False
typeUnify a b | a == b = True
typeUnify _ _ = False

stackDepth :: Int
stackDepth = 5

typeError ::
    ( Applicative m
    , Has st [GNode]
    , MonadError Doc m
    , MonadState st m
    ) => Doc -> m a
typeError s = do
    nodes :: [GNode]
          <- take stackDepth <$> getM
    throwError $ Pr.vcat $ s : [ Pr.nest 4 $ "in: " <+> pretty n
                               | GNode _ n <- nodes
                               ]

typeErrorUnOp ::
    ( Applicative m
    , Has st [GNode]
    , MonadError Doc m
    , MonadState st m
    ) => Type -> Doc -> m a
typeErrorUnOp tx s = do
    nodes :: [GNode]
          <- take stackDepth <$> getM
    throwError $ Pr.vcat $ s
                         :  ( "The operand has type:" <+> pretty tx )
                         :  [ Pr.nest 4 $ "in: " <+> pretty n
                            | GNode _ n <- nodes
                            ]

typeErrorBinOp ::
    ( Applicative m
    , Has st [GNode]
    , MonadError Doc m
    , MonadState st m
    ) => Type -> Type -> Doc -> m a
typeErrorBinOp tx ty s = do
    nodes :: [GNode]
          <- take stackDepth <$> getM
    throwError $ Pr.vcat $ s
                         :  [ "First  operand has type:" <+> pretty tx
                            , "Second operand has type:" <+> pretty ty
                            ]
                         ++ [ Pr.nest 4 $ "in: " <+> pretty n
                            | GNode _ n <- nodes
                            ]

typeHasUnknowns :: Type -> Bool
typeHasUnknowns TUnknown = True
typeHasUnknowns THole    {}    = False
typeHasUnknowns TBool    {}    = False
typeHasUnknowns TInt     {}    = False
typeHasUnknowns TEnum    {}    = False
typeHasUnknowns TUnnamed {}    = False
typeHasUnknowns (TMatrix i  e) = any typeHasUnknowns [i,e]
typeHasUnknowns (TLambda is e) = any typeHasUnknowns (e:is)
typeHasUnknowns (AnyType _ ts) = any typeHasUnknowns ts


data Type = TUnknown
    | THole Identifier
    | TBool
    | TInt
    | TEnum (Maybe [Identifier])
    | TUnnamed Expr
    | TMatrix Type Type
    | TLambda [Type] Type
    | AnyType AnyTypeEnum [Type]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Type

instance Hole Type where
    hole (THole (Identifier "_")) = UnnamedHole
    hole (THole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Type where
    gplate p@(TUnknown     {}) = gplateLeaf p
    gplate   (THole         x) = gplateSingle THole x
    gplate p@(TBool        {}) = gplateLeaf p
    gplate p@(TInt         {}) = gplateLeaf p
    gplate p@(TEnum  Nothing ) = gplateLeaf p
    gplate   (TEnum (Just xs)) = gplateUniList (TEnum . Just) xs
    gplate   (TUnnamed      x) = gplateSingle TUnnamed x
    gplate   (TMatrix    i  e) = gplateUniList (\ [i',e']  -> TMatrix i' e'  ) [i,e]
    gplate   (TLambda    is o) = gplateUniList (\ (o':is') -> TLambda is' o' ) (o:is)
    gplate   (AnyType    e ts) = gplateUniList (AnyType e) ts

instance MatchBind Type

instance ParsePrint Type where
    parse = choiceTry
                [ pTHole, pTBool, pTInt, pEnum, pUnnamed, pMatrix
                , pTTuple, pTSet, pTMSet
                , pTFunction, pTRelation, pTPartition
                , pTLambda
                ]
        where
            pTHole  = THole <$> parse
            pTBool  = TBool <$  reserved "bool"
            pTInt   = TInt  <$  reserved "int"
            pEnum   = do reserved "enum" ; TEnum <$> optionMaybe (braces (sepBy parse comma))
            pUnnamed = do reserved "of"; reserved "size"; TUnnamed <$> parse
            pMatrix = do
                reserved "matrix"
                reserved "indexed"
                reserved "by"
                is <- brackets (parse `sepBy1` comma)
                reserved "of"
                e  <- parse
                return $ foldr TMatrix e is
            pTTuple = do reserved "tuple"; AnyType TTuple <$> parens (sepBy parse comma)
            pTSet   = do reserved "set"  ; reserved "of"; AnyType TSet  . return <$> parse
            pTMSet  = do reserved "mset" ; reserved "of"; AnyType TMSet . return <$> parse
            pTFunction = do
                reserved "function"
                fr <- parse
                reservedOp "-->"
                to <- parse
                return (AnyType TFunction [fr,to])
            pTRelation  = do reserved "relation" ; reserved "of"  ; AnyType TRelation  <$> parens (sepBy parse (reservedOp "*"))
            pTPartition = do reserved "partition"; reserved "from"; AnyType TPartition . return <$> parse
            pTLambda = do
                reserved "lambda"
                braces $ do
                    is <- sepBy1 parse comma
                    reservedOp "-->"
                    o  <- parse
                    return (TLambda is o)

    pretty (THole i) = pretty i
    pretty TBool = "bool"
    pretty TInt  = "int"
    pretty (TEnum Nothing  ) = "enum"
    pretty (TEnum (Just xs)) = "enum" <+> prettyList Pr.braces Pr.comma xs
    pretty (TUnnamed i) = "of size" <+> pretty i
    pretty (TMatrix i e) = "matrix" <+> "indexed"
                       <+> "by" <+> prettyList Pr.brackets Pr.comma is
                       <+> "of" <+> pretty e'
        where
            (is,e') = helper i e
            helper a b = first (a:) $ case b of TMatrix c d -> helper c d
                                                _           -> ([], b)
    pretty (TLambda is  o) = "lambda" <+> Pr.braces (prettyList id Pr.comma is <+> "-->" <+> pretty o)
    pretty (AnyType TTuple ts) = "tuple" <+> prettyList Pr.parens Pr.comma ts
    pretty (AnyType TSet  [t]) = "set"  <+> "of" <+> pretty t
    pretty (AnyType TMSet [t]) = "mset" <+> "of" <+> pretty t
    pretty (AnyType TFunction [fr,to]) = "function" <+> pretty fr <+> "-->" <+> pretty to
    pretty (AnyType TRelation  ts ) = "relation"  <+> "of"   <+> prettyList Pr.parens "*" ts
    pretty (AnyType TPartition [t]) = "partition" <+> "from" <+> pretty t
    pretty TUnknown = "?"
    pretty p = error ("Invalid type: " ++ show p)

instance Arbitrary Type where
    arbitrary = oneof
        [ THole <$> arbitrary
        , return TBool
        , return TInt
        , TEnum <$> arbitrary
        , TMatrix <$> arbitrary <*> arbitrary
        , do (i,is,o) <- arbitrary; return $ TLambda (i:is) o
        , AnyType TTuple              <$> arbitrary
        , AnyType TSet  . return      <$> arbitrary
        , AnyType TMSet . return      <$> arbitrary
        , do (fr,to)  <- arbitrary; return $ AnyType TFunction [fr,to]
        , AnyType TRelation           <$> arbitrary
        , AnyType TPartition . return <$> arbitrary
        ]
    -- shrink (TLambda is o) = do
    --     is' <- shrink is
    --     o'  <- shrink o
    --     THole (Identifier "_") : o' : is' ++ map (\ t -> TLambda t o') (drop 1 $ take (length is) $ inits is')
    -- shrink (AnyType enum is) | enum `elem` [TTuple, TRelation] = do
    --     is' <- shrink is
    --     THole (Identifier "_") : is' ++ map (AnyType enum) (take (length is) $ inits is')
    -- shrink (AnyType enum ts) = do
    --     ts' <- shrink ts
    --     return $ AnyType enum ts'
    -- shrink _ = []

instance TypeOf Type where
    typeOf = return



data AnyTypeEnum = TTuple | TSet | TMSet | TFunction | TRelation | TPartition
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

instance NodeTag AnyTypeEnum

instance Hole AnyTypeEnum

instance GPlate AnyTypeEnum

instance MatchBind AnyTypeEnum

instance ParsePrint AnyTypeEnum where
    fromPairs =
        [ ( TTuple    , "tuple"     )
        , ( TSet      , "set"       )
        , ( TMSet     , "mset"      )
        , ( TFunction , "function"  )
        , ( TRelation , "set"       )
        , ( TPartition, "partition" )
        ]

instance Arbitrary AnyTypeEnum where
    arbitrary = elements [minBound .. maxBound]


