{-# Language FlexibleInstances, OverloadedStrings #-}
module Language.E.Up.Data where

import Language.E
import qualified Text.PrettyPrint as Pr
import Text.Groom(groom)


data VarInfo =  VarInfo {
    indexes :: [[Integer]],
    bounds  :: [Integer]
} deriving (Show)


data VarData =  VarData {
    vIndexes :: [[Integer]],
    vBounds  :: [Integer],
    vEssence :: E
} deriving (Show)


instance Pretty [VarData] where
    pretty vd = Pr.vcat . map pretty $ vd

instance Pretty VarData where
    pretty (VarData{vIndexes=ix, vBounds=bs, vEssence=e}) = 
        "VarData" <+> Pr.braces (
            Pr.sep [
                  "vIndexes =" <+> (pretty . groom $ ix)
                , ",vBounds ="  <+>  (pretty . groom $ bs)
                , ",vEssence = "
                , pretty e
            ]
            )

data Tree a = Leaf a
            | Branch a [Tree a]
            | Tuple [Tree a]
    deriving (Show)


data TagT = TagDomain Domain
          | TagEnum   String
          | TagUnamed String
          | TagTuple  [[TagT]]
          | TagFunc  [TagT] [TagT]
          | TagPar   [TagT]
          | TagRel   [[TagT]]
    deriving (Show)

type IndexRange = Domain

data IndexT = IndexNone 
            | IndexMatrix IndexRange IndexT
            | IndexTuple [IndexT]
            | IndexRel   [IndexT]
            | IndexFunc   IndexT  IndexT
            | IndexPar    IndexT
            | IndexSet    IndexT
    deriving (Show)

