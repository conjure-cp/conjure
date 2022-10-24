module Conjure.Language.Attributes where

import Conjure.Language.Domain (BinaryRelationAttr (..))
import Conjure.Language.Expression.Op.Internal.Common (Lexeme (..))
import Conjure.Prelude
import Data.Map (Map)
import qualified Data.Map.Strict as M

type Attr = (Lexeme, Bool)

mapFrom :: [[Attr]] -> Map Lexeme Bool
mapFrom attrs = M.fromList $ concat attrs

setValidAttrs :: Map Lexeme Bool
setValidAttrs = mapFrom [sizeyAttrs]

msetValidAttrs :: Map Lexeme Bool
msetValidAttrs = mapFrom [sizeyAttrs, occursAttrs]

funAttrs :: Map Lexeme Bool
funAttrs = mapFrom [sizeyAttrs, jectivityAttrs, totalityAttrs]

seqAttrs :: Map Lexeme Bool
seqAttrs = mapFrom [sizeyAttrs, jectivityAttrs]

relAttrs :: Map Lexeme Bool
relAttrs = mapFrom [sizeyAttrs, binRelAttrs, totalityAttrs]

partitionAttrs :: Map Lexeme Bool
partitionAttrs = mapFrom [sizeyAttrs, partSizeAttrs,partNumAttrs, regularity]

sizeyAttrs :: [Attr]
sizeyAttrs =
    [ (L_size, True)
    , (L_maxSize, True)
    , (L_minSize, True)
    ]

occursAttrs :: [Attr]
occursAttrs =
    [ (L_minOccur, True)
    , (L_maxOccur, True)
    ]

partNumAttrs :: [Attr]
partNumAttrs =
    [ (L_numParts, True)
    , (L_maxNumParts, True)
    , (L_minNumParts, True)
    ]

partSizeAttrs :: [(Lexeme, Bool)]
partSizeAttrs =
    [ (L_partSize, True)
    , (L_minPartSize, True)
    , (L_maxPartSize, True)
    ]

jectivityAttrs :: [(Lexeme, Bool)]
jectivityAttrs =
    [ (L_injective, False)
    , (L_bijective, False)
    , (L_surjective, False)
    ]

binRelAttrs :: [(Lexeme, Bool)]
binRelAttrs =
    [ (L_reflexive, False)
    , (L_irreflexive, False)
    , (L_coreflexive, False)
    , (L_symmetric, False)
    , (L_antiSymmetric, False)
    , (L_aSymmetric, False)
    , (L_transitive, False)
    , (L_connex, False)
    , (L_Euclidean, False)
    , (L_serial, False)
    , (L_equivalence, False)
    , (L_partialOrder, False)
    ]

lexemeToBinRel :: Lexeme -> Maybe BinaryRelationAttr
lexemeToBinRel L_reflexive = Just BinRelAttr_Reflexive
lexemeToBinRel L_irreflexive = Just BinRelAttr_Irreflexive
lexemeToBinRel L_coreflexive = Just BinRelAttr_Coreflexive
lexemeToBinRel L_symmetric = Just BinRelAttr_Symmetric
lexemeToBinRel L_antiSymmetric = Just BinRelAttr_AntiSymmetric
lexemeToBinRel L_aSymmetric = Just BinRelAttr_ASymmetric
lexemeToBinRel L_transitive = Just BinRelAttr_Transitive
-- lexemeToBinRel L_total = Just BinRelAttr_Total
lexemeToBinRel L_connex = Just BinRelAttr_Connex
lexemeToBinRel L_Euclidean = Just BinRelAttr_Euclidean
lexemeToBinRel L_serial = Just BinRelAttr_Serial
lexemeToBinRel L_equivalence = Just BinRelAttr_Equivalence
lexemeToBinRel L_partialOrder = Just BinRelAttr_PartialOrder
lexemeToBinRel _ = Nothing

totalityAttrs :: [Attr]
totalityAttrs = [(L_total, False)]

regularity :: [Attr]
regularity = [(L_regular, False)]

allAttributLexemes :: [Lexeme]
allAttributLexemes =
    concatMap
        (map fst)
        [ sizeyAttrs
        , jectivityAttrs
        , occursAttrs
        , partNumAttrs
        , partSizeAttrs
        , binRelAttrs
        , totalityAttrs
        , regularity
        ]