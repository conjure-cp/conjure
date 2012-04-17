{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Op where

import Data.Generics ( Data )
import Data.Maybe ( mapMaybe )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified Data.Set as S

import GenericOps.Core ( NodeTag, Hole, GPlate, gplate, gplateLeaf, MatchBind )
import ParsePrint ( ParsePrint, parse, pretty )
import Utils ( allValues )

import Language.EssenceLexer
import Language.EssenceLexerP



-- the data type for operators in Essence
data Op
    = Plus | Minus | Times | Div | Mod | Pow | Negate
    | Factorial
    | Lt | Leq | Gt | Geq | Neq | Eq
    | Not | Or | And | Imply | Iff
    | Union | Intersect | Subset | SubsetEq | Supset | SupsetEq
    | In | Max | Min
    | ToSet | ToMSet | ToRelation | Defined | Range
    | Image | PreImage | Inverse
    | Together | Apart
    | Party | Participants | Parts
    | Freq | Hist

    | TwoBars

    | Index

    | HasType | HasDomain

    | Replace

    | AllDiff

    | ToInt

    | Flatten | NormIndices

    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

commutativeOps :: S.Set Op
commutativeOps = S.fromList
    [ Plus, Times
    , Neq, Eq
    , Or, And, Iff
    , Union, Intersect
    ]

associativeOps :: S.Set Op
associativeOps = S.fromList
    [ Plus
    ]

opFace :: Op -> Maybe Lexeme
opFace Plus         = Just L_Plus
opFace Minus        = Just L_Minus
opFace Times        = Just L_Times
opFace Div          = Just L_Div
opFace Mod          = Just L_Mod
opFace Pow          = Just L_Pow
opFace Negate       = Just L_Minus
opFace Factorial    = Just L_ExclamationMark
opFace Lt           = Just L_Lt
opFace Leq          = Just L_Leq
opFace Gt           = Just L_Gt
opFace Geq          = Just L_Geq
opFace Neq          = Just L_Neq
opFace Eq           = Just L_Eq
opFace Not          = Just L_ExclamationMark
opFace Or           = Just L_Or
opFace And          = Just L_And
opFace Imply        = Just L_Imply
opFace Iff          = Just L_Iff
opFace Union        = Just L_union
opFace Intersect    = Just L_intersect
opFace Subset       = Just L_subset
opFace SubsetEq     = Just L_subsetEq
opFace Supset       = Just L_supset
opFace SupsetEq     = Just L_supsetEq
opFace In           = Just L_in
opFace Max          = Just L_max
opFace Min          = Just L_min
opFace ToSet        = Just L_toSet
opFace ToMSet       = Just L_toMSet
opFace ToRelation   = Just L_toRelation
opFace Defined      = Just L_defined
opFace Range        = Just L_range
opFace Image        = Just L_image
opFace PreImage     = Just L_preimage
opFace Inverse      = Just L_inverse
opFace Together     = Just L_together
opFace Apart        = Just L_apart
opFace Party        = Just L_party
opFace Participants = Just L_participants
opFace Parts        = Just L_parts
opFace Freq         = Just L_freq
opFace Hist         = Just L_hist
opFace TwoBars      = Nothing
opFace Index        = Nothing
opFace HasType      = Just L_DoubleColon
opFace HasDomain    = Just L_Colon
opFace Replace      = Nothing
opFace AllDiff      = Just L_allDiff
opFace ToInt        = Just L_toInt
opFace Flatten      = Just L_flatten
opFace NormIndices  = Just L_normIndices

instance NodeTag Op

instance Hole Op

instance GPlate Op where
    gplate = gplateLeaf

instance MatchBind Op

instance ParsePrint Op where
    parse     = msum1 [ do lexeme s; return a
                      | (a,s) <- opPairs
                      ]
                <?> "operator"
    pretty a  = case lookup a opPairs of
                    Nothing -> error $ "Cannot render: " ++ show a
                    Just l  -> lexemeFace l


opPairs :: [(Op, Lexeme)]
opPairs = mapMaybe (\ v -> case opFace v of
                                Nothing   -> Nothing
                                Just face -> Just (v, face)
                   ) allValues
