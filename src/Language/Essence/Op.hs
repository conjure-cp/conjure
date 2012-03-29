{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Essence.Op where

import Data.Generics ( Data )
import Data.Maybe ( mapMaybe )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified Data.Set as S

import GenericOps.Core ( NodeTag, Hole, GPlate, gplate, gplateLeaf, MatchBind )
import ParsePrint ( ParsePrint, isoParsePrint, fromPairs )
import Utils ( allValues )



-- the data type for operators in Essence
data Op
    = Plus | Minus | Times | Div | Mod | Pow | Negate
    | Factorial
    | Lt | Leq | Gt | Geq | Neq | Eq
    | Not | Or | And | Imply | Iff
    | Union | Intersect | Subset | SubsetEq | Supset | SupsetEq
    | In | Max | Min
    | ToSet | ToMSet | ToRel | Defined | Range
    | Image | PreImage | Inverse
    | Together | Apart
    | Party | Participants | Parts
    | Freq | Hist

    | TwoBars

    | Index

    | HasType | HasDomain

    | Replace

    | AllDiff

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

opFace :: Op -> String
opFace Plus         = "+"
opFace Minus        = "-"
opFace Times        = "*"
opFace Div          = "/"
opFace Mod          = "%"
opFace Pow          = "**"
opFace Negate       = "-"
opFace Factorial    = "!"
opFace Lt           = "<"
opFace Leq          = "<="
opFace Gt           = ">"
opFace Geq          = ">="
opFace Neq          = "!="
opFace Eq           = "="
opFace Not          = "!"
opFace Or           = "\\/"
opFace And          = "/\\"
opFace Imply        = "->"
opFace Iff          = "<->"
opFace Union        = "union"
opFace Intersect    = "intersect"
opFace Subset       = "subset"
opFace SubsetEq     = "subseteq"
opFace Supset       = "supset"
opFace SupsetEq     = "supseteq"
opFace In           = "in"
opFace Max          = "max"
opFace Min          = "min"
opFace ToSet        = "toSet"
opFace ToMSet       = "toMSet"
opFace ToRel        = "toRel"
opFace Defined      = "defined"
opFace Range        = "range"
opFace Image        = "image"
opFace PreImage     = "preimage"
opFace Inverse      = "inverse"
opFace Together     = "together"
opFace Apart        = "apart"
opFace Party        = "party"
opFace Participants = "participants"
opFace Parts        = "parts"
opFace Freq         = "freq"
opFace Hist         = "hist"
opFace TwoBars      = "{TwoBars}"
opFace Index        = "{Index}"
opFace HasType      = "::"
opFace HasDomain    = ":"
opFace Replace      = "{Replace}"
opFace AllDiff      = "allDiff"

instance NodeTag Op

instance Hole Op

instance GPlate Op where
    gplate = gplateLeaf

instance MatchBind Op

instance ParsePrint Op where
    isoParsePrint = fromPairs $ mapMaybe (\ v ->
                                            case opFace v of
                                                ""   -> Nothing
                                                face -> Just (v, face)
                                         ) allValues
