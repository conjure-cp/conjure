{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Common where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH

-- containers
import Data.Set as S ( toList )


mkSizeCons :: SizeAttr Expression -> Expression -> [Expression]
mkSizeCons sizeAttr cardinality =
    case sizeAttr of
        SizeAttr_None           -> []
        SizeAttr_Size x         -> return [essence| &x =  &cardinality |]
        SizeAttr_MinSize x      -> return [essence| &x <= &cardinality |]
        SizeAttr_MaxSize y      -> return [essence| &cardinality <= &y |]
        SizeAttr_MinMaxSize x y -> [ [essence| &x <= &cardinality |]
                                   , [essence| &cardinality <= &y |]
                                   ]

mkBinRelCons :: NameGen m => BinaryRelationAttrs -> Domain r Expression -> Expression -> m [Expression]
mkBinRelCons (BinaryRelationAttrs binRelAttrs) dom rel = do
    (xP, x) <- quantifiedVar
    (yP, y) <- quantifiedVar
    (zP, z) <- quantifiedVar

    let
        one BinRelAttr_Reflexive     = return [essence| forAll &xP           : &dom . &rel(&x,&x) |]
        one BinRelAttr_Irreflexive   = return [essence| forAll &xP           : &dom . !(&rel(&x,&x)) |]
        one BinRelAttr_Coreflexive   = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &x=&y |]
        one BinRelAttr_Symmetric     = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &rel(&y,&x) |]
        one BinRelAttr_AntiSymmetric = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) /\ &rel(&y,&x) -> &x=&y |]
        one BinRelAttr_ASymmetric    = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> !&rel(&y,&x) |]
        one BinRelAttr_Transitive    = return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&y,&z) -> &rel(&x,&z) |]
        one BinRelAttr_Total         = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) |]
        one BinRelAttr_Connex        = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) \/ (&x = &y) |]
        one BinRelAttr_Euclidean     = return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&x,&z) -> &rel(&y,&z) |]
        one BinRelAttr_LeftTotal     = return [essence| forAll &xP : &dom . exists &yP : &dom . &rel(&x,&y) |]
        one BinRelAttr_RightTotal    = return [essence| forAll &yP : &dom . exists &xP : &dom . &rel(&x,&y) |]

        one BinRelAttr_Serial             = one BinRelAttr_LeftTotal
        one BinRelAttr_Equivalence        = one BinRelAttr_Reflexive ++ one BinRelAttr_Symmetric     ++ one BinRelAttr_Transitive
        one BinRelAttr_LinearOrder        = one BinRelAttr_Total ++ one BinRelAttr_AntiSymmetric ++ one BinRelAttr_Transitive
        one BinRelAttr_WeakOrder          = one BinRelAttr_Total ++ one BinRelAttr_Transitive
        one BinRelAttr_PreOrder           = one BinRelAttr_Reflexive ++ one BinRelAttr_Transitive
        one BinRelAttr_PartialOrder       = one BinRelAttr_Reflexive ++ one BinRelAttr_AntiSymmetric ++ one BinRelAttr_Transitive
        one BinRelAttr_StrictPartialOrder = one BinRelAttr_Irreflexive ++ one BinRelAttr_ASymmetric ++ one BinRelAttr_Transitive

    return $ concatMap one (S.toList binRelAttrs)


mkBinRelConsSoft :: NameGen m => Expression -> Expression -> BinaryRelationAttrs -> Domain r Expression -> Expression -> m [Expression]
mkBinRelConsSoft maxNum divisor (BinaryRelationAttrs binRelAttrs) dom rel = do
    -- maxNum: if the property holds this many times, we perfectly satisfy this condition
    -- divisor: 2,4,8
    --          2: satisfy the property at least half the time
    --          4: satisfy the property at least one quarter of the time
    --          8: satisfy the property at least 1/8th of the time
    (xP, x) <- quantifiedVar
    (yP, y) <- quantifiedVar
    (zP, z) <- quantifiedVar

    let
        one BinRelAttr_Reflexive     = return [essence| &maxNum / &divisor <= sum &xP           : &dom . toInt(&rel(&x,&x)) |]
        one BinRelAttr_Irreflexive   = return [essence| &maxNum / &divisor <= sum &xP           : &dom . toInt(!(&rel(&x,&x))) |]
        one BinRelAttr_Coreflexive   = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . toInt(&rel(&x,&y) -> &x=&y) |]
        one BinRelAttr_Symmetric     = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . toInt(&rel(&x,&y) -> &rel(&y,&x)) |]
        one BinRelAttr_AntiSymmetric = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . toInt(&rel(&x,&y) /\ &rel(&y,&x) -> &x=&y) |]
        one BinRelAttr_ASymmetric    = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . toInt(&rel(&x,&y) -> !(&rel(&y,&x))) |]
        one BinRelAttr_Transitive    = return [essence| &maxNum / &divisor <= sum &xP, &yP, &zP : &dom . toInt(&rel(&x,&y) /\ &rel(&y,&z) -> &rel(&x,&z)) |]
        one BinRelAttr_Total         = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . toInt(&rel(&x,&y) \/ &rel(&y,&x)) |]
        one BinRelAttr_Connex        = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . toInt(&rel(&x,&y) \/ &rel(&y,&x) \/ (&x = &y)) |]
        one BinRelAttr_Euclidean     = return [essence| &maxNum / &divisor <= sum &xP, &yP, &zP : &dom . toInt(&rel(&x,&y) /\ &rel(&x,&z) -> &rel(&y,&z)) |]
        one BinRelAttr_LeftTotal     = return [essence| &maxNum / &divisor <= sum &xP : &dom . sum &yP : &dom . toInt(&rel(&x,&y)) |]
        one BinRelAttr_RightTotal    = return [essence| &maxNum / &divisor <= sum &yP : &dom . sum &xP : &dom . toInt(&rel(&x,&y)) |]

        one BinRelAttr_Serial             = one BinRelAttr_LeftTotal
        one BinRelAttr_Equivalence        = one BinRelAttr_Reflexive ++ one BinRelAttr_Symmetric     ++ one BinRelAttr_Transitive
        one BinRelAttr_LinearOrder        = one BinRelAttr_Total ++ one BinRelAttr_AntiSymmetric ++ one BinRelAttr_Transitive
        one BinRelAttr_WeakOrder          = one BinRelAttr_Total ++ one BinRelAttr_Transitive
        one BinRelAttr_PreOrder           = one BinRelAttr_Reflexive ++ one BinRelAttr_Transitive
        one BinRelAttr_PartialOrder       = one BinRelAttr_Reflexive ++ one BinRelAttr_AntiSymmetric ++ one BinRelAttr_Transitive
        one BinRelAttr_StrictPartialOrder = one BinRelAttr_Irreflexive ++ one BinRelAttr_ASymmetric ++ one BinRelAttr_Transitive



    return $ concatMap one (S.toList binRelAttrs)


-- reflexive, the diagonal is full
-- irreflexive, the diagonal is empty
-- coreflexive, only the diagonal is full
-- symmetric, if R(x,y) then R(y,x)
-- anti-symmetric, both R(x,y) and R(y,x) are never full at the same time (we say nothing about the the diagonal)
-- a-symmetric, anti-symmetric + irreflexive
-- transitive xy + yz -> xz
-- total: either R(x,y) or R(y,x). implies reflexive
-- connex: total, but doesn't imply reflexive (we say nothing about the diagonal)
-- left-total: every x is related to some y  -- R(x,y)
-- right-total: every y is related to some x -- R(x,y)
-- Euclidean: xy + xz -> yz

-- some properties
-- total implies reflexive. connex doesn't.
-- aSymmetric implies irreflexive and antiSymmetric

-- derived definitions
-- serial: left-total
-- equivalance: reflexive + symmetric + transitive
-- linearOrder: antiSymmetric + total + transitive
-- weakOrder; total + transitive
-- preOrder: reflexive + transitive
-- partialOrder: reflexive + antiSymmetric + transitive
-- strictPartialOrder: irreflexive + transitive (implied aSymmetric)

