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
        one BinRelAttr_ASymmetric    = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> !(&rel(&y,&x)) |]
        one BinRelAttr_Transitive    = return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&y,&z) -> &rel(&x,&z) |]
        one BinRelAttr_Total         = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) |]
        one BinRelAttr_Connex        = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) \/ (&x = &y) |]
        one BinRelAttr_Euclidean     = return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&x,&z) -> &rel(&y,&z) |]
        one BinRelAttr_Serial        = return [essence| forAll &xP : &dom . exists &yP : &dom . &rel(&x,&y) |]
        one BinRelAttr_Equivalence   = one BinRelAttr_Reflexive ++ one BinRelAttr_Symmetric     ++ one BinRelAttr_Transitive
        one BinRelAttr_PartialOrder  = one BinRelAttr_Reflexive ++ one BinRelAttr_AntiSymmetric ++ one BinRelAttr_Transitive

    return $ concatMap one (S.toList binRelAttrs)

-- reflexive        forAll x : T . rel(x,x)
-- irreflexive      forAll x : T . !rel(x,x)
-- coreflexive      forAll x,y : T . rel(x,y) -> x = y
--
-- symmetric        forAll x,y : T . rel(x,y) -> rel(y,x)
-- antisymmetric    forAll x,y : T . rel(x,y) /\ rel(y,x) -> x = y
-- asymmetric       forAll x,y : T . rel(x,y) -> !rel(y,x)
--
-- transitive       forAll x,y,z : T . rel(x,y) /\ rel(y,z) -> rel(x,z)
--
-- total            forAll x,y : T . rel(x,y) \/ rel(y,x)
-- connex           forAll x,y : T . rel(x,y) \/ rel(y,x) \/ x = y
-- Euclidean        forAll x,y,z : T . rel(x,y) /\ rel(x,z) -> rel(y,z)
-- serial           forAll x : T . exists y : T . rel(x,y)
-- equivalence      reflexive + symmetric + transitive
-- partialOrder     reflexive + antisymmetric + transitive


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
        one BinRelAttr_Reflexive     = return [essence| &maxNum / &divisor <= sum &xP           : &dom . &rel(&x,&x) |]
        one BinRelAttr_Irreflexive   = return [essence| &maxNum / &divisor <= sum &xP           : &dom . !(&rel(&x,&x)) |]
        one BinRelAttr_Coreflexive   = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . &rel(&x,&y) -> &x=&y |]
        one BinRelAttr_Symmetric     = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . &rel(&x,&y) -> &rel(&y,&x) |]
        one BinRelAttr_AntiSymmetric = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . &rel(&x,&y) /\ &rel(&y,&x) -> &x=&y |]
        one BinRelAttr_ASymmetric    = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . &rel(&x,&y) -> !(&rel(&y,&x)) |]
        one BinRelAttr_Transitive    = return [essence| &maxNum / &divisor <= sum &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&y,&z) -> &rel(&x,&z) |]
        one BinRelAttr_Total         = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) |]
        one BinRelAttr_Connex        = return [essence| &maxNum / &divisor <= sum &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) \/ (&x = &y) |]
        one BinRelAttr_Euclidean     = return [essence| &maxNum / &divisor <= sum &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&x,&z) -> &rel(&y,&z) |]
        one BinRelAttr_Serial        = return [essence| &maxNum / &divisor <= sum &xP : &dom . exists &yP : &dom . &rel(&x,&y) |]
        one BinRelAttr_Equivalence   = one BinRelAttr_Reflexive ++ one BinRelAttr_Symmetric     ++ one BinRelAttr_Transitive
        one BinRelAttr_PartialOrder  = one BinRelAttr_Reflexive ++ one BinRelAttr_AntiSymmetric ++ one BinRelAttr_Transitive

    return $ concatMap one (S.toList binRelAttrs)

