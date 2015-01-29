{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Common where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
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

mkBinRelCons :: Pretty r => BinaryRelationAttrs -> [Name] -> Domain r Expression -> Expression -> [Expression]
mkBinRelCons (BinaryRelationAttrs binRelAttrs) fresh dom rel = concatMap one (S.toList binRelAttrs) where
    (xP, x) = quantifiedVar (fresh `at` 0)
    (yP, y) = quantifiedVar (fresh `at` 1)
    (zP, z) = quantifiedVar (fresh `at` 2)

    one BinRelAttr_Reflexive     = return [essence| forAll &xP           : &dom . &rel(&x,&x) |]
    one BinRelAttr_Irreflexive   = return [essence| forAll &xP           : &dom . !(&rel(&x,&x)) |]
    one BinRelAttr_Coreflexive   = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &x=&y |]
    one BinRelAttr_Symmetric     = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &rel(&y,&x) |]
    one BinRelAttr_AntiSymmetric = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) /\ &rel(&y,&x) -> &x=&y |]
    one BinRelAttr_ASymmetric    = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> !(&rel(&y,&x)) |]
    one BinRelAttr_Transitive    = return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&y,&z) -> &rel(&x,&z) |]
    one BinRelAttr_Total         = return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) |]
    one BinRelAttr_Euclidean     = return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&x,&z) -> &rel(&y,&z) |]
    one BinRelAttr_Serial        = return [essence| forAll &xP : &dom . exists &yP : &dom . &rel(&x,&y) |]
    one BinRelAttr_Equivalence   = one BinRelAttr_Reflexive ++ one BinRelAttr_Symmetric     ++ one BinRelAttr_Transitive
    one BinRelAttr_PartialOrder  = one BinRelAttr_Reflexive ++ one BinRelAttr_AntiSymmetric ++ one BinRelAttr_Transitive

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
-- Euclidean        forAll x,y,z : T . rel(x,y) /\ rel(x,z) -> rel(y,z)
-- serial           forAll x : T . exists y : T . rel(x,y)
-- equivalence      reflexive + symmetric + transitive
-- partialOrder     reflexive + antisymmetric + transitive

