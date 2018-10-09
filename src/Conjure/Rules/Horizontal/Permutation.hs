{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Horizontal.Permutation where
import Conjure.Rules.Import
import Data.List (cycle)

rule_Apply :: Rule
rule_Apply = "permutation-apply{rule_Apply}" `namedRule` theRule where
  theRule [essence| permute(apply(&g, &h),&i) |] = do
    TypePermutation innerG <- typeOf g
    TypePermutation innerH <- typeOf g
    typeI <- typeOf i
    if typesUnify [innerG, innerH, typeI]
       then return
            ( "Horizontal rule for permutation composition/application"
            , do
              return [essence| permute(&g, permute(&h,&i)) |]
            )
       else na "rule_Apply"
  theRule _ = na "rule_Apply"



rule_Permute_Literal :: Rule
rule_Permute_Literal = "permutation-permute-literal{AsFunction}" `namedRule` theRule where
  theRule [essence| permute(&p, &i) |] = do
    (TypePermutation inner, elems) <- match permutationLiteral p 
    typeI <- typeOf i
    if typesUnify [inner, typeI]
       then do        
           innerD <- domainOf i
           let prmTup pt = take (length pt) $ zip (cycle pt) (drop 1 $ cycle pt)
               permTups = join $ prmTup <$> elems 
           let outLiteral = make matrixLiteral
                               (TypeMatrix TypeInt (TypeTuple [inner,inner]))
                               (DomainInt [RangeBounded 1 (fromInt (genericLength permTups))])
                               [ AbstractLiteral (AbsLitTuple [a,b])
                               | (a,b) <- permTups 
                               ]
           return
              ( "Vertical rule for permutation literal application to a single value (permute), AsFunction representation"
              , do
                (hName, h) <- auxiliaryVar
                (fPat, f)  <- quantifiedVar
                (tPat, t)  <- quantifiedVar
                (gPat, g)  <- quantifiedVar
                (ePat, _)  <- quantifiedVar
                return $ WithLocals 
                           [essence| &h |]
                          (AuxiliaryVars 
                            [ Declaration (FindOrGiven LocalFind hName innerD)
                            , SuchThat
                                [ [essence| 
                                      (forAll (&fPat, &tPat) in &outLiteral . &f = &i <-> &h = &t)
                                   /\ (!(exists (&gPat, &ePat) in &outLiteral . &g = &h) <-> &h = &i)
                                  |]
                                ]
                            ]
                          )
              )
       else na "rule_Permute_Literal"
  theRule _ = na "rule_Permute_Literal"


