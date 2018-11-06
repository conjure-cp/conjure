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
    case i of WithLocals{} -> na "bubble-delay" ; _ -> return ()
    typeI <- typeOf i
--    traceM $ show typeI
    if typeI `containsType` inner
      then do
        if typesUnify [inner, typeI]
           then do        
               innerD <- domainOf i
               let prmTup pt = take (length pt) $ zip (cycle pt) (drop 1 $ cycle pt)
                   permTups = join $ prmTup <$> elems 
               let outLiteral = make matrixLiteral
                                   (TypeMatrix (TypeInt NoTag) (TypeTuple [inner,inner]))
                                   (DomainInt NoTag [RangeBounded 1 (fromInt (genericLength permTups))])
                                   [ AbstractLiteral (AbsLitTuple [a,b])
                                   | (a,b) <- permTups 
                                   ]
               return
                  ( "Horizontal rule for permutation literal application to a single value (permute), AsFunction representation"
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
      else return
             ( "Horizontal rule for permutation application to a type the permutation doesn't care about"
             , do
               return [essence| &i |]
             )
  theRule _ = na "rule_Permute_Literal"


rule_Permute_Literal_Comprehension :: Rule
rule_Permute_Literal_Comprehension = "permutation-permute-literal-comprehension{AsFunction}" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, p, i), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
      Generator (GenInExpr pat [essence| permute(&p, &i) |]) -> return (pat, p, i)
      _ -> na "rule_Comprehension"
    case i of WithLocals{} -> na "bubble-delay" ; _ -> return ()
    (TypePermutation inner, elems) <- match permutationLiteral p 
    typeI <- typeOf i
    if typeI `containsType` inner
      then do
        if typesUnify [inner, typeI]
           then do        
               innerD <- domainOf i
               let prmTup pt = take (length pt) $ zip (cycle pt) (drop 1 $ cycle pt)
                   permTups = join $ prmTup <$> elems 
               let outLiteral = make matrixLiteral
                                   (TypeMatrix (TypeInt NoTag) (TypeTuple [inner,inner]))
                                   (DomainInt NoTag [RangeBounded 1 (fromInt (genericLength permTups))])
                                   [ AbstractLiteral (AbsLitTuple [a,b])
                                   | (a,b) <- permTups 
                                   ]
               return
                  ( "Horizontal rule for permutation literal application to a single value (permute), AsFunction representation"
                  , do
                    (hName, h) <- auxiliaryVar
                    (fPat, f)  <- quantifiedVar
                    (tPat, t)  <- quantifiedVar
                    (gPat, g)  <- quantifiedVar
                    (ePat, _)  <- quantifiedVar
                    return $ WithLocals 
                              (Comprehension body $ gocBefore
                                                ++ [Generator (GenInExpr pat
                                                                [essence| &h |])]
                                                ++ gocAfter)
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
      else return
             ( "Horizontal rule for permutation application to a type the permutation doesn't care about"
             , return 
                   (Comprehension body $ gocBefore
                                     ++ [Generator (GenInExpr pat [essence| &i |])]
                                     ++ gocAfter)
             )
  theRule _ = na "rule_Permute_Literal"


