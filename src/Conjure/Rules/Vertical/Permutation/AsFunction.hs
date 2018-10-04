{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Permutation.AsFunction where

import Conjure.Rules.Import

rule_Permute_Comprehension_Tuples :: Rule
rule_Permute_Comprehension_Tuples = "permutation-comprehension-tuples{AsFunction}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) =  do
        (gocBefore, (pat, perm), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat [essence| &perm |]  ) -> return (pat, perm)
            _ -> na "rule_Comprehension"
        TypePermutation{}                 <- typeOf perm
        Permutation_AsFunction <- representationOf perm
        [f] <- downX1 perm
        return
            ( "Vertical rule for permutation-comprehension-tuples, AsFunction representation"
            , do
                return $ Comprehension body 
                        $  gocBefore
                        ++ [ Generator (GenInExpr pat [essence| &f|])
                           ]
                        ++ gocAfter 
            )
    theRule _ = na "rule_Comprehension"



rule_Permute :: Rule
rule_Permute = "permutation-permute{AsFunction}" `namedRule` theRule where
  theRule [essence| permute(&p, &i) |] = do
    case p of WithLocals{} -> na "bubble-delay" ; _ -> return ()
    TypePermutation inner <- typeOf p 
    typeI <- typeOf i
    [f] <- downX1 p
    if typesUnify [inner, typeI]
       then return
              ( "Vertical rule for permutation application to a single value (permute), AsFunction representation"
              , do
                return [essence| [&i, catchUndef(image(&f,&i),0)][toInt(&i in defined(&f))+1] |]
              )
       else na "rule_Permute"
  theRule _ = na "rule_Permute"

