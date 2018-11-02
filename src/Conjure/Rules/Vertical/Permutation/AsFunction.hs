{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Permutation.AsFunction where

import Conjure.Rules.Import
import Conjure.Rules.Vertical.Matrix (flattenIfNeeded)

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


-- TODO need to permute on the indices too
rule_Matrix_Permute :: Rule
rule_Matrix_Permute = "matrix-permute" `namedRule` theRule where
    theRule [essence| permute(&perm, &y) |]  = do
        ty@(TypeMatrix _ yinner) <- typeOf y
        (TypePermutation pinner) <- typeOf perm
        if typesUnify [yinner, pinner]
          then do
            unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
            y' <- flattenIfNeeded y
            DomainMatrix dyindex dyinner <- domainOf y'
            DomainPermutation _ _ dpinner <- domainOf perm
            dun <- domainUnion dpinner dyinner
            return
                ( "Horizontal rule for permute matrix"
                , do
                  (dPat, d) <- quantifiedVar
                  (pyName, py) <- auxiliaryVar
                  return $ WithLocals
                            [essence| &py |]
                         (AuxiliaryVars
                           --TODO need union of permutation and dy domains
                           [ Declaration (FindOrGiven LocalFind pyName
                                            (DomainMatrix dyindex dun))
                           , SuchThat
                             [ [essence|
                                  forAll &dPat : &dyindex .
                                    &py[&d] = permute(&perm,&y'[&d]) 
                               |]
                             ]
                           ]
                         )
                )
          else if yinner `containsType` pinner
                 then error "rule_Matrix_Permute recursion not defined yet" 
                 else return ( "horixontal rule for permute matrix no type match"
                             , return [essence| &y |]
                             )
    theRule _ = na "rule_Matrix_Permute"


--rule_Permute_Set :: Rule
--rule_Permute_Set = "permutation-permute-set{AsFunction}" `namedRule` theRule where
--  theRule [essence| permute(&p,&i) |] = do
--    TypePermutation (TypeUnnamed nameperm) <- typeOf p
--    TypeSet (TypeUnnamed nameset) <- i
--    if nameperm == nameset
--       then applyPermutationOverSet
--       else na "rule_Permute_Set"
--  theRule _ = na "rule_Permute_Set"
--
--applyPermutationOverSet = error "applyPermutationOverSet"
