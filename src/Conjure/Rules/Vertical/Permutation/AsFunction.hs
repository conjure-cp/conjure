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
    if typeI `containsType` inner
      then do
        [f] <- downX1 p
        if typesUnify [inner, typeI]
          then return
                 ( "Vertical rule for permutation application to a single value" 
                 , do
                   return [essence| [&i, catchUndef(image(&f,&i),0)][toInt(&i in defined(&f))+1] |]
                 )
          else na "rule_Permute" --If we hit this then we should hit a refinement error
      else return
             ( "Vertical rule for permutation application to a type the permutation doesn't care about"
             , do
               return [essence| &i |]
             )
  theRule _ = na "rule_Permute"

rule_Matrix_Permute :: Rule
rule_Matrix_Permute = "matrix-permute" `namedRule` theRule where
    theRule [essence| permute(&perm, &y) |]  = do
        ty@(TypeMatrix _ _) <- typeOf y
        (TypePermutation _) <- typeOf perm
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        y' <- flattenIfNeeded y
        dm@(DomainMatrix dyindex _) <- domainOf y'
        return
            ( "Horizontal rule for permute matrix"
            , do
              (dPat, d) <- quantifiedVar
              (pyName, py) <- auxiliaryVar
              return $ WithLocals
                        [essence| &py |]
                     (AuxiliaryVars
                       [ Declaration (FindOrGiven LocalFind pyName dm)
                       , SuchThat
                         [ [essence|
                              forAll &dPat : &dyindex .
                                &py[&d] = permute(&perm,&y'[permute(&perm,&d)]) 
                           |]
                         ]
                       ]
                     )
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
