{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Vertical.Permutation where
import Conjure.Rules.Import

rule_Cardinality :: Rule
rule_Cardinality = "permutation-cardinality" `namedRule` theRule where
  theRule po = do
        p                              <- match opTwoBars po
        TypePermutation{}              <- typeOf p
        Permutation_AsFunction         <- representationOf p
        DomainPermutation _ _ innerDom <- domainOf p
        [fun]                          <- downX1 p
        return
            ( "Vertical rule for permutation cardinality, AsFunction representation."
            , do
               (iPat, i) <- quantifiedVarOverDomain (forgetRepr innerDom)
               return $ [essence| 
                          sum([ toInt(&i != image(&fun, &i)) | &iPat : &innerDom ])
                                       |]
            )

rule_Defined :: Rule
rule_Defined = "permutation-defined" `namedRule` theRule where
  theRule po = do
        p                              <- match opDefined po
        TypePermutation{}              <- typeOf p
        Permutation_AsFunction         <- representationOf p
        [fun]                          <- downX1 p
        return
            ( "Vertical rule for permutation defined, AsFunction representation."
            , do
               return [essence| defined(&fun) |]
            )



rule_Comprehension :: Rule
rule_Comprehension = "permutation-comprehension-tuples{AsFunction}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) =  do
        (gocBefore, (pat, perm), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat expr) -> return (pat, matchDefs [opToSet] expr)
            _ -> na "rule_Comprehension"
        TypePermutation{}                 <- typeOf perm
        Permutation_AsFunction <- representationOf perm
        [f] <- downX1 perm
        return
            ( "Vertical rule for permutation-comprehension"
            , do
                (lPat, l) <- quantifiedVar
                (rPat, r) <- quantifiedVar
                return $ Comprehension body 
                        $  gocBefore
                        ++ [ Generator (GenInExpr pat [essence| [(&l,&r) 
                                                                | (&lPat, &rPat) <- &f
                                                                , &l != &r] |])
                           ]
                        ++ gocAfter 
            )
    theRule _ = na "rule_Comprehension"



rule_Image :: Rule
rule_Image = "permutation-image{AsFunction}" `namedRule` theRule where
  theRule [essence| image(&p, &i) |] = do
    TypePermutation inner <- typeOf p 
    case match permutationLiteral p of
      Nothing -> do
        typeI <- typeOf i
        if let ?typeCheckerMode = StronglyTyped in typesUnify [inner, typeI] 
          then do
            [f] <- downX1 p
            return ( "Vertical rule for permutation application to a single value" 
                   , do
                     return [essence| [&i, catchUndef(image(&f,&i),0)][toInt(&i in defined(&f))+1] |]
                   )
          else if let ?typeCheckerMode = StronglyTyped in typeI `containsType` inner
                 then na "rule_Image"
                 else return ( "Vertical rule for permutation application to a type the permutation doesn't care about"
                             , do
                               return [essence| &i |]
                             )
      _ -> na "rule_Image"
  theRule _ = na "rule_Image"


rule_Permutation_DotLt :: Rule
rule_Permutation_DotLt = "permuation-dotlt" `namedRule` theRule where
  theRule [essence| &lhs .< &rhs |] = do
    TypePermutation _ <- typeOf lhs
    TypePermutation _ <- typeOf rhs
    [fl] <- downX1 lhs
    [fr] <- downX1 rhs
    return
      ( "Vertical rule for permutation dot less."
      , return [essence| &fl .< &fr |]
      )
  theRule _ = na "rule_Permutation_DotLt"



rule_Matrix_Image :: Rule
rule_Matrix_Image = "matrix-image" `namedRule` theRule where
    theRule [essence| image(&perm, &y) |]  = do
      ty@(TypeMatrix _ _) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if let ?typeCheckerMode = StronglyTyped in ty `containsTypeComprehendable` inn
        then do
          let y' = flattenIfNeeded (matrixNumDims ty) y
          dm@(DomainMatrix dyindex _) <- domainOf y'
          return
              ( "Horizontal rule for image matrix"
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
                                  &py[image(&perm,&d)] = image(&perm,&y'[&d]) 
                             |]
                           ]
                         ]
                       )
              )
        else na "rule_Matrix_Image"
    theRule _ = na "rule_Matrix_Image"

rule_Matrix_Image_Comprehension :: Rule
rule_Matrix_Image_Comprehension = "matrix-image-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, perm, y), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
         Generator (GenInExpr pat [essence| image(&perm, &y) |]) -> return (pat, perm, y)
         _ -> na "rule_Matrix_Image"
      ty@(TypeMatrix _ _) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if let ?typeCheckerMode = StronglyTyped in not $ typesUnify [ty, inn]
        then do
          unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
          let y' = flattenIfNeeded (matrixNumDims ty) y
          dm@(DomainMatrix dyindex _) <- domainOf y'
          return
              ( "Horizontal rule for image matrix in comprehension"
              , do
                (dPat, d) <- quantifiedVar
                (pyName, py) <- auxiliaryVar
                return $ WithLocals
                      (Comprehension body $ gocBefore
                                        ++ [Generator (GenInExpr pat [essence| &py |])]
                                        ++ gocAfter)
                       (AuxiliaryVars
                         [ Declaration (FindOrGiven LocalFind pyName dm)
                         , SuchThat
                           [ [essence|
                                forAll &dPat : &dyindex .
                                  &py[image(&perm,&d)] = image(&perm,&y'[&d]) 
                             |]
                           ]
                         ]
                       )
              )
        else na "rule_Matrix_Image_Comprehension"
    theRule _ = na "rule_Matrix_Image_Comprehension"

