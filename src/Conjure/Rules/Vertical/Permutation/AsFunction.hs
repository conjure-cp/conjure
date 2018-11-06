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


rule_Permute_Comprehension :: Rule
rule_Permute_Comprehension = "permutation-permute{AsFunction}" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, p, i), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
      Generator (GenInExpr pat [essence| permute(&p, &i) |]) -> return (pat, p, i)
      _ -> na "rule_Comprehension"

    TypePermutation inner <- typeOf p 
    typeI <- typeOf i
    if typeI `containsType` inner
      then do
        [f] <- downX1 p
        if typesUnify [inner, typeI]
          then return
                 ( "Vertical rule for permutation application to a single value" 
                 , return 
                   (Comprehension body $ gocBefore
                                     ++ [Generator (GenInExpr pat
                    [essence| [&i, catchUndef(image(&f,&i),0)][toInt(&i in defined(&f))+1] |])]
                                     ++ gocAfter)
                 )
          else na "rule_Permute"
      else return
             ( "Vertical rule for permutation application to a type the permutation doesn't care about"
             , return 
                   (Comprehension body $ gocBefore
                                     ++ [Generator (GenInExpr pat [essence| &i |])]
                                     ++ gocAfter)
             )
  theRule _ = na "rule_Permute"

rule_Matrix_Permute :: Rule
rule_Matrix_Permute = "matrix-permute" `namedRule` theRule where
    theRule [essence| permute(&perm, &y) |]  = do
      ty@(TypeMatrix _ _) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if not $ typesUnify [ty, inn]
        then do
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
                                  &py[permute(&perm,&d)] = permute(&perm,&y'[&d]) 
                             |]
                           ]
                         ]
                       )
              )
        else na "rule_Matrix_Permute"
    theRule _ = na "rule_Matrix_Permute"

rule_Matrix_Permute_Comprehension :: Rule
rule_Matrix_Permute_Comprehension = "matrix-permute-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, perm, y), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
         Generator (GenInExpr pat [essence| permute(&perm, &y) |]) -> return (pat, perm, y)
         _ -> na "rule_Matrix_Permute"
      ty@(TypeMatrix _ _) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if not $ typesUnify [ty, inn]
        then do
          unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
          y' <- flattenIfNeeded y
          dm@(DomainMatrix dyindex _) <- domainOf y'
          return
              ( "Horizontal rule for permute matrix in comprehension"
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
                                  &py[permute(&perm,&d)] = permute(&perm,&y'[&d]) 
                             |]
                           ]
                         ]
                       )
              )
        else na "rule_Matrix_Permute_Comprehension"
    theRule _ = na "rule_Matrix_Permute_Comprehension"

rule_Set_Permute :: Rule
rule_Set_Permute = "set-permute" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, perm, y), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
         Generator (GenInExpr pat [essence| permute(&perm, &y) |]) -> return (pat, perm, y)
         _ -> na "rule_Set_Permute"
      ts@(TypeSet _) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if not $ typesUnify [ts, inn]
         then do
           ds <- domainOf y
           return
               ( "Horizontal rule for permute set"
               , do
                 (dPat, d) <- quantifiedVar
                 (pyName, py) <- auxiliaryVar
                 return $ WithLocals
                        (Comprehension body $ gocBefore
                                          ++ [Generator (GenInExpr pat [essence| &py |])]
                                          ++ gocAfter)
                        (AuxiliaryVars
                          [ Declaration (FindOrGiven LocalFind pyName ds)
                          , SuchThat
                            [ [essence|
                                    |&y| = |&py|
                                 /\ forAll &dPat in &y .
                                      permute(&perm, &d) in &py
                              |]
                            ]
                          ]
                        )
               )
         else na "rule_Set_Permute"
    theRule _ = na "rule_Set_Permute"


rule_Relation_Permute :: Rule
rule_Relation_Permute = "relation-permute" `namedRule` theRule where
    theRule [essence| permute(&perm, &y) |]  = do
      case y of WithLocals{} -> na "bubble-delay" ; _ -> return () 
      ts@(TypeRelation _) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if not $ typesUnify [ts, inn]
         then do
           ds <- domainOf y
           return
               ( "Horizontal rule for permute relation in comprehension"
               , do
                 (dPat, d) <- quantifiedVar
                 (pyName, py) <- auxiliaryVar
                 return $ WithLocals
                          [essence| &py |]
                        (AuxiliaryVars
                          [ Declaration (FindOrGiven LocalFind pyName ds)
                          , SuchThat
                            [ [essence|
                                    |&y| = |&py|
                                 /\ and([permute(&perm, &d) in &py | &dPat <- &y])

                              |]
                            ]
                          ]
                        )
               )
         else na "rule_Relation_Permute"
    theRule _ = na "rule_Relation_Permute"

rule_Relation_Permute_Comprehension :: Rule
rule_Relation_Permute_Comprehension = "relation-permute-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, perm, y), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
         Generator (GenInExpr pat [essence| permute(&perm, &y) |]) -> return (pat, perm, y)
         _ -> na "rule_Relation_Permute_Comprehension"
      case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
      ts@(TypeRelation _) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if not $ typesUnify [ts, inn]
         then do
           ds <- domainOf y
           return
               ( "Horizontal rule for permute relation in comprehension"
               , do
                 (dPat, d) <- quantifiedVar
                 (pyName, py) <- auxiliaryVar
                 return $ WithLocals
                        (Comprehension body $ gocBefore
                                          ++ [Generator (GenInExpr pat [essence| &py |])]
                                          ++ gocAfter)
                        (AuxiliaryVars
                          [ Declaration (FindOrGiven LocalFind pyName ds)
                          , SuchThat
                            [ [essence|
                                    |&y| = |&py|
                                 /\ and([permute(&perm, &d) in &py | &dPat <- &y])
                              |]
                            ]
                          ]
                        )
               )
         else na "rule_Relation_Permute_Comprehension"
    theRule _ = na "rule_Relation_Permute_Comprehension"


rule_Tuple_Permute :: Rule
rule_Tuple_Permute = "tuple-permute" `namedRule` theRule where
    theRule [essence| permute(&perm, &y) |]  = do
      case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
      ty' <- typeOf y
--      traceM $ "rule_Tuple_Permute: " ++ show ty'
      ty@(TypeTuple it) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if not $ typesUnify [ty, inn]
        then do
--          traceM $ "rule_Tuple_Permute: applying" 
          dm <- domainOf y
          return
              ( "Horizontal rule for permute tuple in comprehension"
              , do
                (pyName, py) <- auxiliaryVar
                return $ WithLocals
                          [essence| &py |]
                       (AuxiliaryVars $ 
                         [ Declaration (FindOrGiven LocalFind pyName dm)]
                         ++ ((\x -> let d = Constant $ ConstantInt NoTag x
                                       in SuchThat [[essence|  &py[&d] = permute(&perm,&y[&d]) |] ])
                                       <$> [1..(genericLength it)])

                        
                       )
              )
        else na "rule_Tuple_Permute"
    theRule _ = na "rule_Tuple_Permute"

rule_Tuple_Permute_Comprehension :: Rule
rule_Tuple_Permute_Comprehension = "tuple-permute-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, perm, y), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
         Generator (GenInExpr pat [essence| permute(&perm, &y) |]) -> return (pat, perm, y)
         _ -> na "rule_Tuple_Permute"
      case y of WithLocals{} -> na "bubble-delay" ; _ -> return ()
      ty' <- typeOf y
--      traceM $ "rule_Tuple_Permute_Comprehension: " ++ show ty'
      ty@(TypeTuple it) <- typeOf y
      (TypePermutation inn) <- typeOf perm
      if not $ typesUnify [ty, inn]
        then do
--          traceM $ "rule_Tuple_Permute_Comprehension: applying" 
          dm <- domainOf y
          return
              ( "Horizontal rule for permute tuple in comprehension"
              , do
                (pyName, py) <- auxiliaryVar
                return $ WithLocals
                      (Comprehension body $ gocBefore
                                        ++ [Generator (GenInExpr pat [essence| &py |])]
                                        ++ gocAfter)
                       (AuxiliaryVars $ 
                         [ Declaration (FindOrGiven LocalFind pyName dm)]
                         ++ ((\x -> let d = Constant $ ConstantInt NoTag x
                                       in SuchThat [[essence|  &py[&d] = permute(&perm,&y[&d]) |] ])
                                       <$> [1..(genericLength it)])
                       )
              )
        else na "rule_Tuple_Permute_Comprehension"
    theRule _ = na "rule_Tuple_Permute_Comprehension"



--rule_Function_Permute :: Rule
--rule_Partition_Permute :: Rule
--rule_MSet_Permute :: Rule
--rule_Sequence_Permute :: Rule
