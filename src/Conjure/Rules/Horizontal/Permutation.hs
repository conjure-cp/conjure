{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Horizontal.Permutation where
import Conjure.Rules.Import
import Data.Permutation (size, fromCycles, toFunction)

rule_Cardinality_Literal :: Rule
rule_Cardinality_Literal = "permutation-cardinality-literal" `namedRule` theRule where
  theRule p' = do
    p                              <- match opTwoBars p'
    (TypePermutation _, elems) <- match permutationLiteral p 
    let i' = Constant . ConstantInt TagInt . fromIntegral . size <$> fromCycles elems
    case i' of
      Left er -> fail $ "Permutation literal invalid." <++> stringToDoc (show er)
      Right i -> return
        ( "Vertical rule for permutation cardinality, AsFunction representation."
        , do
           return [essence| &i |]
        )

rule_Equality :: Rule
rule_Equality = "permutation-equality" `namedRule` theRule where
  theRule e = do
    (p,q)  <- match opEq e
    TypePermutation{} <- typeOf p
    TypePermutation{} <- typeOf q
    return ( "Horizontal rule for permutation equality"
           , return [essence| toSet(&p) = toSet(&q) |]
           )


rule_Comprehension :: Rule
rule_Comprehension = "permutation-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) =  do
        (gocBefore, (pat, perm), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat expr) -> return (pat, matchDefs [opToSet] expr)
            _ -> na "rule_Comprehension"
        (TypePermutation inner, elems) <- match permutationLiteral perm 
        DomainPermutation _ _ innerD <- domainOf perm 
        let f' = toFunction <$> fromCycles elems 
        case f' of
          Left er -> fail $ "Permutation literal invalid." <++> stringToDoc (show er)
          Right f -> do 
            let outLiteral = make matrixLiteral
                    (TypeMatrix (TypeInt TagInt) (TypeTuple [inner,inner])) innerD  
                                   [ AbstractLiteral (AbsLitTuple [de
                                                                  ,f de])
                                   | de <- join elems 
                                   ]
            return
              ( "Vertical rule for permutation-comprehension"
              , do
                  return $ Comprehension body 
                          $  gocBefore
                          ++ [ Generator (GenInExpr pat [essence| &outLiteral|])
                             ]
                          ++ gocAfter 
              )
    theRule _ = na "rule_Comprehension"

rule_Image_Literal :: Rule
rule_Image_Literal = "permutation-image-literal" `namedRule` theRule where
  theRule [essence| image(&p, &i) |] = do
    (TypePermutation inner, elems) <- match permutationLiteral p
    typeI <- typeOf i
    case typeI of TypeList{} -> na "list is a special case" ; _ -> return ()
    case typeI of
      TypeTuple tint -> do
        let tupleIndexImage indx = let indexexpr = Constant (ConstantInt TagInt indx)
                                   in [essence| image(&p, &i[&indexexpr]) |]
            tupleExpression = AbstractLiteral $ AbsLitTuple
                            $ (tupleIndexImage <$> [1..(fromIntegral $ length tint)])
        return
            ( "Horizontal rule for image of tuple under permutation"
            , return tupleExpression 
            )
      _ -> do
        let f' = toFunction <$> fromCycles elems 
        case f' of
          Left er -> fail $ "Permutation literal invalid." <++> stringToDoc (show er)
          Right f -> do 
            if let ?typeCheckerMode = StronglyTyped in typesUnify [inner, typeI] 
              then do
                let srtdel = sortBy compare (join elems) 
                    inperm = (\x -> [essence| toInt(&x) + 1 |])
                             ((\o -> [essence| or(&o) |])
                             ((fromList ((\q -> [essence| &q = &i |]) <$> srtdel))))
                    indexr = (\x -> [essence| sum(&x) |]) 
                             (fromList ((\(n,q) -> [essence| toInt(&q = &i) * &n |])
                              <$> (zip [1..] srtdel)))
                    matIdx = mkDomainIntB (fromInt 1)
                                          (fromInt (fromIntegral (length srtdel)))
                    matLit = make matrixLiteral (TypeMatrix (TypeInt TagInt) inner)
                                                 matIdx (f <$> srtdel)
                return
                   ( "Horizontal rule for permutation literal application to a single value (image), AsFunction representation"
                   , do
                     return [essence| [&i, catchUndef(&matLit[&indexr],0)][&inperm] |]
    
                   )
              else if let ?typeCheckerMode = StronglyTyped in typeI `containsType` inner
                     then na "rule_Image_Literal"
                     else return ( "Horizontal rule for permutation application to a type the permutation doesn't care about"
                                 , do
                                   return [essence| &i |]
                                 )
  theRule _ = na "rule_Image_Literal"


rule_In :: Rule
rule_In = "permutation-in" `namedRule` theRule where
    theRule p = do
        (x,s)     <- match opIn p
        TypePermutation{} <- typeOf s
        -- do not apply this rule to quantified variables
        -- or else we might miss the opportunity to apply a more specific vertical rule
        if referenceToComprehensionVar s
            then na "rule_In"
            else return ()
        return
            ( "Horizontal rule for permutation-in."
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| exists &iPat in &s . &i = &x |]
            )

rule_Permutation_Inverse :: Rule
rule_Permutation_Inverse = "permutation-inverse" `namedRule` theRule where
    theRule [essence| inverse(&p1, &p2)|] = do
        TypePermutation{}                 <- typeOf p1
        TypePermutation{}                 <- typeOf p2
        return
            ( "Vertical rule for permutation-inverse"
            , do
                (iPat, i) <- quantifiedVar
                return [essence|
                        (forAll &iPat in &p1 . image(&p2,&i[2]) = &i[1])
                            /\
                        (forAll &iPat in &p2 . image(&p1,&i[2]) = &i[1])
                      |] 
            )
    theRule _ = na "rule_Permutation_Inverse"

rule_Compose_Image :: Rule
rule_Compose_Image = "permutation-compose-image" `namedRule` theRule where
  theRule [essence| image(compose(&g, &h),&i) |] = do
    TypePermutation innerG <- typeOf g
    TypePermutation innerH <- typeOf g
    typeI <- typeOf i
    if let ?typeCheckerMode = StronglyTyped in typesUnify [innerG, innerH, typeI]
       then return
            ( "Horizontal rule for image of permutation composition"
            , do
              return [essence| image(&g, image(&h,&i)) |]
            )
       else na "rule_Compose_Image"
  theRule _ = na "rule_Compose_Image"

rule_Image_Comprehension :: Rule
rule_Image_Comprehension = "comprehension-image" `namedRule` theRule where
  theRule x = do
    (perm, Comprehension body gensOrConds) <- match opImage x
    TypePermutation{} <- typeOf perm
    return ( "Horizontal rule for image of comprehension"
           , do
               gox <- sequence (permutationOverGenOrCond perm <$> gensOrConds)        
               return $ Comprehension [essence| image(&perm, &body) |] (join gox) 
           )

 -- permutationOverGenOrCond :: Expression
 --                          -> GeneratorOrCondition 
 --                          -> m [GeneratorOrCondition]
  permutationOverGenOrCond p (Generator g) = permutationOverGenerator p g
  permutationOverGenOrCond p (Condition e) = return [Condition [essence| image(&p,&e) |]]
  permutationOverGenOrCond p (ComprehensionLetting n e) =
    return [ComprehensionLetting n [essence| image(&p,&e) |]]

--  permutationOverGenerator :: Expression
--                           -> Generator
--                           -> m [GeneratorOrCondition]
  permutationOverGenerator p (GenDomainHasRepr a d) = do
    (Single nm, n) <- quantifiedVarOverDomain $ forgetRepr d
    return [Generator (GenDomainHasRepr nm d)
           ,ComprehensionLetting a [essence| image(&p, &n) |]
           ]
  permutationOverGenerator p (GenInExpr a e) =
    return [Generator (GenInExpr a [essence| image(&p,&e) |])]
  permutationOverGenerator p (GenDomainNoRepr absPat d) = do
    (rPat, ns) <- clonePattern absPat
    return $ [Generator (GenDomainNoRepr rPat d)]
           ++ ((\(pat,exp) ->
                  ComprehensionLetting pat [essence| image(&p,&exp) |]
               ) <$> ns)

--  clonePattern :: AbstractPattern
--               -> m (AbstractPattern, [(Namr, Expression)])
  clonePattern (Single name) = do
    (nPat, n) <- quantifiedVar
    return (nPat,[(name, n)])
  clonePattern (AbsPatTuple pats) = do
    rec <- sequence (clonePattern <$> pats)
    return ( AbsPatTuple $ fst <$> rec
           , join $ snd <$> rec)
  clonePattern (AbsPatMatrix pats) = do
    rec <- sequence (clonePattern <$> pats)
    return ( AbsPatMatrix $ fst <$> rec
           , join $ snd <$> rec)
  clonePattern (AbsPatSet pats) = do
    rec <- sequence (clonePattern <$> pats)
    return ( AbsPatSet $ fst <$> rec
           , join $ snd <$> rec)
  clonePattern _ =
    bug "rule_Image_Comprehension: clonePattern: unsupported Abstract Pattern"
    

   


 


rule_Image_Comprehendable :: Rule
rule_Image_Comprehendable = "comprehendable-image" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
         _ -> na "rule_Image_Comprehendable"
    (perm, y) <- match opImage x 
    ty <- typeOf y
    case ty of TypeSequence{} -> na "sequence is a special case" ; _ -> return ()
    case ty of TypePartition{} -> na "partition is a special case" ; _ -> return ()
    case ty of TypeList{} -> na "list is a special case" ; _ -> return ()
    (TypePermutation inn) <- typeOf perm
    if let ?typeCheckerMode = StronglyTyped in ty `containsTypeComprehendable` inn
       then do
         return
             ( "Horizontal rule for image of comprehendable under permutation"
             , do
               (dPat, d) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr dPat [essence| &y |])]
                 ++ ((ComprehensionLetting pat [essence| image(&perm, &d) |] ):gocAfter)
                      )
                      
             )
       else na "rule_Image_Comprehendable"
  theRule _ = na "rule_Image_Comprehendable"

rule_Image_Sequence_Literal :: Rule
rule_Image_Sequence_Literal = "image-permutation-sequence-literal" `namedRule` theRule where
    theRule expr = do
        (perm,seq) <- match opImage expr
        (TypeSequence _, elems) <- match sequenceLiteral seq
        (TypePermutation _) <- typeOf perm
        let outLiteral = AbstractLiteral $ AbsLitSequence [ [essence| image(&perm,&e) |] | e <- elems ]
        return
            ( "Comprehension on permutation image of sequence literals"
            , return [essence| &outLiteral |] 
            )



rule_Image_Sequence :: Rule
rule_Image_Sequence = "image-sequence" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
         _ -> na "rule_Image_Sequence"
    (perm, y) <- match opImage x 
    ty <- typeOf y
    case ty of TypeSequence{} -> return () ; _ -> na "only applies to sequences" 
    (TypePermutation inn) <- typeOf perm
    if let ?typeCheckerMode = StronglyTyped in ty `containsTypeComprehendable` inn
       then do
         return
             ( "Horizontal rule for image of sequence under permutation"
             , do
               (dPat, d) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr dPat [essence| &y |])]
                 ++ ((ComprehensionLetting pat [essence| (&d[1],image(&perm, &d[2])) |] ):gocAfter)
                      )
                      
             )
       else na "rule_Image_Sequence"
  theRule _ = na "rule_Image_Sequence"


rule_Image_Sequence_Defined :: Rule
rule_Image_Sequence_Defined = "image-sequence-defined" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
         _ -> na "rule_Image_Sequence_Defined"
    defi <- match opDefined x
    (perm, y) <- match opImage defi 
    ty <- typeOf y
    case ty of TypeSequence{} -> return () ; _ -> na "only applies to sequences" 
    (TypePermutation inn) <- typeOf perm
    if let ?typeCheckerMode = StronglyTyped in ty `containsTypeComprehendable` inn
       then do
         return
             ( "Horizontal rule for image of sequence defined under permutation"
             , do
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr pat [essence| defined(&y) |])]
                 ++ gocAfter 
                      )
             )
       else na "rule_Image_Sequence_Defined"
  theRule _ = na "rule_Image_Sequence_Defined"


rule_Image_Partition :: Rule
rule_Image_Partition = "image-partition" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
         _ -> na "rule_Image_Partition"
    z <- match opParts x
    (perm, y) <- match opImage z 
    ty <- typeOf y
    case ty of TypePartition{} -> return () ; _ -> na "only applies to partitions" 
    (TypePermutation inn) <- typeOf perm
    if let ?typeCheckerMode = StronglyTyped in ty `containsTypeComprehendable` inn
       then do
         return
             ( "Horizontal rule for image of partition under permutation"
             , do
               (dPat, d) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr dPat [essence| parts(&y) |])]
                 ++ ((ComprehensionLetting pat [essence| image(&perm, &d) |] ):gocAfter)
                      )
                      
             )
       else na "rule_Image_Partition"
  theRule _ = na "rule_Image_Partition"





rule_Image_Incomprehendable :: Rule
rule_Image_Incomprehendable = "comprehendable-image" `namedRule` theRule where
  theRule [essence| image(&p, &i) |] = do 
    (TypePermutation inn) <- typeOf p
    ti <- typeOf i
    if let ?typeCheckerMode = StronglyTyped in ti `containsTypeIncomprehendable` inn
       then case ti of
         (TypeTuple tint) -> do
           let tupleIndexImage indx = let indexexpr = Constant (ConstantInt TagInt indx)
                                      in [essence| image(&p, &i[&indexexpr]) |]
               tupleExpression = AbstractLiteral $ AbsLitTuple
                               $ (tupleIndexImage <$> [1..(fromIntegral $ length tint)])
           return
               ( "Horizontal rule for image of tuple under permutation"
               , return tupleExpression 
               )
         (TypeRecord namet) -> do
           let names = fst <$> namet
               recordIndexImage indx = let indexexpr = patternToExpr $ Single indx
                                       in (indx, [essence| image(&p, &i[&indexexpr]) |])
               recordExpression = AbstractLiteral $ AbsLitRecord
                                $ (recordIndexImage <$> names)
           return
               ( "Horizontal rule for image of record under permutation"
               , return recordExpression
               ) 
         (TypeVariant _) ->
           bug "rule_Image_Incomprehendable not implemented for Variant" 
         _ -> bug "rule_Image_Incomprehendable this is a bug"
       else na "rule_Image_Comprehendable"
  theRule _ = na "rule_Image_Comprehendable"

----------------------------------------------------------------------------------

rule_Image_Matrix_Indexing :: Rule
rule_Image_Matrix_Indexing = "image-matrix-indexing" `namedRule` theRule where
  theRule p = do 
    (matexp, indexer) <- match opIndexing p
    (perm, mat)       <- match opImage matexp
    ty <- typeOf mat
    case ty of TypeMatrix{} -> return () ; _ -> na "only applies to matrices" 
    (TypePermutation inn) <- typeOf perm
    if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn 
       then do
         return
             ( "Horizontal rule for image of matrix under permutation"
             , return $ [essence| image(&perm, &mat[image(&perm, &indexer)]) |]                        )
       else na "rule_Image_Matrix_Indexing"


--TODO don't need?
rule_Image_Matrix_Indexing_Comprehension :: Rule
rule_Image_Matrix_Indexing_Comprehension = "image-matrix-indexing-comprehension" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
         _ -> na "rule_Image_Matrix_Indexing_Comprehension"
    (matexp, indexer) <- match opIndexing x
    (perm, mat)       <- match opImage matexp
    ty <- typeOf mat
    case ty of TypeMatrix{} -> return () ; _ -> na "only applies to matrices" 
    (TypePermutation inn) <- typeOf perm
    if let ?typeCheckerMode = StronglyTyped in ty `containsTypeComprehendable` inn
       then do
         return
             ( "Horizontal rule for image of matrix under permutation"
             , do
               (dPat, d) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr dPat [essence| &mat[image(&perm, &indexer)] |])]
                 ++ [ComprehensionLetting pat [essence| image(&perm, &d) |]]
                 ++ gocAfter)
             )
       else na "rule_Image_Matrix_Indexing_Comprehension"

  theRule _ = na "rule_Image_Matrix_Indexing_Comprehension"


