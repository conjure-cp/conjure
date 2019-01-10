{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Horizontal.Permutation where
import Conjure.Rules.Import
import Data.Permutation (size, fromCycles, toFunction)


rule_Cardinality_Literal :: Rule
rule_Cardinality_Literal = "permutation-cardinality-literal" `namedRule` theRule where
  theRule p' = do
    p                              <- match opTwoBars p'
    (TypePermutation _, elems) <- match permutationLiteral p 
    let i' = Constant . ConstantInt AnyTag . fromIntegral . size <$> fromCycles elems
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
                    (TypeMatrix (TypeInt AnyTag) (TypeTuple [inner,inner])) innerD  
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
    let f' = toFunction <$> fromCycles elems 
    case f' of
      Left er -> fail $ "Permutation literal invalid." <++> stringToDoc (show er)
      Right f -> do 
        typeI <- typeOf i
        if typesUnify [inner, typeI] 
          then do
            let srtdel = sortBy compare (join elems) 
                domIndx = reTag AnyTag $  mkDomainInt (RangeSingle <$> srtdel) 
                matLit = make matrixLiteral (TypeMatrix (TypeInt AnyTag) inner) domIndx ( f <$> srtdel)
            return
               ( "Horizontal rule for permutation literal application to a single value (image), AsFunction representation"
               , do
                 return [essence| [&i, catchUndef(&matLit[&i],0)][toInt(&i in toSet(&matLit))+1] |]

               )
          else if typeI `containsType` inner
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
        case p1 of WithLocals{} -> na "bubble-delay" ; _ -> return ()
        case p2 of WithLocals{} -> na "bubble-delay" ; _ -> return ()
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
--    case match permutationLiteral h of
--      Nothing -> return () --SR error when h is literal, fall back to rule_Compose 
--      Just _ -> na "rule_Compose_Image" 
    TypePermutation innerG <- typeOf g
    TypePermutation innerH <- typeOf g
    typeI <- typeOf i
    if typesUnify [innerG, innerH, typeI]
       then return
            ( "Horizontal rule for image of permutation composition"
            , do
              return [essence| image(&g, image(&h,&i)) |]
            )
       else na "rule_Compose_Image"
  theRule _ = na "rule_Compose_Image"


rule_Compose :: Rule
rule_Compose = "permutation-compose" `namedRule` theRule where
  theRule [essence| compose(&g,&h) |] = do
    TypePermutation innerG <- typeOf g
    TypePermutation innerH <- typeOf h 
    dg <- domainOf g
    dh <- domainOf h
    if typesUnify [innerG, innerH]
      then do
        du <- domainUnion dg dh
        return ( "Horizontal rule for permutation composition"
               , do
                 
                 (lPat, l)  <- quantifiedVar
                 (pName, p) <- auxiliaryVar
                 return $ WithLocals
                            [essence| &p |]
                        ( AuxiliaryVars
                           [ (Declaration (FindOrGiven LocalFind pName du))
                           , SuchThat
                               [ [essence| 
                                    forAll &lPat in (defined(&g) union defined(&h)) .
                                      image(&p,&l) = image(&g,image(&h,&l))
                                 |]
                               ]
                           ]
                        )
               )
      else na "rule_Compose"
  theRule _ = na "rule_Compose"

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
    (TypePermutation inn) <- typeOf perm
    if ty `containsTypeComprehendable` inn
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
    if ty `containsTypeComprehendable` inn
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
    if ty `containsTypeComprehendable` inn
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
    if ty `containsTypeComprehendable` inn
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
    if ti `containsTypeIncomprehendable` inn
       then case ti of
         (TypeTuple tint) -> do
           let tupleIndexImage indx = let indexexpr = Constant (ConstantInt AnyTag indx)
                                      in [essence| image(&p, &i[&indexexpr]) |]
               tupleExpression = AbstractLiteral $ AbsLitTuple
                               $ (tupleIndexImage <$> [1..(fromIntegral $ length tint)])
           return
               ( "Horizontal rule for image of incomprehendable under permutation"
               , return tupleExpression 
               )
         (TypeRecord _) ->
           bug "rule_Image_Incomprehendable not implemented for Record"
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
    if ty `containsTypeComprehendable` inn
       then do
         return
             ( "Horizontal rule for image of matrix under permutation"
             , return $ [essence| image(&perm, &mat[image(&perm, &indexer)]) |]                        )
       else na "rule_Image_Matrix_Indexing"


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
    if ty `containsTypeComprehendable` inn
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


