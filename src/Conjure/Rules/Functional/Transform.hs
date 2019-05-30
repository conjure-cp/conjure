{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Functional.Transform where
import Conjure.Rules.Import

rule_Transform_Unifying :: Rule
rule_Transform_Unifying = "transform-unifying" `namedRule` theRule where
  theRule [essence| transform(&morphism, &i) |] = do
    inner <- morphing =<< typeOf morphism
    typeI <- typeOf i
    if let ?typeCheckerMode = StronglyTyped in typesUnify [inner, typeI]
      then return ( "Horizontal rule for transform unifying"
                  , return [essence| image(&morphism, &i) |]
                  )
      else if let ?typeCheckerMode = StronglyTyped in typeI `containsType` inner
             then na "rule_Transform_Unifying"
             else return ( "Horizontal rule for transform abort"
                         , do
                           return [essence| &i |]
                         )
  theRule _ = na "rule_Transform_Unifying"


rule_Transform_Functorially :: Rule
rule_Transform_Functorially = "transform-functorially" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) ->
           return (pat, matchDefs [opToSet, opToMSet] expr)
         _ -> na "rule_Transform_Functorially"
    (morphism, y) <- match opTransform x
    ty <- typeOf y
    inn <- morphing =<< typeOf morphism 
    if let ?typeCheckerMode = StronglyTyped in ty `containsTypeFunctorially` inn
       then do
         return
             ( "Horizontal rule for transform of functorially"
             , do
               (dPat, d) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr dPat [essence| &y |])]
                 ++ ((ComprehensionLetting pat [essence|
                              transform(&morphism, &d) |] ):gocAfter)
                      )
             )
       else na "rule_Transform_Functorially"
  theRule _ = na "rule_Transform_Functorially"

--TODO early abort via traversal
rule_Transform_Comprehension :: Rule
rule_Transform_Comprehension = "transform-comprehension" `namedRule` theRule where
  theRule x = do
    (morphism, Comprehension body gensOrConds) <- match opTransform x
    return ( "Horizontal rule for transform comprehension"
           , do
               gox <- sequence (transformOverGenOrCond morphism <$> gensOrConds)
               return $ Comprehension [essence| transform(&morphism, &body) |] (join gox)
           )
  transformOverGenOrCond m (Generator g) = transformOverGenerator m g
  transformOverGenOrCond m (Condition e) =
    return [Condition [essence| transform(&m,&e) |]]
  transformOverGenOrCond m (ComprehensionLetting n e) =
    return [ComprehensionLetting n [essence| transform(&m,&e) |]]

  transformOverGenerator m (GenDomainHasRepr a d) = do
    (Single nm, n) <- quantifiedVarOverDomain $ forgetRepr d
    return [Generator (GenDomainHasRepr nm d)
           ,ComprehensionLetting a [essence| transform(&m, &n) |]
           ]
  transformOverGenerator m (GenInExpr a e) =
    return [Generator (GenInExpr a [essence| transform(&m,&e) |])]
  transformOverGenerator m (GenDomainNoRepr absPat d) = do
    (rPat, ns) <- clonePattern absPat
    return $ [Generator (GenDomainNoRepr rPat d)]
           ++ ((\(pat,exp) ->
                  ComprehensionLetting pat [essence| transform(&m,&exp) |]
               ) <$> ns)

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
    bug "rule_Transform_Comprehension: clonePattern: unsupported Abstract Pattern"

rule_Transformed_Matrix_Indexing :: Rule
rule_Transformed_Matrix_Indexing = "transformed-matrix-indexing" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
         _ -> na "rule_Transformed_Matrix_Indexing"
    (matexp, indexer)     <- match opIndexing exp 
    (morphism, mat) <- match opTransform matexp
    ty <- typeOf mat
    inn <- morphing =<< typeOf morphism 
    if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
       then do
         return
             ( "Horizontal rule for transformed matrix indexing"
             , do
               (Single mName, m) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [ComprehensionLetting mName [essence| &matexp[&indexer] |]]

                 ++ [ComprehensionLetting pat [essence| transform(&morphism, &m) |]]
                 ++ gocAfter)
             )
       else na "rule_Transformed_Matrix_Indexing"
  theRule _ = na "rule_Transformed_Matrix_Indexing"


rule_Lift_Transformed_Indexing :: Rule
rule_Lift_Transformed_Indexing = "lift-transformed-indexing" `namedRule` theRule where
  matchIndexing :: (?typeCheckerMode::TypeCheckerMode)
                      => Expression
                      -> Maybe (Expression, Expression, Expression, Expression) 
  matchIndexing exp = do
    (matexp, indexer)     <- match opIndexing exp 
    (morphism, mat) <- match opTransform matexp
    return (exp, morphism, mat, indexer)

  liftIndexing (exp, morphism, mat, indexer) = do
    (Single nm, m) <- quantifiedVar
    return ( (exp, [essence| transform(&morphism, &m) |])
           , ComprehensionLetting nm [essence| &mat[&indexer] |])

  transformBody bdy [] = bdy
  transformBody bdy ((orig, repl):rest) = 
    let nbdy = transformBi (\e -> if e == orig 
                                     then repl
                                     else e) bdy
    in transformBody nbdy rest

  theRule (Comprehension body gensOrConds) = do
    let matched = catMaybes [matchIndexing exp | exp <- universeBi body]
    case matched of
      [] -> na "rule_Lift_Transformed_Indexing: nothing to lift"
      _ -> do 
        replacements <- sequence (liftIndexing <$> matched)
        return ( "Horizontal rule for lift transformed indexing"
               , return (Comprehension (transformBody body (fst <$> replacements)) $
                         gensOrConds ++ (snd <$> replacements))
               )
  theRule _ = na "rule_Lift_Transformed_Indexing"

rule_Transform_Matrix_Indexing :: Rule
rule_Transform_Matrix_Indexing = "transform-matrix-indexing" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr pat expr) -> return (pat, expr)
         _ -> na "rule_Transform_Matrix_Indexing"
    (morphism, matexp) <- match opTransform exp
    (mat, indexer)     <- match opIndexing matexp
    ty <- typeOf mat
--    ty@TypeMatrix{} <- typeOf mat
    inn <- morphing =<< typeOf morphism 
    if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
       then do
         return
             ( "Horizontal rule for transform matrix indexing"
             , do
               (Single mName, m) <- quantifiedVar
               (Single iName, i) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [ComprehensionLetting iName [essence| transform(&morphism, &indexer) |]]
                 ++ [ComprehensionLetting mName [essence| &mat[&i] |]]

                 ++ [Generator (GenInExpr pat [essence| transform(&morphism, &m) |])]
                 ++ gocAfter)
             )
       else na "rule_Transform_Matrix_Indexing"
  theRule _ = na "rule_Transform_Matrix_Indexing"


--TODO early abort via traversal
rule_Transform_Matrix :: Rule
rule_Transform_Matrix = "transform-matrix" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
         _ -> na "rule_Transform_Matrix"
    (morphism, matexp) <- match opTransform exp
--    ty <- typeOf matexp
    DomainMatrix domIndx _ <- domainOf matexp
--    inn <- morphing =<< typeOf morphism 
    return
        ( "Horizontal rule for transform matrix in comprehension generator"
        , do
          (dPat, d) <- quantifiedVar
          (Single mName, m) <- quantifiedVar
          (Single iName, i) <- quantifiedVar
          return (Comprehension body $
                gocBefore
            ++ [Generator (GenDomainNoRepr dPat (forgetRepr domIndx))]
            ++ [ComprehensionLetting iName [essence| transform(&morphism, &d) |]]
            ++ [ComprehensionLetting mName [essence| &matexp[&i] |]]
            ++ [ComprehensionLetting pat [essence| transform(&morphism, &m) |]]
            ++ gocAfter)
        )
  theRule _ = na "rule_Transform_Matrix"

rule_Transform_Sum_Product :: Rule
rule_Transform_Sum_Product = "transform-sum-product" `namedRule` theRule where
  theRule [essence| transform(&morphism, &i) |] = do
    inn <- morphing =<< typeOf morphism 
    ti <- typeOf i
    if let ?typeCheckerMode = StronglyTyped in ti `containsSumProductType` inn
       then case ti of
         (TypeTuple tint) -> do
           let tupleIndexTransform indx =
                 let indexexpr = Constant (ConstantInt TagInt indx)
                 in [essence| transform(&morphism, &i[&indexexpr]) |]
               tupleExpression =
                 AbstractLiteral $ AbsLitTuple
                 $ (tupleIndexTransform <$> [1..(fromIntegral $ length tint)])
           return
               ( "Horizontal rule for transform of tuple"
               , return tupleExpression
               )
         (TypeRecord namet) -> do
           let recordIndexTransform indx =
                 let indexexpr = Reference (fst indx)
                               $ Just $ RecordField (fst indx) (snd indx) 
                 in (fst indx, [essence| transform(&morphism, &i[&indexexpr]) |])
               recordExpression = AbstractLiteral $ AbsLitRecord
                                $ (recordIndexTransform <$> namet)
           return
               ( "Horizontal rule for transform of record"
               , return recordExpression
               )
         (TypeVariant namet) -> do
           marker:vars <- downX1 i 
           
           bug  "rule_Transform_Sum_Product not implemented"
         _ -> bug "rule_Transform_Sum_Product this is a bug"
       else na "rule_Transform_Sum_Product"
  theRule _ = na "rule_Transform_Sum_Product"




rule_Transform_Sequence :: Rule
rule_Transform_Sequence = "transform-sequence" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) ->
           return (pat, matchDefs [opToSet, opToMSet] expr)
         _ -> na "rule_Transform_Sequence"
    (morphism, y) <- match opTransform x
    ty <- typeOf y
    case ty of TypeSequence{} -> return () ; _ -> na "only applies to sequences"
    inn <- morphing =<< typeOf morphism 
    if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
       then do
         return
             ( "Horizontal rule for transform of sequence"
             , do
               (dPat, d) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr dPat [essence| &y |])]
                 ++ ((ComprehensionLetting pat [essence|
                   (&d[1], transform(&morphism, &d[2])) |] ):gocAfter)
                      )

             )
       else na "rule_Transform_Sequence"
  theRule _ = na "rule_Transform_Sequence"



rule_Transform_Sequence_Defined :: Rule
rule_Transform_Sequence_Defined = "transform-sequence-defined" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr pat@Single{} expr) ->
           return (pat, matchDefs [opToSet, opToMSet] expr)
         _ -> na "rule_Transform_Sequence_Defined"
    defi <- match opDefined x
    (morphism, y) <- match opTransform defi
    ty <- typeOf y
    case ty of TypeSequence{} -> return () ; _ -> na "only applies to sequences"
    inn <- morphing =<< typeOf morphism
    if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
       then do
         return
             ( "Horizontal rule for transform of sequence defined"
             , do
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr pat [essence| defined(&y) |])]
                 ++ gocAfter
                      )
             )
       else na "rule_Transform_Sequence_Defined"
  theRule _ = na "rule_Transform_Sequence_Defined"



rule_Transform_Partition :: Rule
rule_Transform_Partition = "transform-partition" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
         _ -> na "rule_Transform_Partition"
    z <- match opParts x
    (morphism, y) <- match opTransform z
    ty <- typeOf y
    case ty of TypePartition{} -> return () ; _ -> na "only applies to partitions"
    inn <- morphing =<< typeOf morphism
    if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
       then do
         return
             ( "Horizontal rule for transform of partition"
             , do
               (dPat, d) <- quantifiedVar
               return (Comprehension body $
                     gocBefore
                 ++ [Generator (GenInExpr dPat [essence| parts(&y) |])]
                 ++ ((ComprehensionLetting pat [essence| transform(&morphism, &d) |] ):gocAfter)
                      )

             )
       else na "rule_Transform_Partition"
  theRule _ = na "rule_Transform_Partition"
