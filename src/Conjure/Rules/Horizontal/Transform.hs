{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Horizontal.Transform where
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
             then na "rule_Image"
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
    bug "rule_Image_Comprehension: clonePattern: unsupported Abstract Pattern"

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


rule_Lift_Transformed_Matrix_Indexing :: Rule
rule_Lift_Transformed_Matrix_Indexing = "lift-transformed-matrix-indexing" `namedRule` theRule where
  matchMatrixIndexing :: (?typeCheckerMode::TypeCheckerMode)
                      => Expression
                      -> Maybe (Expression, Expression, Expression, Expression) 
  matchMatrixIndexing exp = do
    (matexp, indexer)     <- match opIndexing exp 
    (morphism, mat) <- match opTransform matexp
    TypeMatrix{} <- typeOf mat
    return (exp, morphism, mat, indexer)

  liftMatrixIndexing (exp, morphism, mat, indexer) = do
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
    let matched = catMaybes [matchMatrixIndexing exp | exp <- universeBi body]
    case matched of
      [] -> na "rule_Lift_Transformed_Matrix_Indexing: nothing to lift"
      _ -> do 
        replacements <- sequence (liftMatrixIndexing <$> matched)
        return ( "Horizontal rule for lift transformed matrix indexing"
               , return (Comprehension (transformBody body (fst <$> replacements)) $
                         gensOrConds ++ (snd <$> replacements))
               )
  theRule _ = na "rule_Lift_Transformed_Matrix_Indexing"

rule_Transform_Matrix_Indexing :: Rule
rule_Transform_Matrix_Indexing = "transform-matrix-indexing" `namedRule` theRule where
  theRule (Comprehension body gensOrConds) = do
    (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $  \ goc -> case goc of
         Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
         _ -> na "rule_Transform_Matrix_Indexing"
    (morphism, matexp) <- match opTransform exp
    (mat, indexer)     <- match opIndexing matexp
    ty <- typeOf mat
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
                 ++ [ComprehensionLetting mName [essence| &matexp[&i] |]]

                 ++ [ComprehensionLetting pat [essence| transform(&morphism, &m) |]]
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
    ty <- typeOf matexp
    DomainMatrix domIndx _ <- domainOf matexp
    inn <- morphing =<< typeOf morphism 
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

