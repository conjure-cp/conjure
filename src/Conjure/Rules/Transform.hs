{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Transform (rules_Transform) where

import Conjure.Rules.Import
import Conjure.Rules.Vertical.Variant (onTagged)

rules_Transform :: [Rule]
rules_Transform =
  [ rule_Transform_DotLess,
    rule_Transform_Sequence_Literal,
    rule_Transform_Functorially,
    rule_Transform_Comprehension,
    rule_Transform_Product_Types,
    rule_Transform_Matrix,
    rule_Transform_Partition,
    rule_Transform_Sequence,
    rule_Transform_Sequence_Defined,
    rule_Transformed_Indexing,
    rule_Lift_Transformed_Indexing,
    rule_Transform_Indexing,
    rule_Transform_Unifying,
    rule_Transform_Variant_Literal,
    rule_Transform_Variant_Eq,
    rule_Transform_Variant_Neq,
    rule_Transform_Variant_Lt,
    rule_Transform_Variant_Leq,
    rule_Transformed_Variant_Index,
    rule_Transformed_Variant_Active
  ]

rule_Transform_DotLess :: Rule
rule_Transform_DotLess = "transform-dotless" `namedRule` theRule
  where
    theRule [essence| &x .<= transform(&p, &y) |] | x == y = do
      TypeMatrix {} <- typeOf x
      (xInd : _) <- indexDomainsOf x
      return
        ( "",
          do
            (iPat, i) <- quantifiedVar
            return [essence| &x .<= [ transform(&p, &x[transform(permInverse(&p), &i)]) | &iPat : &xInd ] |]
        )
    theRule _ = na "rule_Transform_DotLess"

rule_Transform_Functorially :: Rule
rule_Transform_Functorially = "transform-functorially" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) ->
          return (pat, matchDefs [opToSet, opToMSet] expr)
        _ -> na "rule_Transform_Functorially"
      (morphism, y) <- match opTransform x
      ty <- typeOf y
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsTypeFunctorially` inn
        then
          return
            ( "Horizontal rule for transform of functorially",
              do
                (dPat, d) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenInExpr dPat y)]
                      ++ ( ComprehensionLetting
                             (Single pat)
                             [essence| transform(&morphism, &d) |]
                             : gocAfter
                         )
                  )
            )
        else na "rule_Transform_Functorially"
    theRule _ = na "rule_Transform_Functorially"

rule_Transform_Comprehension :: Rule
rule_Transform_Comprehension = "transform-comprehension" `namedRule` theRule
  where
    theRule x = do
      (morphism, cmp@(Comprehension body gensOrConds)) <- match opTransform x
      ty <- typeOf cmp
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then
          return
            ( "Horizontal rule for transform comprehension",
              do
                gox <- sequence (transformOverGenOrCond morphism <$> gensOrConds)
                return
                  $ Comprehension
                    [essence| transform(&morphism, &body) |]
                    (join gox)
            )
        else na "rule_Transform_Comprehension"
    transformOverGenOrCond m (Generator g) = transformOverGenerator m g
    transformOverGenOrCond m (Condition e) =
      return [Condition [essence| transform(&m,&e) |]]
    transformOverGenOrCond m (ComprehensionLetting pat e) =
      return [ComprehensionLetting pat [essence| transform(&m,&e) |]]

    transformOverGenerator m (GenDomainHasRepr a d) = do
      (Single nm, n) <- quantifiedVarOverDomain $ forgetRepr d
      return
        [ Generator (GenDomainHasRepr nm d),
          ComprehensionLetting (Single a) [essence| transform(&m, &n) |]
        ]
    transformOverGenerator m (GenInExpr a e) =
      return [Generator (GenInExpr a [essence| transform(&m,&e) |])]
    transformOverGenerator m (GenDomainNoRepr absPat d) = do
      (rPat, ns) <- clonePattern absPat
      return
        $ [Generator (GenDomainNoRepr rPat d)]
        ++ ( ( \(pat, exp) ->
                 ComprehensionLetting (Single pat) [essence| transform(&m,&exp) |]
             )
               <$> ns
           )

    clonePattern (Single name) = do
      (nPat, n) <- quantifiedVar
      return (nPat, [(name, n)])
    clonePattern (AbsPatTuple pats) = do
      rec <- sequence (clonePattern <$> pats)
      return
        ( AbsPatTuple $ fst <$> rec,
          join $ snd <$> rec
        )
    clonePattern (AbsPatMatrix pats) = do
      rec <- sequence (clonePattern <$> pats)
      return
        ( AbsPatMatrix $ fst <$> rec,
          join $ snd <$> rec
        )
    clonePattern (AbsPatSet pats) = do
      rec <- sequence (clonePattern <$> pats)
      return
        ( AbsPatSet $ fst <$> rec,
          join $ snd <$> rec
        )
    clonePattern _ =
      bug "rule_Transform_Comprehension: clonePattern: unsupported Abstract Pattern"

rule_Transform_Product_Types :: Rule
rule_Transform_Product_Types = "transform-product-types" `namedRule` theRule
  where
    theRule [essence| transform(&morphism, &i) |] = do
      inn <- morphing =<< typeOf morphism
      ti <- typeOf i
      if let ?typeCheckerMode = StronglyTyped in ti `containsProductType` inn
        then case ti of
          (TypeTuple tint) -> do
            let tupleIndexTransform indx =
                  let indexexpr = Constant (ConstantInt TagInt indx)
                   in [essence| transform(&morphism, &i[&indexexpr]) |]
                tupleExpression =
                  AbstractLiteral
                    $ AbsLitTuple
                    $ (tupleIndexTransform <$> [1 .. (fromIntegral $ length tint)])
            return
              ( "Horizontal rule for transform of tuple",
                return tupleExpression
              )
          (TypeRecord namet) -> do
            let recordIndexTransform indx =
                  let indexexpr =
                        Reference (fst indx)
                          $ Just
                          $ uncurry RecordField indx
                   in (fst indx, [essence| transform(&morphism, &i[&indexexpr]) |])
                recordExpression =
                  AbstractLiteral
                    $ AbsLitRecord
                    $ recordIndexTransform
                    <$> namet
            return
              ( "Horizontal rule for transform of record",
                return recordExpression
              )
          _ -> bug "rule_Transform_Product_Types this is a bug"
        else na "rule_Transform_Product_Types"
    theRule _ = na "rule_Transform_Product_Types"

rule_Transform_Matrix :: Rule
rule_Transform_Matrix = "transform-matrix" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
        _ -> na "rule_Transform_Matrix"
      (morphism, matexp) <- match opTransform exp
      DomainMatrix domIndx _ <- domainOf matexp
      ty <- typeOf matexp
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then
          return
            ( "Horizontal rule for transform matrix in comprehension generator",
              do
                (dPat, d) <- quantifiedVar
                (Single mName, m) <- quantifiedVar
                (Single iName, i) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenDomainNoRepr dPat (forgetRepr domIndx))]
                      ++ [ComprehensionLetting (Single iName) [essence| transform(&morphism, &d) |]]
                      ++ [ComprehensionLetting (Single mName) [essence| &matexp[&i] |]]
                      ++ [ComprehensionLetting (Single pat) [essence| transform(&morphism, &m) |]]
                      ++ gocAfter
                  )
            )
        else na "rule_Transform_Matrix"
    theRule _ = na "rule_Transform_Matrix"

rule_Transform_Partition :: Rule
rule_Transform_Partition = "transform-partition" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
        _ -> na "rule_Transform_Partition"
      z <- match opParts x
      (morphism, y) <- match opTransform z
      ty <- typeOf y
      case ty of TypePartition {} -> return (); _ -> na "only applies to partitions"
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform of partition",
              do
                (dPat, d) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenInExpr dPat [essence| parts(&y) |])]
                      ++ (ComprehensionLetting (Single pat) [essence| transform(&morphism, &d) |] : gocAfter)
                  )
            )
        else na "rule_Transform_Partition"
    theRule _ = na "rule_Transform_Partition"

rule_Transform_Sequence :: Rule
rule_Transform_Sequence = "transform-sequence" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) ->
          return (pat, matchDefs [opToSet, opToMSet] expr)
        _ -> na "rule_Transform_Sequence"
      (morphism, y) <- match opTransform x
      ty <- typeOf y
      case ty of TypeSequence {} -> return (); _ -> na "only applies to sequences"
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform of sequence",
              do
                (dPat, d) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenInExpr dPat y)]
                      ++ ( ( ComprehensionLetting
                               (Single pat)
                               [essence|
                   (&d[1], transform(&morphism, &d[2])) |]
                           )
                             : gocAfter
                         )
                  )
            )
        else na "rule_Transform_Sequence"
    theRule _ = na "rule_Transform_Sequence"

rule_Transform_Sequence_Defined :: Rule
rule_Transform_Sequence_Defined = "transform-sequence-defined" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, x), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr pat@Single {} expr) ->
          return (pat, matchDefs [opToSet, opToMSet] expr)
        _ -> na "rule_Transform_Sequence_Defined"
      defi <- match opDefined x
      (morphism, y) <- match opTransform defi
      ty <- typeOf y
      case ty of TypeSequence {} -> return (); _ -> na "only applies to sequences"
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform of sequence defined",
              do
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [Generator (GenInExpr pat [essence| defined(&y) |])]
                      ++ gocAfter
                  )
            )
        else na "rule_Transform_Sequence_Defined"
    theRule _ = na "rule_Transform_Sequence_Defined"

rule_Transformed_Indexing :: Rule
rule_Transformed_Indexing = "transformed-indexing" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, exp), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
        _ -> na "rule_Transformed_Indexing"
      (matexp, indexer) <- match opIndexing exp
      (morphism, mat) <- match opTransform matexp
      ty <- typeOf mat
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transformed indexing",
              do
                (Single mName, m) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [ComprehensionLetting (Single mName) [essence| &matexp[&indexer] |]]
                      ++ [ComprehensionLetting (Single pat) [essence| transform(&morphism, &m) |]]
                      ++ gocAfter
                  )
            )
        else na "rule_Transformed_Indexing"
    theRule _ = na "rule_Transformed_Indexing"

rule_Lift_Transformed_Indexing :: Rule
rule_Lift_Transformed_Indexing = "lift-transformed-indexing" `namedRule` theRule
  where
    theRule [essence| transform(&p, &x)[&i] |] = do
      TypePermutation {} <- typeOf p
      return
        ( "transformed indexing",
          return [essence| transform(&p, &x[transform(permInverse(&p), &i)]) |]
        )
    theRule _ = na "rule_Lift_Transformed_Indexing"

rule_Transform_Indexing :: Rule
rule_Transform_Indexing = "transform-indexing" `namedRule` theRule
  where
    theRule (Comprehension body gensOrConds) = do
      (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \case
        Generator (GenInExpr pat expr) -> return (pat, expr)
        _ -> na "rule_Transform_Indexing"
      (morphism, matexp) <- match opTransform expr
      (mat, indexer) <- match opIndexing matexp
      ty <- typeOf mat
      inn <- morphing =<< typeOf morphism
      if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
        then do
          return
            ( "Horizontal rule for transform indexing",
              do
                (Single mName, m) <- quantifiedVar
                (Single iName, i) <- quantifiedVar
                return
                  ( Comprehension body
                      $ gocBefore
                      ++ [ComprehensionLetting (Single iName) [essence| transform(&morphism, &indexer) |]]
                      ++ [ComprehensionLetting (Single mName) [essence| &mat[&i] |]]
                      ++ [Generator (GenInExpr pat [essence| transform(&morphism, &m) |])]
                      ++ gocAfter
                  )
            )
        else na "rule_Transform_Indexing"
    theRule _ = na "rule_Transform_Indexing"

rule_Transform_Unifying :: Rule
rule_Transform_Unifying = "transform-unifying" `namedRule` theRule
  where
    theRule [essence| transform(&morphism, &i) |] = do
      inner <- morphing =<< typeOf morphism
      typeI <- typeOf i
      if let ?typeCheckerMode = StronglyTyped in typesUnify [inner, typeI]
        then
          return
            ( "Horizontal rule for transform unifying",
              return [essence| image(&morphism, &i) |]
            )
        else
          if let ?typeCheckerMode = StronglyTyped in typeI `containsType` inner
            then na "rule_Transform_Unifying"
            else
              return
                ( "Horizontal rule for transform shortcut",
                  return i
                )
    theRule _ = na "rule_Transform_Unifying"

rule_Transform_Sequence_Literal :: Rule
rule_Transform_Sequence_Literal = "transform-sequence-literal" `namedRule` theRule
  where
    theRule p = do
      _ <- match opTransform p
      let (x, rx) = matchManyTransforms p
      TypeSequence {} <- typeOf x
      (_, as) <- match sequenceLiteral x
      return
        ( "Horizontal rule for transform sequence literal",
          return $ AbstractLiteral $ AbsLitSequence $ rx <$> as
        )

rule_Transform_Variant_Literal :: Rule
rule_Transform_Variant_Literal = "transform-variant-literal" `namedRule` theRule
  where
    theRule p = do
      _ <- match opTransform p
      let (x, rx) = matchManyTransforms p
      case x of
        AbstractLiteral (AbsLitVariant d n a) ->
          return
            ( "Horizontal rule for transform variant literal",
              return $ AbstractLiteral $ AbsLitVariant d n $ rx a
            )
        _ -> na "rule_Transform_Variant_Literal"

atLeastOneTransform :: (MonadFailDoc m) => (Expression, Expression) -> m ()
atLeastOneTransform (l, r) = do
  case (match opTransform l, match opTransform r) of
    (Nothing, Nothing) -> na "no transforms on either side"
    _ -> return ()

matchManyTransforms ::
  Expression ->
  (Expression, Expression -> Expression)
matchManyTransforms exp =
  case match opTransform exp of
    Nothing -> (exp, id)
    Just (morphism, so) ->
      let (nexp, ntrans) = matchManyTransforms so
       in ( nexp,
            \x -> let nx = ntrans x in [essence| transform(&morphism, &nx) |]
          )

rule_Transform_Variant_Eq :: Rule
rule_Transform_Variant_Eq = "transform-variant-eq" `namedRule` theRule
  where
    theRule p = do
      (l, r) <- match opEq p
      atLeastOneTransform (l, r)
      let (x, rx) = matchManyTransforms l
      let (y, ry) = matchManyTransforms r
      TypeVariant {} <- typeOf x
      TypeVariant {} <- typeOf y
      (xWhich : xs) <- downX1 x
      (yWhich : ys) <- downX1 y
      return
        ( "Vertical rule for right transformed variant equality",
          return
            $ make opAnd
            $ fromList
              [ [essence| &xWhich = &yWhich |],
                onTagged (make opEq) xWhich (rx <$> xs) (ry <$> ys)
              ]
        )

rule_Transform_Variant_Neq :: Rule
rule_Transform_Variant_Neq = "transform-variant-neq" `namedRule` theRule
  where
    theRule p = do
      (l, r) <- match opNeq p
      atLeastOneTransform (l, r)
      let (x, rx) = matchManyTransforms l
      let (y, ry) = matchManyTransforms r
      TypeVariant {} <- typeOf x
      TypeVariant {} <- typeOf y
      (xWhich : xs) <- downX1 x
      (yWhich : ys) <- downX1 y
      return
        ( "Vertical rule for right transformed variant nequality",
          return
            $ make opOr
            $ fromList
              [ [essence| &xWhich != &yWhich |],
                make opAnd
                  $ fromList
                    [ [essence| &xWhich = &yWhich |],
                      onTagged (make opNeq) xWhich (rx <$> xs) (ry <$> ys)
                    ]
              ]
        )

rule_Transform_Variant_Lt :: Rule
rule_Transform_Variant_Lt = "transform-variant-lt" `namedRule` theRule
  where
    theRule p = do
      (l, r) <- match opLt p
      atLeastOneTransform (l, r)
      let (x, rx) = matchManyTransforms l
      let (y, ry) = matchManyTransforms r
      TypeVariant {} <- typeOf x
      TypeVariant {} <- typeOf y
      (xWhich : xs) <- downX1 x
      (yWhich : ys) <- downX1 y
      return
        ( "Vertical rule for right transformed variant less than",
          return
            $ make opOr
            $ fromList
              [ [essence| &xWhich < &yWhich |],
                make opAnd
                  $ fromList
                    [ [essence| &xWhich = &yWhich |],
                      onTagged (make opLt) xWhich (rx <$> xs) (ry <$> ys)
                    ]
              ]
        )

rule_Transform_Variant_Leq :: Rule
rule_Transform_Variant_Leq = "transform-variant-leq" `namedRule` theRule
  where
    theRule p = do
      (l, r) <- match opLeq p
      atLeastOneTransform (l, r)
      let (x, rx) = matchManyTransforms l
      let (y, ry) = matchManyTransforms r
      TypeVariant {} <- typeOf x
      TypeVariant {} <- typeOf y
      (xWhich : xs) <- downX1 x
      (yWhich : ys) <- downX1 y
      return
        ( "Vertical rule for right transformed variant less than eq",
          return
            $ make opOr
            $ fromList
              [ [essence| &xWhich < &yWhich |],
                make opAnd
                  $ fromList
                    [ [essence| &xWhich = &yWhich |],
                      onTagged (make opLeq) xWhich (rx <$> xs) (ry <$> ys)
                    ]
              ]
        )

rule_Transformed_Variant_Index :: Rule
rule_Transformed_Variant_Index = "transformed-variant-index" `namedRule` theRule
  where
    theRule p = do
      (l, arg) <- match opIndexing p
      atLeastOneTransform (l, l)
      let (x, rx) = matchManyTransforms l
      TypeVariant ds <- typeOf x
      (xWhich : xs) <- downX1 x
      name <- nameOut arg
      argInt <-
        case elemIndex name (map fst ds) of
          Nothing -> failDoc "Variant indexing, not a member of the type."
          Just argInt -> return argInt
      return
        ( "Variant indexing on:" <+> pretty p,
          return
            $ WithLocals
              (rx (atNote "Variant indexing" xs argInt))
              ( DefinednessConstraints
                  [ [essence| &xWhich = &argInt2 |]
                    | let argInt2 = fromInt (fromIntegral (argInt + 1))
                  ]
              )
        )

rule_Transformed_Variant_Active :: Rule
rule_Transformed_Variant_Active = "transformed-variant-active" `namedRule` theRule
  where
    theRule p = do
      (l, name) <- match opActive p
      atLeastOneTransform (l, l)
      let (x, _) = matchManyTransforms l
      TypeVariant ds <- typeOf x
      (xWhich : _) <- downX1 x
      argInt <- case elemIndex name (map fst ds) of
        Nothing -> failDoc "Variant indexing, not a member of the type."
        Just argInt -> return $ fromInt $ fromIntegral $ argInt + 1
      return
        ( "Variant active on:" <+> pretty p,
          return [essence| &xWhich = &argInt |]
        )
