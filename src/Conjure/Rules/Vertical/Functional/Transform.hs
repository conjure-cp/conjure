{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Vertical.Functional.Transform where
import Conjure.Rules.Vertical.Variant (onTagged)
import Conjure.Rules.Import


matchManyTransforms :: Expression
                    -> (Expression, Expression -> Expression)
matchManyTransforms exp =
  case match opTransform exp of
    Nothing -> (exp, id)
    Just (morphism, so) ->
      let (nexp, ntrans) = matchManyTransforms so
      in ( nexp
         , \x -> let nx = ntrans x in [essence| transform(&morphism, &nx) |])

rule_Transform_Variant_Eq :: Rule
rule_Transform_Variant_Eq = "transform-variant-eq" `namedRule` theRule where
  theRule p = do
    (l,r) <- match opEq p
    let (x, rx) = matchManyTransforms l
    let (y, ry) = matchManyTransforms r
    TypeVariant{} <- typeOf x
    TypeVariant{} <- typeOf y
    (xWhich:xs)   <- downX1 x
    (yWhich:ys)   <- downX1 y
    return ( "Vertical rule for right transformed variant equality"
           , return $ make opAnd $ fromList
               [ [essence| &xWhich = &yWhich |]
               , onTagged (make opEq) xWhich (rx <$> xs) (ry<$> ys)
               ]
           )


rule_Transform_Variant_Neq :: Rule
rule_Transform_Variant_Neq = "transform-variant-neq" `namedRule` theRule where
  theRule p = do
    (l,r) <- match opNeq p
    let (x, rx) = matchManyTransforms l
    let (y, ry) = matchManyTransforms r
    TypeVariant{} <- typeOf x
    TypeVariant{} <- typeOf y
    (xWhich:xs)   <- downX1 x
    (yWhich:ys)   <- downX1 y
    return ( "Vertical rule for right transformed variant nequality"
           , return $ make opOr $ fromList
               [ [essence| &xWhich != &yWhich |]
               , make opAnd $ fromList
                   [ [essence| &xWhich = &yWhich |]
                   , onTagged (make opNeq) xWhich (rx <$> xs) (ry<$> ys)
                   ]
               ]
           )


rule_Transform_Variant_Lt :: Rule
rule_Transform_Variant_Lt = "transform-variant-lt" `namedRule` theRule where
  theRule p = do
    (l,r) <- match opLt p
    let (x, rx) = matchManyTransforms l
    let (y, ry) = matchManyTransforms r
    TypeVariant{} <- typeOf x
    TypeVariant{} <- typeOf y
    (xWhich:xs)   <- downX1 x
    (yWhich:ys)   <- downX1 y
    return ( "Vertical rule for right transformed variant less than"
           , return $ make opOr $ fromList
               [ [essence| &xWhich < &yWhich |]
               , make opAnd $ fromList
                   [ [essence| &xWhich = &yWhich |]
                   , onTagged (make opLt) xWhich (rx <$> xs) (ry<$> ys)
                   ]
               ]
           )

rule_Transform_Variant_Leq :: Rule
rule_Transform_Variant_Leq = "transform-variant-leq" `namedRule` theRule where
  theRule p = do
    (l,r) <- match opLeq p
    let (x, rx) = matchManyTransforms l
    let (y, ry) = matchManyTransforms r
    TypeVariant{} <- typeOf x
    TypeVariant{} <- typeOf y
    (xWhich:xs)   <- downX1 x
    (yWhich:ys)   <- downX1 y
    return ( "Vertical rule for right transformed variant less than eq"
           , return $ make opOr $ fromList
               [ [essence| &xWhich < &yWhich |]
               , make opAnd $ fromList
                   [ [essence| &xWhich = &yWhich |]
                   , onTagged (make opLeq) xWhich (rx <$> xs) (ry<$> ys)
                   ]
               ]
           )

rule_Transformed_Variant_Index :: Rule
rule_Transformed_Variant_Index = "transformed-variant-index" `namedRule` theRule where
    theRule p = do
        (l,arg)        <- match opIndexing p
        let (x, rx)    = matchManyTransforms l
        TypeVariant ds <- typeOf x
        (xWhich:xs)    <- downX1 x
        name           <- nameOut arg
        argInt         <-
          case elemIndex name (map fst ds) of
            Nothing     -> fail "Variant indexing, not a member of the type."
            Just argInt -> return argInt
        return
            ( "Variant indexing on:" <+> pretty p
            , return $ WithLocals
                (rx (atNote "Variant indexing" xs argInt))
                (DefinednessConstraints
                    [ [essence| &xWhich = &argInt2 |]
                    | let argInt2 = fromInt (fromIntegral (argInt + 1))
                    ])
            )


rule_Transformed_Variant_Active :: Rule
rule_Transformed_Variant_Active = "transformed-variant-active" `namedRule` theRule where
    theRule p = do
        (l,name)       <- match opActive p
        let (x, _)    = matchManyTransforms l
        TypeVariant ds <- typeOf x
        (xWhich:_)     <- downX1 x
        argInt         <- case elemIndex name (map fst ds) of
                            Nothing     -> fail "Variant indexing, not a member of the type."
                            Just argInt -> return $ fromInt $ fromIntegral $ argInt + 1
        return
            ( "Variant active on:" <+> pretty p
            , return $ [essence| &xWhich = &argInt |]
            )

