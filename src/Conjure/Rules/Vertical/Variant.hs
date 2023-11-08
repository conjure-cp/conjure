{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Variant where

import Conjure.Rules.Import


rule_Variant_Eq :: Rule
rule_Variant_Eq = "variant-eq" `namedRule` theRule where
    theRule p = do
        (x,y)         <- match opEq p
        TypeVariant{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeVariant{} <- typeOf y
        (xWhich:xs)   <- downX1 x
        (yWhich:ys)   <- downX1 y
        return
            ( "Horizontal rule for variant equality"
            , return $ make opAnd $ fromList
                [ [essence| &xWhich = &yWhich |]                        -- the tags are eq
                , onTagged (make opEq) xWhich xs ys                     -- and the tagged values are eq
                ]
            )


rule_Variant_Neq :: Rule
rule_Variant_Neq = "variant-neq" `namedRule` theRule where
    theRule p = do
        (x,y)         <- match opNeq p
        TypeVariant{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeVariant{} <- typeOf y
        (xWhich:xs)   <- downX1 x
        (yWhich:ys)   <- downX1 y
        return
            ( "Horizontal rule for variant !="
            , return $ make opOr $ fromList
                [ [essence| &xWhich != &yWhich |]                       -- either the tags are diff
                , make opAnd $ fromList
                    [ [essence| &xWhich = &yWhich |]                    -- or the tags are eq
                    , onTagged (make opNeq) xWhich xs ys                -- and the tagged values are diff
                    ]
                ]
            )


rule_Variant_Lt :: Rule
rule_Variant_Lt = "variant-Lt" `namedRule` theRule where
    theRule p = do
        (x,y)         <- match opLt p
        TypeVariant{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeVariant{} <- typeOf y
        (xWhich:xs)   <- downX1 x
        (yWhich:ys)   <- downX1 y
        return
            ( "Horizontal rule for variant <"
            , return $ make opOr $ fromList
                [ [essence| &xWhich < &yWhich |]                        -- either the tags are <
                , make opAnd $ fromList
                    [ [essence| &xWhich = &yWhich |]                    -- or the tags are eq
                    , onTagged (make opLt) xWhich xs ys                 -- and the tagged values are <
                    ]
                ]
            )


rule_Variant_Leq :: Rule
rule_Variant_Leq = "variant-Leq" `namedRule` theRule where
    theRule p = do
        (x,y)         <- match opLeq p
        TypeVariant{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeVariant{} <- typeOf y
        (xWhich:xs)   <- downX1 x
        (yWhich:ys)   <- downX1 y
        return
            ( "Horizontal rule for variant <="
            , return $ make opOr $ fromList
                [ [essence| &xWhich < &yWhich |]                        -- either the tags are <
                , make opAnd $ fromList
                    [ [essence| &xWhich = &yWhich |]                    -- or the tags are eq
                    , onTagged (make opLeq) xWhich xs ys                -- and the tagged values are <=
                    ]
                ]
            )


rule_Variant_Index :: Rule
rule_Variant_Index = "variant-index" `namedRule` theRule where
    theRule p = do
        (x,arg)        <- match opIndexing p
        TypeVariant ds <- typeOf x
        (xWhich:xs)    <- downX1 x
        name           <- nameOut arg
        argInt         <- case elemIndex name (map fst ds) of
                            Nothing     -> failDoc "Variant indexing, not a member of the type."
                            Just argInt -> return argInt
        return
            ( "Variant indexing on:" <+> pretty p
            , return $ WithLocals
                (atNote "Variant indexing" xs argInt)                   -- the value is projected
                (DefinednessConstraints
                    [ [essence| &xWhich = &argInt2 |]                   -- the tag is equal to i
                    | let argInt2 = fromInt (fromIntegral (argInt + 1))
                    ])
            )


rule_Variant_Active :: Rule
rule_Variant_Active = "variant-active" `namedRule` theRule where
    theRule p = do
        (x,name)       <- match opActive p
        TypeVariant ds <- typeOf x
        (xWhich:_)     <- downX1 x
        argInt         <- case elemIndex name (map fst ds) of
                            Nothing     -> failDoc "Variant indexing, not a member of the type."
                            Just argInt -> return $ fromInt $ fromIntegral $ argInt + 1
        return
            ( "Variant active on:" <+> pretty p
            , return $ [essence| &xWhich = &argInt |]
            )


-- | puts a constraint on the pair of tagged values
--   NOTICE: you might want to check if the tags are same before calling this!
onTagged
    :: (Expression -> Expression -> Expression)         -- the constraint generator
    -> Expression                                       -- tag
    -> [Expression]                                     -- first bunch of options
    -> [Expression]                                     -- second bunch of options
    -> Expression                                       -- the constraint
onTagged f which xs ys = make opAnd $ fromList
    [ [essence| &i = &which -> &cons |]                 -- and the tagged values are eq
    | (i',x,y) <- zip3 [1..] xs ys
    , let i = fromInt i'
    , let cons = f x y
    , let zero = ExpressionMetaVar "zeroVal for variant"
    , x /= zero
    , y /= zero
    ]
