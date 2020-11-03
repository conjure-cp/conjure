module Conjure.Rules.Vertical.Relation.RelationAsMatrix where

import Conjure.Rules.Import


rule_Image :: Rule
rule_Image = "relation-image{RelationAsMatrix}" `namedRule` theRule where
    theRule p = do
        (rel, args)         <- match opRelationImage p
        TypeRelation{}      <- typeOf rel
        Relation_AsMatrix   <- representationOf rel
        [m]                 <- downX1 rel
        let unroll = foldl (make opIndexing)
        return
            ( "relation image, RelationAsMatrix representation"
            , return $ unroll m args
            )


rule_Comprehension :: Rule
rule_Comprehension = "relation-comprehension{RelationAsMatrix}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, rel), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension"
        let upd val old        =  lambdaToFunction pat old val
        TypeRelation{}         <- typeOf rel
        Relation_AsMatrix      <- representationOf rel
        [m]                    <- downX1 rel
        mDom                   <- domainOf m
        let (mIndices, _)      =  getIndices mDom

        -- we need something like:
        -- Q i in rel . f(i)
        -- Q j in (indices...) , filter(f) . f(tuple)

        -- let out fresh = unroll m [] (zip [ quantifiedVar fr TypeInt | fr <- fresh ] mIndices)
        return
            ( "Vertical rule for comprehension for relation domains, RelationAsMatrix representation."
            , do
                (iPat, i) <- quantifiedVar

                let lit = AbstractLiteral $ AbsLitTuple
                                 [ make opIndexing i (fromInt n) | n <- [1 .. genericLength mIndices] ]
                let indexThis anyMatrix = make opMatrixIndexing anyMatrix
                                 [ make opIndexing i (fromInt n) | n <- [1 .. genericLength mIndices] ]

                return $ Comprehension (upd lit body)
                         $  gocBefore
                         ++ [ Generator (GenDomainNoRepr iPat (DomainTuple mIndices))
                            , Condition (indexThis m)
                            ]
                         ++ transformBi (upd lit) gocAfter
            )
    theRule _ = na "rule_Comprehension"
