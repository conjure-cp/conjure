module Conjure.Rules.Vertical.Relation.RelationAsMatrix where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.DomainOf
import Conjure.Language.TypeOf
import Conjure.Language.Lenses

import Conjure.Rules.Definition ( Rule(..), namedRule, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Image :: Rule
rule_Image = "relation-image{RelationAsMatrix}" `namedRule` theRule where
    theRule p = do
        (rel, args)         <- match opRelationImage p
        TypeRelation{}      <- typeOf rel
        "RelationAsMatrix"  <- representationOf rel
        [m]                 <- downX1 rel
        let unroll = foldl (make opIndexing)
        return ( "relation image, RelationAsMatrix representation"
               , const $ unroll m args
               )


rule_Comprehension :: Rule
rule_Comprehension = "relation-map_in_expr{RelationAsMatrix}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, rel), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDef opToSet expr)
            _ -> na "rule_Comprehension"
        let upd val old        =  lambdaToFunction pat old val
        TypeRelation{}         <- typeOf rel
        "RelationAsMatrix"     <- representationOf rel
        [m]                    <- downX1 rel
        mDom                   <- domainOf m
        let (mIndices, _)      =  getIndices mDom

        -- we need something like:
        -- Q i in rel . f(i)
        -- Q j in (indices...) , filter(f) . f(tuple)

        -- let out fresh = unroll m [] (zip [ quantifiedVar fr TypeInt | fr <- fresh ] mIndices)
        return ( "Vertical rule for map_in_expr for relation domains, RelationAsMatrix representation."
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)

                        lit = AbstractLiteral $ AbsLitTuple
                                    [ make opIndexing i (fromInt n) | n <- [1 .. genericLength mIndices] ]
                        indexThis anyMatrix = make opMatrixIndexing anyMatrix
                                    [ make opIndexing i (fromInt n) | n <- [1 .. genericLength mIndices] ]

                    in  Comprehension (upd lit body)
                            $  gocBefore
                            ++ [ Generator (GenDomainNoRepr iPat (DomainTuple mIndices))
                               , Condition (indexThis m)
                               ]
                            ++ transformBi (upd lit) gocAfter
               )
    theRule _ = na "rule_Comprehension"
