{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Tuple where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation, isAtomic, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


rule_Tuple_Eq :: Rule
rule_Tuple_Eq = "tuple-eq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opEq p
        TypeTuple _ <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple _ <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        return ( "Horizontal rule for tuple equality"
               , const $ make opAnd (zipWith (make opEq) xs ys)
               )


rule_Tuple_Lt :: Rule
rule_Tuple_Lt = "tuple-lt" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLt p
        TypeTuple _ <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple _ <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        let unroll [a]    [b]    = [essence| &a < &b |]
            unroll (a:as) (b:bs) = let rest = unroll as bs
                                   in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
            unroll _ _ = bug ("arity mismatch in:" <+> pretty p)
        return ( "Horizontal rule for tuple <"
               , const $ unroll xs ys
               )


rule_Tuple_Leq :: Rule
rule_Tuple_Leq = "tuple-leq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLeq p
        TypeTuple _ <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple _ <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        let unroll [a]    [b]    = [essence| &a <= &b |]
            unroll (a:as) (b:bs) = let rest = unroll as bs
                                   in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
            unroll _ _ = bug ("arity mismatch in:" <+> pretty p)
        return ( "Horizontal rule for tuple <="
               , const $ unroll xs ys
               )


rule_Tuple_Index :: Rule
rule_Tuple_Index = "tuple-index" `namedRule` theRule where
    theRule p = do
        (t,i)       <- match opIndexing p
        TypeTuple{} <- typeOf t
        iInt        <- match constantInt i
        ts          <- downX1 t
        return ( "Tuple indexing on:" <+> pretty p
               , const $ atNote "Tuple indexing" ts (iInt-1)
               )


rule_Tuple_DomainComprehension :: Rule
rule_Tuple_DomainComprehension = "tuple-domain-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (gofBefore, (pat, domains), gofAfter) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenDomain pat@Single{} (DomainTuple domains)) -> return (pat, domains)
            _ -> fail "No match."
        let pats fresh     = [ Single i            | i <- fresh ]
        let refs fresh     = [ Reference i Nothing | i <- fresh ]
        let theValue fresh = AbstractLiteral (AbsLitTuple (refs fresh))
        let upd val old    = lambdaToFunction pat old val -- given an expression "old",
                                                          -- update uses of "pat" in it
                                                          -- to use "val"
        return ( "Tuple domain comprehension"
               , \ fresh' ->
                   let fresh = take (length domains) fresh'
                       val = theValue fresh
                   in  Comprehension (upd val body)
                       $  gofBefore
                       ++ [ Generator (GenDomain p d)
                          | (p,d) <- zip (pats fresh) domains ]
                       ++ transformBi (upd val) gofAfter
               )
    theRule _ = fail "No match."
