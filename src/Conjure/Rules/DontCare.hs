{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.DontCare where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation )

import Conjure.Representations ( downX1 )


rule_Bool :: Rule
rule_Bool = "dontCare-bool" `namedRule` theRule where
    theRule p = do
        x          <- match opDontCare p
        DomainBool <- domainOf x
        return ( "dontCare value for bools is false."
               , const $ make opEq x (fromBool False)
               )


rule_Int :: Rule
rule_Int = "dontCare-int" `namedRule` theRule where
    theRule p = do
        x                          <- match opDontCare p
        xDomain@(DomainInt ranges) <- domainOf x
        let raiseBug = bug ("dontCare on domain:" <+> pretty xDomain)
        let val = case ranges of
                [] -> raiseBug
                (r:_) -> case r of
                    RangeOpen -> raiseBug
                    RangeSingle v -> v
                    RangeLowerBounded v -> v
                    RangeUpperBounded v -> v
                    RangeBounded v _ -> v
        return ( "dontCare value for this integer is" <+> pretty val
               , const $ make opEq x val
               )


rule_Tuple :: Rule
rule_Tuple = "dontCare-tuple" `namedRule` theRule where
    theRule p = do
        x           <- match opDontCare p
        TypeTuple{} <- typeOf x
        xs          <- downX1 x
        return ( "dontCare handling for tuple"
               , const $ make opAnd $ fromList $ map (make opDontCare) xs
               )


rule_Record :: Rule
rule_Record = "dontCare-record" `namedRule` theRule where
    theRule p = do
        x            <- match opDontCare p
        TypeRecord{} <- typeOf x
        xs           <- downX1 x
        return ( "dontCare handling for record"
               , const $ make opAnd $ fromList $ map (make opDontCare) xs
               )


rule_Variant :: Rule
rule_Variant = "dontCare-variant" `namedRule` theRule where
    theRule p = do
        x             <- match opDontCare p
        TypeVariant{} <- typeOf x
        xs            <- downX1 x
        return ( "dontCare handling for variant"
               , const $ make opAnd $ fromList $ map (make opDontCare) xs
               )


rule_Matrix :: Rule
rule_Matrix = "dontCare-matrix" `namedRule` theRule where
    theRule p = do
        x                    <- match opDontCare p
        DomainMatrix index _ <- domainOf x
        return ( "dontCare handling for matrix"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat : &index . dontCare(&x[&i]) |]
               )


rule_Abstract :: Rule
rule_Abstract = "dontCare-abstract" `namedRule` theRule where
    theRule p = do
        x  <- match opDontCare p
        ty <- typeOf x
        case ty of
            TypeSet       {} -> return ()
            TypeMSet      {} -> return ()
            TypeFunction  {} -> return ()
            TypeRelation  {} -> return ()
            TypePartition {} -> return ()
            _ -> na "not a known abstract domain"
        hasRepresentation x
        xs <- downX1 x
        return ( "dontCare handling for an abstract domain"
               , const $ make opAnd $ fromList $ map (make opDontCare) xs
               )
