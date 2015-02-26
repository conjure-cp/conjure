{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Tuple where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule )

import Conjure.Representations ( downX1 )


rule_Tuple_Eq :: Rule
rule_Tuple_Eq = "tuple-eq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opEq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple equality"
            , const $ make opAnd $ fromList $ zipWith (make opEq) xs ys
            )


rule_Tuple_Neq :: Rule
rule_Tuple_Neq = "tuple-neq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opNeq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple !="
            , const $ make opNot $ make opAnd $ fromList $ zipWith (make opEq) xs ys
            )


rule_Tuple_Lt :: Rule
rule_Tuple_Lt = "tuple-lt" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLt p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple <"
            , const $ decomposeLexLt p xs ys
            )


rule_Tuple_Leq :: Rule
rule_Tuple_Leq = "tuple-leq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLeq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple <="
            , const $ decomposeLexLeq p xs ys
            )


decomposeLexLt :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexLt p xs ys = unroll xs ys
    where
        unroll [a]    [b]    = [essence| &a < &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)


decomposeLexLeq :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexLeq p xs ys = unroll xs ys
    where
        unroll [a]    [b]    = [essence| &a <= &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)


rule_Tuple_Index :: Rule
rule_Tuple_Index = "tuple-index" `namedRule` theRule where
    theRule p = do
        (t,i)       <- match opIndexing p
        TypeTuple{} <- typeOf t
        iInt        <- match constantInt i
        ts          <- downX1 t
        return
            ( "Tuple indexing on:" <+> pretty p
            , const $ atNote "Tuple indexing" ts (fromInteger (iInt-1))
            )
