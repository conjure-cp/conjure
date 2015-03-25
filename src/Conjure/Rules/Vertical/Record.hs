{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Record where

import Conjure.Rules.Import


rule_Record_Eq :: Rule
rule_Record_Eq = "record-eq" `namedRule` theRule where
    theRule p = do
        (x,y)        <- match opEq p
        TypeRecord{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeRecord{} <- typeOf y
        xs           <- downX1 x
        ys           <- downX1 y
        return
            ( "Horizontal rule for record equality"
            , return $ make opAnd $ fromList $ zipWith (make opEq) xs ys
            )


rule_Record_Neq :: Rule
rule_Record_Neq = "record-neq" `namedRule` theRule where
    theRule p = do
        (x,y)        <- match opNeq p
        TypeRecord{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeRecord{} <- typeOf y
        xs           <- downX1 x
        ys           <- downX1 y
        return
            ( "Horizontal rule for record !="
            , return $ make opNot $ make opAnd $ fromList $ zipWith (make opEq) xs ys
            )


rule_Record_Lt :: Rule
rule_Record_Lt = "record-lt" `namedRule` theRule where
    theRule p = do
        (x,y)        <- match opLt p
        TypeRecord{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeRecord{} <- typeOf y
        xs           <- downX1 x
        ys           <- downX1 y
        return
            ( "Horizontal rule for record <"
            , return $ decomposeLexLt p xs ys
            )


rule_Record_Leq :: Rule
rule_Record_Leq = "record-leq" `namedRule` theRule where
    theRule p = do
        (x,y)        <- match opLeq p
        TypeRecord{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeRecord{} <- typeOf y
        xs           <- downX1 x
        ys           <- downX1 y
        return
            ( "Horizontal rule for record <="
            , return $ decomposeLexLeq p xs ys
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


rule_Record_Index :: Rule
rule_Record_Index = "record-index" `namedRule` theRule where
    theRule p = do
        (t,i)         <- match opIndexing p
        TypeRecord ds <- typeOf t
        name          <- nameOut i
        iInt          <- case findIndex (name==) (map fst ds) of
                            Nothing   -> fail "Record indexing, not a member of the type."
                            Just iInt -> return iInt
        ts            <- downX1 t
        return
            ( "Record indexing on:" <+> pretty p
            , return $ atNote "Record indexing" ts iInt
            )
