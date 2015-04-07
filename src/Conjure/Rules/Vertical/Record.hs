{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Record where

import Conjure.Rules.Import
import Conjure.Rules.Vertical.Tuple ( decomposeLexLt, decomposeLexLeq, decomposeLexDotLt, decomposeLexDotLeq  )


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
rule_Record_Lt = "record-Lt" `namedRule` theRule where
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
rule_Record_Leq = "record-Leq" `namedRule` theRule where
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


rule_Record_DotLt :: Rule
rule_Record_DotLt = "record-DotLt" `namedRule` theRule where
    theRule p = do
        (x,y)        <- match opDotLt p
        TypeRecord{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeRecord{} <- typeOf y
        xs           <- downX1 x
        ys           <- downX1 y
        return
            ( "Horizontal rule for record <"
            , return $ decomposeLexDotLt p xs ys
            )


rule_Record_DotLeq :: Rule
rule_Record_DotLeq = "record-DotLeq" `namedRule` theRule where
    theRule p = do
        (x,y)        <- match opDotLeq p
        TypeRecord{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeRecord{} <- typeOf y
        xs           <- downX1 x
        ys           <- downX1 y
        return
            ( "Horizontal rule for record <="
            , return $ decomposeLexDotLeq p xs ys
            )


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
