{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Matrix where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Domain
import Conjure.Language.DomainOf
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, matchFirst )

import Conjure.Representations ( downX1 )
import Conjure.Rules.Vertical.Tuple ( decomposeLexLt, decomposeLexLeq )


-- TODO: when _gofBefore and _gofAfter are /= []
rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "matrix-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrFilters) = do
        (_gofBefore@[], (pat, expr), _gofAfter@[]) <- matchFirst gensOrFilters $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        (index, elems) <- match matrixLiteral expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for matrix-comprehension"
            , const $ AbstractLiteral $ AbsLitMatrix index
                [ upd e body
                | e <- elems
                ]
            )
    theRule _ = na "rule_Comprehension_Literal"


rule_Matrix_Eq :: Rule
rule_Matrix_Eq = "matrix-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                <- match opEq p
        TypeMatrix{}         <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}         <- typeOf y
        DomainMatrix index _ <- domainOf x
        return
            ( "Horizontal rule for matrix ="
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                 in  [essence| forAll &iPat : &index . &x[&i] = &y[&i] |]
            )


sliceEnoughTimes :: MonadFail m => Expression -> m Expression
sliceEnoughTimes m = do
    tym    <- typeOf m
    let nestingLevel (TypeMatrix _ a) = 1 + nestingLevel a
        nestingLevel _ = 0 :: Int
    let howMany = nestingLevel tym
    let unroll a 0 = a
        unroll a i = make opSlicing (unroll a (i-1)) Nothing Nothing
    let sliced = unroll m howMany
    let flatten = if howMany > 0 then make opFlatten else id
    return $ flatten sliced


rule_Matrix_Lt_Primitive :: Rule
rule_Matrix_Lt_Primitive = "matrix-lt-primitive" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLt p
        tx@TypeMatrix{} <- typeOf x        -- TODO: check if x and y have the same arity
        ty@TypeMatrix{} <- typeOf y
        unless (isPrimitiveType tx) $ fail ("not a primitive type:" <+> pretty tx)
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        x' <- sliceEnoughTimes x
        y' <- sliceEnoughTimes y
        return
            ( "Horizontal rule for matrix <"
            , const [essence| &x' <lex &y' |]
            )


rule_Matrix_Lt_Decompose :: Rule
rule_Matrix_Lt_Decompose = "matrix-lt-tuple" `namedRule` theRule where
    theRule p = do
        (x,y) <- match opLt p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ fail ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ fail ("this is a primitive type:" <+> pretty ty)
        xs <- downX1 x
        ys <- downX1 y
        return
            ( "Horizontal rule for matrix <, decomposing"
            , const $ decomposeLexLt p xs ys
            )


rule_Matrix_Leq_Primitive :: Rule
rule_Matrix_Leq_Primitive = "matrix-leq-primitive" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLeq p
        tx@TypeMatrix{} <- typeOf x        -- TODO: check if x and y have the same arity
        ty@TypeMatrix{} <- typeOf y
        unless (isPrimitiveType tx) $ fail ("not a primitive type:" <+> pretty tx)
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        x' <- sliceEnoughTimes x
        y' <- sliceEnoughTimes y
        return
            ( "Horizontal rule for matrix <="
            , const [essence| &x' <=lex &y' |]
            )


rule_Matrix_Leq_Decompose :: Rule
rule_Matrix_Leq_Decompose = "matrix-leq-tuple" `namedRule` theRule where
    theRule p = do
        (x,y) <- match opLeq p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ fail ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ fail ("this is a primitive type:" <+> pretty ty)
        xs <- downX1 x
        ys <- downX1 y
        return
            ( "Horizontal rule for matrix <=, decomposing"
            , const $ decomposeLexLeq p xs ys
            )
