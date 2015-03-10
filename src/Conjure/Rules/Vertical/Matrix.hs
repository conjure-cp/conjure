{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Matrix where

import Conjure.Rules.Import
import Conjure.Rules.Vertical.Tuple ( decomposeLexLt, decomposeLexLeq )


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "matrix-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        (_, index, _elems) <- match matrixLiteral expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for matrix-comprehension on matrix literal"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                     val = make opIndexing expr i
                 in  Comprehension (upd val body)
                         $  gocBefore
                         ++ [Generator (GenDomainNoRepr iPat index)]
                         ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


-- | input:  [ i | i <- m[j] ] with m = [a,b,c]
--   output: [ [ i | i <- a ]
--           , [ i | i <- b ]
--           , [ i | i <- c ]
--           ][j]
--   namely: [ [ m[k]
--             | k : int(1..3)
--             ]
--           , [ i | i <- b ]
--           , [ i | i <- c ]
--           ][j]

rule_Comprehension_LiteralIndexed :: Rule
rule_Comprehension_LiteralIndexed = "matrix-comprehension-literal-indexed" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_LiteralIndexed"
        (mkM, expr2)      <- match opModifier expr
        (matrix, indices) <- match opMatrixIndexing expr2
        (_, index, elems) <- match matrixLiteral matrix
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for matrix-comprehension on matrix literal (indexed)"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                    comprehensions =
                        [ Comprehension (upd i body)
                             $  gocBefore
                             ++ [Generator (GenInExpr iPat (mkM el))]
                             ++ transformBi (upd i) gocAfter
                        | el <- elems
                        ]
                    core = AbstractLiteral $ AbsLitMatrix index comprehensions
                in
                    make opMatrixIndexing core indices
            )
    theRule _ = na "rule_Comprehension_LiteralIndexed"


rule_Comprehension_ToSet :: Rule
rule_Comprehension_ToSet = "matrix-toSet" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_ToSet"
        matrix       <- match opToSet expr
        TypeMatrix{} <- typeOf matrix
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for comprehension over matrix-toSet"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                     val  = make opIndexing i 1
                     over = make opHist matrix
                 in  Comprehension (upd val body)
                         $  gocBefore
                         ++ [Generator (GenInExpr iPat over)]
                         ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_ToSet"


-- [ i | ... , i <- [ j | ... j ... ], ... i ... ]
-- [ j | ... , ... j ..., ... j ... ]
rule_Comprehension_Nested :: Rule
rule_Comprehension_Nested = "matrix-comprehension-nested" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, innerBody, innerGof), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} (Comprehension innerBody innerGof)) -> return (pat, innerBody, innerGof)
            _ -> na "rule_Comprehension_Nested"
        let upd val old = lambdaToFunction pat old val
        return
            ( "Nested matrix comprehension"
            , const $ Comprehension (upd innerBody body)
                         $  gocBefore
                         ++ innerGof
                         ++ transformBi (upd innerBody) gocAfter
            )
    theRule _ = na "rule_Comprehension_Nested"


-- withAuxVar :: Name -> Domain () Expression -> (Expression -> Expression) -> Expression
-- withAuxVar nm dom f =
--     WithLocals
--         (Reference nm Nothing)
--         [ Declaration (FindOrGiven LocalFind nm dom)
--         , SuchThat [f (Reference nm Nothing)]
--         ]
--         []
--
--
-- rule_Comprehension_ToSet2 :: Rule
-- rule_Comprehension_ToSet2 = "matrix-toSet2" `namedRule` theRule where
--     theRule p = do
--         let lu (Comprehension body goc) = return (body, goc)
--             lu (Reference _ (Just (Alias ref))) = lu ref
--             lu _ = fail "not a comprehension"
--         inToSet     <- match opToSet p
--         (body, goc) <- lu inToSet
--         domBody     <- domainOf body
--         return
--             ( "Vertical rule for comprehension over matrix-hist"
--             , \ fresh -> withAuxVar
--                     (fresh `at` 0)
--                     (DomainSet () def (forgetRepr domBody)) $ \ aux ->
--                         make opAnd $ Comprehension [essence| &body in &aux |] goc
--             )


rule_Comprehension_Hist :: Rule
rule_Comprehension_Hist = "matrix-hist" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Hist"
        matrix               <- match opHist expr
        TypeMatrix{}         <- typeOf matrix
        DomainMatrix index _ <- domainOf matrix
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for comprehension over matrix-hist"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                     (jPat, j) = quantifiedVar (fresh `at` 1)
                     value = [essence| &matrix[&i] |]
                     -- if this is the left-most occurrence of value
                     -- count all
                     -- otherwise, 0
                     count = [essence|
                         sum([ 1                            $ number of occurrences of this value in the matrix
                             | &jPat : &index
                             , &matrix[&i] = &matrix[&j]
                             ])
                     |]
                     val   = AbstractLiteral $ AbsLitTuple [value, count]
                     appearsBefore = [essence|
                         or([ &matrix[&j] = &matrix[&i]
                            | &jPat : &index
                            , &j < &i
                            ])
                    |]
                 in  Comprehension (upd val body)
                         $  gocBefore
                         ++ [ Generator (GenDomainNoRepr iPat index)
                            , Condition [essence| ! &appearsBefore |]
                            ]
                         ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_Hist"


rule_Matrix_Eq :: Rule
rule_Matrix_Eq = "matrix-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                 <- match opEq p
        TypeMatrix{}          <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}          <- typeOf y
        DomainMatrix indexX _ <- domainOf x
        DomainMatrix indexY _ <- domainOf y
        return
            ( "Horizontal rule for matrix ="
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                     (jPat, j) = quantifiedVar (fresh `at` 1)
                 in  [essence| (forAll &iPat : &indexX . &x[&i] = &y[&i])
                                /\
                               (forAll &iPat : &indexX . exists &jPat : &indexY . &i = &j)
                                /\
                               (forAll &iPat : &indexY . exists &jPat : &indexX . &i = &j)
                             |]
            )


rule_Matrix_Neq :: Rule
rule_Matrix_Neq = "matrix-neq" `namedRule` theRule where
    theRule p = do
        (x,y)                 <- match opNeq p
        TypeMatrix{}          <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}          <- typeOf y
        DomainMatrix indexX _ <- domainOf x
        DomainMatrix indexY _ <- domainOf y
        return
            ( "Horizontal rule for matrix !="
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                 in  [essence| (exists &iPat : &indexX . &x[&i] != &y[&i])
                                 \/
                               (exists &iPat : &indexY . &x[&i] != &y[&i])
                             |]
            )


flattenIfNeeded :: MonadFail m => Expression -> m Expression
flattenIfNeeded m = do
    tyM <- typeOf m
    let nestingLevel (TypeMatrix _ a) = 1 + nestingLevel a
        nestingLevel (TypeList     a) = 1 + nestingLevel a
        nestingLevel _ = 0 :: Int
    return $ if nestingLevel tyM > 1
        then make opFlatten m
        else m

rule_Matrix_Lt_Primitive :: Rule
rule_Matrix_Lt_Primitive = "matrix-lt-primitive" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLt p
        tx@TypeMatrix{} <- typeOf x        -- TODO: check if x and y have the same arity
        ty@TypeMatrix{} <- typeOf y
        unless (isPrimitiveType tx) $ fail ("not a primitive type:" <+> pretty tx)
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        x' <- flattenIfNeeded x
        y' <- flattenIfNeeded y
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
        x' <- flattenIfNeeded x
        y' <- flattenIfNeeded y
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


rule_Comprehension_SingletonDomain :: Rule
rule_Comprehension_SingletonDomain = "matrix-comprehension-singleton-domain" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, singleVal), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainHasRepr patName (DomainInt [RangeSingle a])) -> return (Single patName, a)
            Generator (GenDomainHasRepr patName (DomainInt [RangeBounded a b])) | a == b -> return (Single patName, a)
            _ -> na "rule_Comprehension_SingletonDomain"
        let upd val old = lambdaToFunction pat old val
        return
            ( "Removing matrix-comprehension of a singleton int domain"
            , const $
                if null gocBefore && null gocAfter
                    then AbstractLiteral $ AbsLitMatrix (mkDomainIntB 1 1) [upd singleVal body]
                    else Comprehension (upd singleVal body)
                         $  gocBefore
                         ++ transformBi (upd singleVal) gocAfter
            )
    theRule _ = na "rule_Comprehension_SingletonDomain"


rule_Comprehension_Singleton :: Rule
rule_Comprehension_Singleton = "matrix-comprehension-singleton" `namedRule` theRule where
    theRule p = do
        (_mkQuan, AbstractLiteral (AbsLitMatrix _ [singleVal])) <- match opQuantifier p
        return
            ( "Removing quantifier of a single item"
            , const $ singleVal
            )


rule_MatrixIndexing :: Rule
rule_MatrixIndexing = "matrix-indexing" `namedRule` theRule where
    theRule p = do
        (matrix, indexer)            <- match opIndexing p
        (_, DomainInt ranges, elems) <- match matrixLiteral matrix
        indexInts                    <- rangesInts ranges
        indexerInt                   <- intOut indexer
        if length indexInts == length elems
            then
                case lookup indexerInt (zip indexInts elems) of
                    Nothing -> na "rule_MatrixIndexing"
                    Just v  ->
                        return
                            ( "Matrix indexing"
                            , const v
                            )
            else na "rule_MatrixIndexing"


rule_IndexingIdentical :: Rule
rule_IndexingIdentical = "matrix-indexing-identical" `namedRule` theRule where
    theRule p = do
        (matrix, indexer)                       <- match opIndexing p
        (_, indexDomain, (firstElem:restElems)) <- match matrixLiteral matrix
        indexerDomain <- domainOf indexer
        if indexDomain == forgetRepr indexerDomain && all (firstElem==) restElems
            then return
                    ( "rule_IndexingIdentical"
                    , const firstElem
                    )
            else na "rule_IndexingIdentical"
