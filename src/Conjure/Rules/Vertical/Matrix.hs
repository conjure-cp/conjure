{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Rules.Vertical.Matrix where

import Conjure.Rules.Import
import Conjure.Rules.Vertical.Tuple ( decomposeLexLt, decomposeLexLeq, decomposeLexDotLt, decomposeLexDotLeq  )


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "matrix-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr (Single pat) expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        (_, _index, elems) <- match matrixLiteral expr
        tyInner <- typeOf body
        let ty = TypeMatrix TypeInt tyInner
        return
            ( "Vertical rule for matrix-comprehension on matrix literal"
            , return $ if null elems
                then make matrixLiteral ty (mkDomainIntB 1 0) []
                else make opConcatenate $ AbstractLiteral $ AbsLitMatrix
                    (mkDomainIntB 1 (fromInt $ genericLength elems))
                    [ Comprehension body
                        $  gocBefore
                        ++ [ComprehensionLetting pat el]
                        ++ gocAfter
                    | el <- elems
                    ]
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

rule_ModifierAroundIndexedMatrixLiteral :: Rule
rule_ModifierAroundIndexedMatrixLiteral = "modifier-around-indexed-matrix-literal" `namedRule` theRule where
    theRule p = do
        (mkM, p2)         <- match opModifier p
        (matrix, indices) <- match opMatrixIndexing p2
        case match opMatrixIndexing p of
            Nothing -> return ()
            Just{}  -> na "rule_ModifierAroundIndexedMatrixLiteral, no modifier"
        let
            fullyMatrixLiteral 0 _ = return True
            fullyMatrixLiteral n m =
                case match matrixLiteral m of
                    Nothing            -> return False
                    Just (_, _, elems) -> and <$> mapM (fullyMatrixLiteral (n-1)) elems
        True <- fullyMatrixLiteral (length indices) matrix
        return
            ( "Pushing a modifier inwards, through a matrix literal"
            , do
                matrix' <- onMatrixLiteral Nothing (return . mkM) matrix
                return $ make opMatrixIndexing matrix' indices
            )


rule_QuantifierAroundIndexedMatrixLiteral :: Rule
rule_QuantifierAroundIndexedMatrixLiteral = "quantifier-around-indexed-matrix-literal" `namedRule` theRule where
    theRule p = do
        (mkM, p2)         <- match opQuantifier p
        (matrix, indices) <- match opMatrixIndexing p2
        case match opMatrixIndexing p of
            Nothing -> return ()
            Just{}  -> na "rule_ModifierAroundIndexedMatrixLiteral, no quantifier"
        -- let
        --     fullyMatrixLiteral 0 _ = return True
        --     fullyMatrixLiteral n m =
        --         case match matrixLiteral m of
        --             Nothing            -> return False
        --             Just (_, _, elems) -> and <$> mapM (fullyMatrixLiteral (n-1)) elems
        -- True <- fullyMatrixLiteral (length indices) matrix
        return
            ( "Pushing a modifier inwards, through a matrix literal"
            , do
                matrix' <- onMatrixLiteral (Just (length indices)) (return . mkM) matrix
                return $ make opMatrixIndexing matrix' indices
            )


rule_Comprehension_LiteralIndexed :: Rule
rule_Comprehension_LiteralIndexed = "matrix-comprehension-literal-indexed" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_LiteralIndexed"
        tyExpr             <- typeOf expr
        (matrix, indices)  <- match opMatrixIndexing expr
        (_, _index, elems) <- match matrixLiteral matrix
        return
            ( "Vertical rule for matrix-comprehension on matrix literal"
            , case indices of
                [] -> bug "rule_Comprehension_LiteralIndexed indices=[]"
                [index] ->
                    return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                        (mkDomainIntB 1 (fromInt $ genericLength elems))
                        [ Comprehension body
                            $  gocBefore
                            ++ [ Generator (GenInExpr pat el)
                               , Condition [essence| &num = &index |]
                               ]
                            ++ gocAfter
                        | (num', el') <- zip [1..] elems
                        , let num = fromInt num'
                        -- let's not lose the type information for empty collections
                        , let el = if emptyCollectionX el'
                                    then case el' of Typed{} -> el'
                                                     _       -> Typed el' tyExpr
                                    else el'
                        ]
                (index:rest) ->
                    return $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                        (mkDomainIntB 1 (fromInt $ genericLength elems))
                        [ Comprehension body
                            $  gocBefore
                            ++ [ Generator (GenInExpr pat (make opMatrixIndexing el rest))
                               , Condition [essence| &num = &index |]
                               ]
                            ++ gocAfter
                        | (num', el') <- zip [1..] elems
                        , let num = fromInt num'
                        -- let's not lose the type information for empty collections
                        , let el = if emptyCollectionX el'
                                    then case el' of Typed{} -> el'
                                                     _       -> Typed el' tyExpr
                                    else el'
                        ]
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
            , do
                 (iPat, i) <- quantifiedVar
                 let val  = make opIndexing i 1
                 let over = make opHistAll matrix
                 return $ Comprehension (upd val body)
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
        (gocBefore, (pat, Comprehension innerBody innerGocs), gocAfter) <- matchFirst gensOrConds $ \case
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet, opToMSet] expr)
            _ -> na "rule_Comprehension_Nested"
        let upd val old = lambdaToFunction pat old val
        let
            -- update the quantified variable names inside innerBody&innerGocs_ here,
            -- because they may be shadowed.
            updateQuantified innerBody_ innerGocs_ = do
                let olds = concatMap collectOldQuantifiers innerGocs_
                if null olds
                    then return (innerBody_, innerGocs_)
                    else do
                        oldnews <- forM olds $ \ old -> do
                            (Single new, _) <- quantifiedVar
                            return (old, new)
                        let
                            f :: Name -> Name
                            f nm = fromMaybe nm (lookup nm oldnews)
                        return (transformBi f (innerBody_, innerGocs_))

            collectOldQuantifiers = \case
                Generator (GenDomainNoRepr  pt _) -> universeBi pt
                Generator (GenDomainHasRepr nm _) -> [nm]
                Generator (GenInExpr        pt _) -> universeBi pt
                Condition _                       -> []
                ComprehensionLetting nm _         -> [nm]

        (innerBody', innerGocs') <- updateQuantified innerBody innerGocs
        return
            ( "Nested matrix comprehension"
            , return $ Comprehension (upd innerBody' body)
                         $  gocBefore
                         ++ innerGocs'
                         ++ transformBi (upd innerBody') gocAfter
            )
    theRule _ = na "rule_Comprehension_Nested"


rule_Comprehension_HistAll :: Rule
rule_Comprehension_HistAll = "matrix-histAll" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_HistAll"
        matrix               <- match opHistAll expr
        TypeMatrix{}         <- typeOf matrix
        index:_              <- indexDomainsOf matrix
        let upd val old = lambdaToFunction pat old val
        return
            ( "Rule for comprehension over matrix-hist"
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 let value = [essence| &matrix[&i] |]
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
                 return $ Comprehension (upd val body)
                         $  gocBefore
                         ++ [ Generator (GenDomainNoRepr iPat index)
                            , Condition [essence| ! &appearsBefore |]
                            ]
                         ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_HistAll"


rule_Comprehension_HistFor :: Rule
rule_Comprehension_HistFor = "matrix-histFor" `namedRule` theRule where
    theRule [essence| hist(&m, &n)[&i] |] = do
        TypeMatrix _ mInner <- typeOf m
        TypeMatrix _ nInner <- typeOf n
        mIndex:_            <- indexDomainsOf m
        case nInner of
            _ | typeUnify mInner nInner ->
                return
                    ( "Rule for histogram, indexed"
                    , do
                        (jPat, j) <- quantifiedVar
                        return [essence| sum &jPat : &mIndex . toInt(&m[&j] = &n[&i])
                                         |]
                    )
            TypeTuple [lb, ub] | typesUnify [mInner,lb,ub] ->
                return
                    ( "Rule for histogram, indexed"
                    , do
                        (jPat, j) <- quantifiedVar
                        return [essence| sum &jPat : &mIndex . toInt(&m[&j] >= &n[&i][1]
                                                                  /\ &m[&j] <  &n[&i][2])
                                       |]
                    )
            _ -> na "rule_Comprehension_HistFor"
    theRule _ = na "rule_Comprehension_HistFor"


-- freq(mset,arg) ~~> sum([ toInt(arg = i) | i in mset ])
rule_Matrix_Freq :: Rule
rule_Matrix_Freq = "matrix-freq" `namedRule` theRule where
    theRule p = do
        (matrix, arg) <- match opFreq p
        index:_       <- indexDomainsOf matrix
        TypeMatrix{}  <- typeOf matrix
        return
            ( "Rule for matrix-freq."
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| sum &iPat : &index . toInt(&matrix[&i] = &arg) |]
            )


rule_Matrix_Eq :: Rule
rule_Matrix_Eq = "matrix-eq" `namedRule` theRule where
    theRule p = do
        (x,y)                 <- match opEq p
        TypeMatrix{}          <- typeOf x        -- TODO: check if x and y have the same arity
        TypeMatrix{}          <- typeOf y
        indexX:_              <- indexDomainsOf x
        indexY:_              <- indexDomainsOf y
        return
            ( "Horizontal rule for matrix ="
            , do
                 (iPat, i) <- quantifiedVar
                 (jPat, j) <- quantifiedVar
                 return [essence|
                     (forAll &iPat : &indexX . &x[&i] = &y[&i])
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
        indexX:_              <- indexDomainsOf x
        indexY:_              <- indexDomainsOf y
        return
            ( "Horizontal rule for matrix !="
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence|
                     (exists &iPat : &indexX . &x[&i] != &y[&i])
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
rule_Matrix_Lt_Primitive = "matrix-Lt-primitive" `namedRule` theRule where
    theRule p = do
        (x,y)           <- case (match opLt p, match opDotLt p) of
                                (Just a, _) -> return a
                                (_, Just a) -> return a
                                _ -> na "rule_Matrix_Lt_Primitive"
        tx@TypeMatrix{} <- typeOf x        -- TODO: check if x and y have the same arity
        ty@TypeMatrix{} <- typeOf y
        unless (isPrimitiveType tx) $ fail ("not a primitive type:" <+> pretty tx)
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        x' <- flattenIfNeeded x
        y' <- flattenIfNeeded y
        return
            ( "Horizontal rule for matrix <"
            , return [essence| &x' <lex &y' |]
            )


rule_Matrix_Leq_Primitive :: Rule
rule_Matrix_Leq_Primitive = "matrix-Leq-primitive" `namedRule` theRule where
    theRule p = do
        (x,y)           <- case (match opLeq p, match opDotLeq p) of
                                (Just a, _) -> return a
                                (_, Just a) -> return a
                                _ -> na "rule_Matrix_Leq_Primitive"
        tx@TypeMatrix{} <- typeOf x        -- TODO: check if x and y have the same arity
        ty@TypeMatrix{} <- typeOf y
        unless (isPrimitiveType tx) $ fail ("not a primitive type:" <+> pretty tx)
        unless (isPrimitiveType ty) $ fail ("not a primitive type:" <+> pretty ty)
        x' <- flattenIfNeeded x
        y' <- flattenIfNeeded y
        return
            ( "Horizontal rule for matrix <="
            , return [essence| &x' <=lex &y' |]
            )


rule_Matrix_Lt_Decompose :: Rule
rule_Matrix_Lt_Decompose = "matrix-Lt-tuple" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLt p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ fail ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ fail ("this is a primitive type:" <+> pretty ty)
        xs              <- downX1 x
        ys              <- downX1 y
        return
            ( "Horizontal rule for matrix <, decomposing"
            , return $ decomposeLexLt p xs ys
            )


rule_Matrix_Leq_Decompose :: Rule
rule_Matrix_Leq_Decompose = "matrix-Leq-tuple" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opLeq p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ fail ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ fail ("this is a primitive type:" <+> pretty ty)
        xs              <- downX1 x
        ys              <- downX1 y
        return
            ( "Horizontal rule for matrix <=, decomposing"
            , return $ decomposeLexLeq p xs ys
            )


rule_Matrix_DotLt_Decompose :: Rule
rule_Matrix_DotLt_Decompose = "matrix-DotLt-tuple" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opDotLt p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ fail ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ fail ("this is a primitive type:" <+> pretty ty)
        xs              <- downX1 x
        ys              <- downX1 y
        return
            ( "Horizontal rule for matrix .<, decomposing"
            , return $ decomposeLexDotLt p xs ys
            )


rule_Matrix_DotLeq_Decompose :: Rule
rule_Matrix_DotLeq_Decompose = "matrix-DotLeq-tuple" `namedRule` theRule where
    theRule p = do
        (x,y)           <- match opDotLeq p
        tx@TypeMatrix{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeMatrix{} <- typeOf y
        when (isPrimitiveType tx) $ fail ("this is a primitive type:" <+> pretty tx)
        when (isPrimitiveType ty) $ fail ("this is a primitive type:" <+> pretty ty)
        xs              <- downX1 x
        ys              <- downX1 y
        return
            ( "Horizontal rule for matrix .<=, decomposing"
            , return $ decomposeLexDotLeq p xs ys
            )


rule_Comprehension_SingletonDomain :: Rule
rule_Comprehension_SingletonDomain = "matrix-comprehension-singleton-domain" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, singleVal), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainHasRepr patName (singletonDomainInt -> Just a)) -> return (Single patName, a)
            _ -> na "rule_Comprehension_SingletonDomain"
        let upd val old = lambdaToFunction pat old val
        return
            ( "Removing matrix-comprehension of a singleton int domain"
            , return $
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
            , return singleVal
            )


rule_Concatenate_Singleton :: Rule
rule_Concatenate_Singleton = "matrix-concatenate-singleton" `namedRule` theRule where
    theRule p = do
        AbstractLiteral (AbsLitMatrix _ [singleVal]) <- match opConcatenate p
        return
            ( "Removing concatenate of a single item"
            , return singleVal
            )


rule_MatrixIndexing :: Rule
rule_MatrixIndexing = "matrix-indexing" `namedRule` theRule where
    theRule p = do
        (matrix, indexer)            <- match opIndexing p
        (_, DomainInt ranges, elems) <- match matrixLiteral matrix
        indexInts                    <- rangesInts ranges
        indexerInt                   <- intOut "rule_MatrixIndexing" indexer
        if length indexInts == length elems
            then
                case lookup indexerInt (zip indexInts elems) of
                    Nothing -> na "rule_MatrixIndexing"
                    Just v  ->
                        return
                            ( "Matrix indexing"
                            , return v
                            )
            else na "rule_MatrixIndexing"


rule_IndexingIdentical :: Rule
rule_IndexingIdentical = "matrix-indexing-identical" `namedRule` theRule where
    theRule p = do
        (matrix, indexer)                     <- match opIndexing p
        (_, indexDomain, firstElem:restElems) <- match matrixLiteral matrix
        indexerDomain <- domainOf indexer
        if indexDomain == forgetRepr indexerDomain && all (firstElem==) restElems
            then return
                    ( "rule_IndexingIdentical"
                    , return firstElem
                    )
            else na "rule_IndexingIdentical"
