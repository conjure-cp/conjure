{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Tuple where

import Conjure.Rules.Import


rule_Tuple_Eq :: Rule
rule_Tuple_Eq = "tuple-eq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opEq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple equality"
            , return $ make opAnd $ fromList $ zipWith (make opEq) xs ys
            )


rule_Tuple_Neq :: Rule
rule_Tuple_Neq = "tuple-neq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opNeq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple !="
            , return $ make opNot $ make opAnd $ fromList $ zipWith (make opEq) xs ys
            )


rule_Tuple_Lt :: Rule
rule_Tuple_Lt = "tuple-Lt" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLt p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple <"
            , return $ decomposeLexLt p xs ys
            )


rule_Tuple_Leq :: Rule
rule_Tuple_Leq = "tuple-Leq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opLeq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple <="
            , return $ decomposeLexLeq p xs ys
            )


rule_Tuple_DotLt :: Rule
rule_Tuple_DotLt = "tuple-DotLt" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opDotLt p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple .<"
            , return $ decomposeLexDotLt p xs ys
            )


rule_Tuple_DotLeq :: Rule
rule_Tuple_DotLeq = "tuple-DotLeq" `namedRule` theRule where
    theRule p@[essence| &x .<= permute(&perm, &y) |] = do
        tx@TypeTuple{} <- typeOf x     -- TODO: check matrix index & tuple arity
        ty@TypeTuple{} <- typeOf y
        TypePermutation{} <- typeOf perm
        xs              <- downX1 x
        ys              <- downX1 y
        return
            ( "Horizontal rule for matrix .<=, decomposing"
            , return $ decomposeLexDotLeqSym p perm xs ys
            )

    theRule p = do
        (x,y)       <- match opDotLeq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple .<="
            , return $ decomposeLexDotLeq p xs ys
            )


rule_Tuple_TildeLt :: Rule
rule_Tuple_TildeLt = "tuple-TildeLt" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opTildeLt p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple .<"
            , return $ decomposeLexTildeLt p xs ys
            )


rule_Tuple_TildeLeq :: Rule
rule_Tuple_TildeLeq = "tuple-TildeLeq" `namedRule` theRule where
    theRule p = do
        (x,y)       <- match opTildeLeq p
        TypeTuple{} <- typeOf x        -- TODO: check if x and y have the same arity
        TypeTuple{} <- typeOf y
--        failExceptForTuplesOfSameArity x y
        xs          <- downX1 x
        ys          <- downX1 y
        return
            ( "Horizontal rule for tuple .<="
            , return $ decomposeLexTildeLeq p xs ys
            )

failExceptForTuplesOfSameArity :: MonadFail m => Expression -> Expression -> m () 
failExceptForTuplesOfSameArity (AbstractLiteral (AbsLitTuple a)) (AbstractLiteral (AbsLitTuple b)) = 
  if length a == length b
     then return ()
     else fail "failExceptForTuplesOfSameArity: These tuples are not of the same arity."
failExceptForTuplesOfSameArity (Domain (DomainTuple a)) (Domain (DomainTuple b)) = 
  if length a == length b
     then return ()
     else fail "failExceptForTuplesOfSameArity: These tuples are not of the same arity."
failExceptForTuplesOfSameArity _ _ = fail "failExceptForTuplesOfSameArity: These things are not tuples."


decomposeLexLt :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexLt p = unroll
    where
        unroll [a]    [b]    = [essence| &a < &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)

decomposeLexLeq :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexLeq p = unroll
    where
        unroll [a]    [b]    = [essence| &a <= &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a < &b) \/ ((&a = &b) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)


decomposeLexDotLt :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexDotLt p = unroll
    where
        unroll [a]    [b]    = [essence| &a .< &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a .< &b) \/ ((&a = &b) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)

decomposeLexDotLeq :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexDotLeq p = unroll
    where
        unroll [a]    [b]    = [essence| &a .<= &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a .< &b) \/ ((&a = &b) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)

decomposeLexDotLeqSym :: Expression -> Expression
                      -> [Expression] -> [Expression] -> Expression
decomposeLexDotLeqSym p perm = unroll
    where
        unroll [a]    [b]    = [essence| &a .<= permute(&perm, &b) |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a .< permute(&perm,&b)) \/ ((&a = permute(&perm,&b)) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)



decomposeLexTildeLt :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexTildeLt p = unroll
    where
        unroll [a]    [b]    = [essence| &a ~< &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a ~< &b) \/ ((&a = &b) /\ &rest) |]
        unroll _ _ = bug ("arity mismatch in:" <+> pretty p)

decomposeLexTildeLeq :: Expression -> [Expression] -> [Expression] -> Expression
decomposeLexTildeLeq p = unroll
    where
        unroll [a]    [b]    = [essence| &a ~<= &b |]
        unroll (a:as) (b:bs) = let rest = unroll as bs
                               in  [essence| (&a ~< &b) \/ ((&a = &b) /\ &rest) |]
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
            , return $ atNote "Tuple indexing" ts (fromInteger (iInt-1))
            )
