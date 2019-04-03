{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations
    ( downD, downC, up
    , downD1, downC1, up1
    , downToX1
    , reprOptions, getStructurals
    , symmetryOrdering
    , reprsStandardOrderNoLevels, reprsStandardOrder, reprsSparseOrder
    , downX1
    , downX
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Process.Enumerate
import Conjure.Compute.DomainOf
import Conjure.Representations.Combined


-- | Refine (down) an expression (X), one level (1).
downX1 ::
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m [Expression]
downX1 (Constant x) = onConstant x
downX1 (AbstractLiteral x) = onAbstractLiteral x
downX1 (Reference x (Just refTo)) = onReference x refTo
downX1 (Op x) = onOp x
downX1 (Comprehension body stmts) = do
    xs <- downX1 body
    return [Comprehension x stmts | x <- xs]
downX1 x@WithLocals{} = fail ("downX1:" <++> pretty (show x))
downX1 x = bug ("downX1:" <++> pretty (show x))


-- | Refine (down) an expression (X), all the way.
downX ::
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m [Expression]
downX x = do
    res <- runMaybeT $ downX1 x
    case res of
        Nothing -> return [x]
        Just [] -> return [x]
        Just xs -> concatMapM downX xs


onConstant ::
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Constant -> m [Expression]
onConstant (ConstantAbstract (AbsLitTuple xs)) = return (map Constant xs)
onConstant (ConstantAbstract (AbsLitRecord xs)) = return (map (Constant . snd) xs)
onConstant (ConstantAbstract (AbsLitVariant (Just t) n x))
    | Just i <- elemIndex n (map fst t)
    , let iExpr = fromInt (fromIntegral (i+1))
    = return $ iExpr : [ if n == n'
                            then Constant x
                            else ExpressionMetaVar "zeroVal for variant"
                       | (n',_) <- t ]
onConstant (ConstantAbstract (AbsLitMatrix index xs)) = do
    yss <- mapM (downX1 . Constant) xs
    let indexX = fmap Constant index
    return [ AbstractLiteral (AbsLitMatrix indexX ys) | ys <- transpose yss ]
onConstant (TypedConstant c _) = onConstant c
onConstant x = bug ("downX1.onConstant:" <++> pretty (show x))

onAbstractLiteral ::
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    AbstractLiteral Expression -> m [Expression]
onAbstractLiteral (AbsLitTuple xs) = return xs
onAbstractLiteral (AbsLitRecord xs) = return (map snd xs)
onAbstractLiteral (AbsLitVariant (Just t) n x)
    | Just i <- elemIndex n (map fst t)
    , let iExpr = fromInt (fromIntegral (i+1))
    = return $ iExpr : [ if n == n'
                            then x
                            else ExpressionMetaVar "zeroVal for variant"
                       | (n',_) <- t ]
onAbstractLiteral (AbsLitMatrix index xs) = do
    yss <- mapM downX1 xs
    return [ AbstractLiteral (AbsLitMatrix index ys) | ys <- transpose yss ]
onAbstractLiteral x = bug ("downX1.onAbstractLiteral:" <++> pretty (show x))

onReference ::
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Name -> ReferenceTo -> m [Expression]
onReference nm refTo =
    case refTo of
        Alias x                   -> downX1 x
        InComprehension{}         -> fail ("downX1.onReference.InComprehension:" <++> pretty (show nm))
        DeclNoRepr{}              -> fail ("downX1.onReference.DeclNoRepr:"      <++> pretty (show nm))
        DeclHasRepr forg _ domain -> downToX1 forg nm domain
        RecordField{}             -> fail ("downX1.onReference.RecordField:"     <++> pretty (show nm))
        VariantField{}            -> fail ("downX1.onReference.VariantField:"    <++> pretty (show nm))

onOp ::
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Op Expression -> m [Expression]
onOp p@(MkOpIndexing (OpIndexing m i)) = do
    ty <- typeOf m
    case ty of
        TypeMatrix{} -> return ()
        TypeList{}   -> return ()
        _ -> fail $ "[onOp, not a TypeMatrix or TypeList]" <+> vcat [pretty ty, pretty p]
    xs <- downX1 m
    let iIndexed x = Op (MkOpIndexing (OpIndexing x i))
    return (map iIndexed xs)
onOp op = fail ("downX1.onOp:" <++> pretty op)



symmetryOrdering ::
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m Expression
symmetryOrdering inp =
    case inp of
        -- Constant x -> so_onConstant x
        -- AbstractLiteral x
--        AbstractLiteral x -> do
--          case x of
--            AbsLitTuple xs -> do
--              soVals <- sequence (symmetryOrdering <$> xs)
--              return $ make opFlatten (fromList soVals)
--            _ -> bug ("symmetryOrdering: AbstractLiteral:" <++> pretty (show inp) <++> pretty (inp))
--

        Reference _ (Just refTo) -> do
            case refTo of
                Alias x                        -> symmetryOrdering x
                InComprehension{}              -> bug ("symmetryOrdering.InComprehension:" <++> pretty (show inp))
                DeclNoRepr{}                   -> bug ("symmetryOrdering.DeclNoRepr:"      <++> pretty (show inp))
                DeclHasRepr _forg _name domain -> symmetryOrderingDispatch downX1 inp domain
                RecordField{}                  -> bug ("symmetryOrdering.RecordField:"     <++> pretty (show inp))
                VariantField{}                 -> bug ("symmetryOrdering.VariantField:"    <++> pretty (show inp))
        Op op -> case op of
            MkOpIndexing (OpIndexing m _) -> do
                ty <- typeOf m
                case ty of
                    TypeMatrix{} -> return ()
                    TypeList{}   -> return ()
                    _ -> bug $ "[symmetryOrdering.onOp, not a TypeMatrix or TypeList]" <+> vcat [pretty ty, pretty op]
                mDom <- domainOfR m
                case mDom of
                    DomainMatrix _ domainInner -> symmetryOrderingDispatch downX1 inp domainInner
                    _ -> bug ("symmetryOrdering, not DomainMatrix:" <++> pretty (show op))
            _ -> bug ("symmetryOrdering, no OpIndexing:" <++> pretty (show op))
        Comprehension body stmts -> do
            xs <- symmetryOrdering body
            return $ make opFlatten $ Comprehension xs stmts
        -- x@WithLocals{} -> bug ("downX1:" <++> pretty (show x))
        _ -> bug ("symmetryOrdering:" <++> pretty (show inp) <++> pretty (inp))

