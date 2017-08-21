module Conjure.Representations
    ( downD, downC, up
    , downD1, downC1, up1
    , downToX1
    , reprOptions, getStructurals
    , reprsStandardOrderNoLevels, reprsStandardOrder, reprsSparseOrder
    , downX1
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Expression.Op
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Process.Enumerate
import Conjure.Representations.Combined


-- | Refine (down) an expression (X), one level (1).
downX1 :: (MonadFail m, NameGen m, EnumerateDomain m) => Expression -> m [Expression]
downX1 (Constant x) = onConstant x
downX1 (AbstractLiteral x) = onAbstractLiteral x
downX1 (Reference x (Just refTo)) = onReference x refTo
downX1 (Op x) = onOp x
downX1 (Comprehension body stmts) = do
    xs <- downX1 body
    return [Comprehension x stmts | x <- xs]
downX1 x@WithLocals{} = fail ("downX1:" <++> pretty (show x))
downX1 x = bug ("downX1:" <++> pretty (show x))

onConstant :: (MonadFail m, NameGen m, EnumerateDomain m) => Constant -> m [Expression]
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

onAbstractLiteral :: (MonadFail m, NameGen m, EnumerateDomain m) => AbstractLiteral Expression -> m [Expression]
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

onReference :: (MonadFail m, NameGen m, EnumerateDomain m) => Name -> ReferenceTo -> m [Expression]
onReference nm refTo =
    case refTo of
        Alias x                   -> downX1 x
        InComprehension{}         -> fail ("downX1.onReference.InComprehension:" <++> pretty (show nm))
        DeclNoRepr{}              -> fail ("downX1.onReference.DeclNoRepr:"      <++> pretty (show nm))
        DeclHasRepr forg _ domain -> downToX1 forg nm domain
        RecordField{}             -> fail ("downX1.onReference.RecordField:"     <++> pretty (show nm))
        VariantField{}            -> fail ("downX1.onReference.VariantField:"    <++> pretty (show nm))
        FrameUpdateVar{}          -> fail "downX1.onReference.FrameUpdateVar"

onOp :: (MonadFail m, NameGen m, EnumerateDomain m) => Op Expression -> m [Expression]
onOp p@(MkOpIndexing (OpIndexing m i)) = do
    ty <- typeOf m
    case ty of
        TypeMatrix{} -> return ()
        TypeList{}   -> return ()
        _ -> fail $ "[onOp, not a TypeMatrix or TypeList]" <+> vcat [pretty ty, pretty p]
    xs <- downX1 m
    let iIndexed x = Op (MkOpIndexing (OpIndexing x i))
    return (map iIndexed xs)
onOp (MkOpIncumbent (OpIncumbent x)) = do
    xs <- downX1 x
    return $ map (Op . MkOpIncumbent . OpIncumbent) xs
onOp op = fail ("downX1.onOp:" <++> pretty op)
