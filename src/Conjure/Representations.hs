{-# LANGUAGE FlexibleContexts #-}

module Conjure.Representations
    ( downD, downC, up
    , downD1, downC1, up1
    , reprOptions
    , downX1
    , DownDResult(..)
    ) where

-- conjure
import Conjure.Bug
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Ops
import Conjure.Language.Pretty
import Conjure.Representations.Combined


-- | Refine (down) an expression (X), one level (1).
downX1 :: (MonadFail m, MonadState St m) => Expression -> m [Expression]
downX1 (Constant x) = onConstant x
downX1 (AbstractLiteral x) = onAbstractLiteral x
downX1 (Reference x (Just dom)) = onReference x dom
downX1 (Op x) = onOp x
downX1 x = bug ("downX1:" <++> pretty (show x))

onConstant :: MonadFail m => Constant -> m [Expression]
onConstant (ConstantTuple xs) = return (map Constant xs)
onConstant x = bug ("downX1.onConstant:" <++> pretty (show x))

onAbstractLiteral :: MonadFail m => AbstractLiteral Expression -> m [Expression]
onAbstractLiteral (AbsLitTuple xs) = return xs
onAbstractLiteral x = bug ("downX1.onAbstractLiteral:" <++> pretty (show x))

onReference :: (MonadState St m, MonadFail m) => Name -> Domain HasRepresentation Expression -> m [Expression]
onReference nm domain = do
    mpairs <- runExceptT $ downD1 (nm, domain)
    case mpairs of
        Left err -> bug err
        Right Nothing -> bug ("downX1.onReference, downD1 doesn't work:" <++> pretty nm)
        Right (Just (DownDResult pairs _cons)) -> return [ Reference n (Just d) | (n,d) <- pairs ]

onOp :: (MonadState St m, MonadFail m) => Ops Expression -> m [Expression]
onOp (MkOpIndexing (OpIndexing m i)) = do
    xs <- downX1 m
    let iIndexed x = Op (MkOpIndexing (OpIndexing x i))
    return (map iIndexed xs)
onOp op = bug ("downX1.onOp:" <++> pretty op)
