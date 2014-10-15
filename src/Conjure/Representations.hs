{-# LANGUAGE FlexibleContexts #-}

module Conjure.Representations
    ( down_, down, up
    , down1_, down1, up1
    , reprOptions
    , down1X
    ) where

-- conjure
import Conjure.Bug
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Ops
import Conjure.Language.Pretty
import Conjure.Representations.Combined


down1X :: (MonadFail m, MonadState St m) => Expression -> m [Expression]
down1X (Constant x) = onConstant x
down1X (AbstractLiteral x) = onAbstractLiteral x
down1X (Reference x (Just dom)) = onReference x dom
down1X (Op x) = onOp x
down1X x = bug ("down1X:" <++> pretty (show x))

onConstant :: MonadFail m => Constant -> m [Expression]
onConstant (ConstantTuple xs) = return (map Constant xs)
onConstant x = bug ("down1X.onConstant:" <++> pretty (show x))

onAbstractLiteral :: MonadFail m => AbstractLiteral Expression -> m [Expression]
onAbstractLiteral (AbsLitTuple xs) = return xs
onAbstractLiteral x = bug ("down1X.onAbstractLiteral:" <++> pretty (show x))

onReference :: (MonadState St m, MonadFail m) => Name -> Domain HasRepresentation Expression -> m [Expression]
onReference nm domain = do
    mpairs <- runExceptT $ down1_ (nm, domain)
    case mpairs of
        Left err -> bug err
        Right Nothing -> bug ("down1X.onReference, down1_ doesn't work:" <++> pretty nm)
        Right (Just pairs) -> return [ Reference n (Just d) | (n,d) <- pairs ]

onOp :: (MonadState St m, MonadFail m) => Ops Expression -> m [Expression]
onOp (MkOpIndexing (OpIndexing m i)) = do
    xs <- down1X m
    let iIndexed x = Op (MkOpIndexing (OpIndexing x i))
    return (map iIndexed xs)
onOp op = bug ("down1X.onOp:" <++> pretty op)
