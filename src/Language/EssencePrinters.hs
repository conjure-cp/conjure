{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Language.EssencePrinters where


import Control.Applicative hiding ( empty )
import Control.Monad ( forM, msum )

import Language.Essence
import PrintUtils


prExpr :: Expr -> Maybe Doc
prExpr = prExprPrec 0

type Prec = Int

prExprPrec :: Prec -> Expr -> Maybe Doc
prExprPrec prec x = msum $ map (\ pr -> pr prec x) [prIdentifier, prValue, prGenericNode, prDomain]


prIdentifier :: Prec -> Expr -> Maybe Doc
prIdentifier _ (Identifier i) = return $ text i
prIdentifier _ _ = Nothing


--------------------------------------------------------------------------------
-- Printers for inline values --------------------------------------------------
--------------------------------------------------------------------------------

prValue :: Prec -> Expr -> Maybe Doc
prValue _ (ValueBoolean False) = return $ text "false"
prValue _ (ValueBoolean True ) = return $ text "true"
prValue _ (ValueInteger i    ) = return $ integer i
prValue _ (ValueMatrix    xs ) = brackets . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueTuple     xs ) = parens   . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueSet       xs ) = (text "set"       <+>) . braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueMSet      xs ) = (text "mset"      <+>) . braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValueFunction  xs ) = (text "function"  <+>) . braces . sep . punctuate comma <$> elements
    where elements = forM xs $ \ (i,j) -> do i' <- prExpr i
                                             j' <- prExpr j
                                             return (i' <+> text "->" <+> j')
prValue _ (ValueRelation  xs ) = (text "relation"  <+>) . braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ (ValuePartition xss) = (text "partition" <+>) . braces . sep . punctuate comma <$> mapM elements xss
    where elements xs = braces . sep . punctuate comma <$> mapM prExpr xs
prValue _ _ = Nothing


prDomain :: Prec -> Expr -> Maybe Doc
prDomain _ _ = Nothing


prGenericNode :: Prec -> Expr -> Maybe Doc
prGenericNode prec (GenericNode op xs) = prOpExpr prec (opDescriptor op) xs
prGenericNode _ _ = Nothing


prOpExpr :: Int -> OpDescriptor -> [Expr] -> Maybe Doc
prOpExpr _ OpSpecial _ = error "OpSpecial"
prOpExpr _ (OpLispy {face}) xs = do
    elements <- mapM prExpr xs
    return $ text face <+> parens (sep elements)
prOpExpr p (OpInfixL {face,precedence}) [a,b] = parensIf (p > precedence) <$> do
    a' <- prExprPrec precedence a
    b' <- prExprPrec (precedence+1) b
    return $ sep [a', text face, b']
prOpExpr p (OpInfixN {face,precedence}) [a,b] = parensIf (p > precedence) <$> do
    a' <- prExprPrec precedence a
    b' <- prExprPrec precedence b
    return $ sep [a', text face, b']
prOpExpr p (OpInfixR {face,precedence}) [a,b] = parensIf (p > precedence) <$> do
    a' <- prExprPrec (precedence+1) a
    b' <- prExprPrec precedence b
    return $ sep [a', text face, b']
prOpExpr p (OpPrefix {face,precedence}) [a]   = parensIf (p > precedence) <$> do
    a' <- prExprPrec precedence a
    return $ sep [text face, a']
prOpExpr _ _ _ = error "prOpExpr"
