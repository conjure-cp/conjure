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
prDomain _ DomainBoolean = return $ text "bool"
prDomain _ (DomainIntegerFromTo fr to) = do
    frDom <- maybe (return empty) prExpr fr
    toDom <- maybe (return empty) prExpr to
    return $ text "int" <> parens (frDom <> text ".." <> toDom)
prDomain _ (DomainIntegerList []) = return $ text "int"
prDomain _ (DomainIntegerList xs) = do
    xs' <- mapM prExpr xs
    return $ text "int" <> parens (sep (punctuate comma xs'))
prDomain _ (DomainUnnamed {theSize,representation}) = do
    s <- prExpr theSize
    return $ text "new"
         <+> text "type"
         <+> maybe empty (\ r -> parens (text "representation" <+> text r) ) representation
         <+> text "of"
         <+> text "size"
         <+> s
prDomain _ (DomainEnum {enums,representation}) =
    return $ text "enum"
         <+> maybe empty (\ r -> parens (text "representation" <+> text r) ) representation
         <+> braces (sep (punctuate comma (map text enums)))
prDomain _ (DomainMatrix i j) = do
    is' <- mapM prExpr is
    k'  <- prExpr k
    return $ text "matrix"
         <+> text "indexed"
         <+> text "by"
         <+> brackets (sep (punctuate comma is'))
         <+> text "of"
         <+> k'
    where
        (is,k) = helper i j
        helper a (DomainMatrix b c) = let (d,e) = helper b c in (a:d,e)
        helper a b = ([a],b)
prDomain _ (DomainTuple {components,representation}) = do
    cs <- mapM prExpr components
    return $ text "tuple"
         <+> maybe empty (\ r -> parens (text "representation" <+> text r) ) representation
         <+> text "of"
         <+> parens (sep (punctuate comma cs))
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
