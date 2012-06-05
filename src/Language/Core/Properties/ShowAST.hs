{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Core.Properties.ShowAST where

import Language.Core.Imports
import Language.Core.Definition

import PrintUtils as Pr


class ShowAST a where
    showAST :: a -> Doc

instance ShowAST Spec where
    showAST (Spec (l,is) xs) =
        vcat $ ("language" <+> text l <+> Pr.hcat (intersperse Pr.dot (map Pr.int is)))
             : map showAST xs

instance ShowAST Core where
    showAST (L x) = showAST x
    showAST (R x) = showAST x
    -- showAST (Expr t xs) = hang (showAST t) 4 (Pr.sep $ map showAST xs)
    showAST (Expr t xs) = Pr.parens $ hang (showAST t) 4 (Pr.sep $ map showAST xs)
    -- showAST (Let r ty val) = "let" <+> showAST r <+> ": `" <> showAST ty <> "` in" <+> Pr.braces (showAST val)

instance ShowAST Tag where
    showAST (Tag t) = textToDoc t

instance ShowAST Literal where
    showAST (B False) = "false"
    showAST (B True ) = "true"
    showAST (I i    ) = Pr.integer i

instance ShowAST Reference where
    showAST (Reference r) = textToDoc r

instance ShowAST a => ShowAST [a] where
    showAST = vcat . map showAST

instance ShowAST RuleRefn where
    showAST (name,mInt,core) = vcat [textToDoc name, stringToDoc $ show mInt, showAST core]