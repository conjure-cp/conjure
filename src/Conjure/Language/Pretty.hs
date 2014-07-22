{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Conjure.Language.Pretty
    ( module Language.E.Pretty
    ) where

-- conjure
import Conjure.Language.Definition
import Bug
import Language.E.Pretty

-- pretty
import Text.PrettyPrint as Pr


instance Pretty Model where
    pretty = bug "pretty Model"

instance Pretty Expression where
    pretty (Constant x) = pretty x
    pretty (Reference x) = pretty x
    pretty (Op op xs) = pretty op <> prettyList Pr.parens "," xs
    pretty (Lambda nm ty x _) = "lambda" <> Pr.parens (pretty nm <+> ":" <+> pretty ty <+> "-->" <+> pretty x)

instance Pretty Type where
    pretty TypeBool = "bool"
    pretty TypeInt = "int"
    pretty (TypeEnum (DomainDefnEnum nm _)) = pretty nm
    pretty (TypeUnnamed (DomainDefnUnnamed nm _)) = pretty nm
    pretty (TypeTuple xs) = (if length xs <= 1 then "tuple" else "")
                         <> prettyList Pr.parens "," xs
    pretty (TypeMatrix index inner) = "matrix indexed by"
                                  <+> Pr.brackets (pretty index)
                                  <+> "of" <+> pretty inner
    pretty (TypeSet x) = "set of" <+> pretty x
    pretty (TypeMSet x) = "mset of" <+> pretty x
    pretty (TypeFunction fr to) = "function" <+> pretty fr <+> "-->" <+> pretty to
    pretty (TypePartition x) = "partition from" <+> pretty x
    pretty (TypeRelation xs) = prettyList Pr.parens " *" xs

