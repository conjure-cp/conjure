{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.E.Pretty ( module Stuff.Pretty, prettySpecDebug ) where

import Utils.DebugPretty
import Stuff.Pretty
import Language.E.Definition
import Language.E.Data ( Fixity(..), operators )
import Language.E.Lexer ( textToLexeme )

import Control.Arrow ( first, second )
import Data.List ( intersperse )
import Text.PrettyPrint as Pr


prettySpecDebug :: Spec -> Doc
prettySpecDebug sp@(Spec _ st) = vcat $ pretty sp : map prettyAsPaths (statementAsList st)

instance Pretty RuleRefn where
    pretty ruleRefn = pretty (show ruleRefn)

instance Pretty RuleRepr where
    pretty (RuleRepr _ reprName domOut mcons lcls cases) =
        vcat $ [ "~~>" <+> pretty reprName
               , "~~>" <+> pretty domOut
               , maybe Pr.empty (\ x -> "~~>" <+> pretty x) mcons
               ] ++ map (nest 4 . pretty) lcls
                 ++ map pretty cases

instance Pretty RuleReprCase where
    pretty (RuleReprCase domIn mcons lcls) =
        vcat $ [ "***" <+> pretty domIn
               , maybe Pr.empty (\ x -> "~~>" <+> pretty x) mcons
               ] ++ map (nest 4 . pretty) lcls

instance Pretty Spec where
    pretty (Spec (LanguageVersion language version) statements)
        = vcat [ "language" <+> pretty language
                            <+> Pr.hcat (intersperse "." (map Pr.int version))
               , ""
               , pretty statements
               , ""
               ]

instance Pretty [E] where
    pretty = vcat . map pretty

instance Pretty E where

    -- pretty x | trace (show $ "pretty: " $+$ prettyAsPaths x) False = undefined

    pretty (D d) = pretty d
    pretty EOF = empty
    pretty (StatementAndNext this next) = pretty this $$ pretty next

    pretty (Prim (B False)) = "false"
    pretty (Prim (B True )) = "true"
    pretty (Prim x) = pretty x

    pretty [xMatch| [] := emptyGuard |] = "emptyGuard"

    -- slicers
    pretty [xMatch| [] := slicer     |] = ".."

    pretty [xMatch| [Prim (S x)] := metavar       |] = "&" <> pretty x
    pretty [xMatch| [Prim (S x)] := reference     |] = pretty x

    -- top levels
    pretty [xMatch| [ name ] := topLevel.declaration.given.name
                  | [domain] := topLevel.declaration.given.domain
                  |]
        = hang ("given" <+> pretty name <> ":") 8 (pretty domain)

    pretty [xMatch| [ name ] := topLevel.declaration.find.name
                  | [domain] := topLevel.declaration.find.domain
                  |]
        = hang ("find"  <+> pretty name <> ":") 8 (pretty domain)

    pretty [xMatch| [ name ] := topLevel.declaration.dim.name
                  | [domain] := topLevel.declaration.dim.domain
                  |]
        = hang ("dim"   <+> pretty name <> ":") 8 (pretty domain)

    pretty [xMatch| [ name ] := topLevel.declaration.given.name
                  | [      ] := topLevel.declaration.given.typeEnum
                  |]
        = hang ("given" <+> pretty name) 8 "new type enum"

    pretty [xMatch| [ name ] := topLevel.declaration.given.name
                  | [      ] := topLevel.declaration.given.typeInt
                  |]
        = hang ("given" <+> pretty name) 8 "new domain int"

    pretty [xMatch| [ name ] := dimFind.name
                  | [domain] := dimFind.domain
                  |]
        = hang ("find"  <+> pretty name <> ":") 8 (pretty domain)

    pretty [xMatch| [x] := topLevel.declaration.nestedDimFind |]
        = pretty [xMake| atTopLevel := [x] |]

    pretty [xMatch| [ name ] := topLevel.letting.name
                  | [thingy] := topLevel.letting.expr
                  |]
        = hang ("letting" <+> pretty name <+> "be") 8 (pretty thingy)

    pretty [xMatch| [ name ] := topLevel.letting.name
                  | [thingy] := topLevel.letting.domain
                  |]
        = hang ("letting" <+> pretty name <+> "be domain") 8 (pretty thingy)

    pretty [xMatch| [ name ] := topLevel.letting.name
                  | [thingy] := topLevel.letting.lambda
                  |]
        = hang ("letting" <+> pretty name <+> "be lambda") 8 (pretty thingy)

    pretty [xMatch| [ name ] := topLevel.letting.name
                  | [thingy] := topLevel.letting.quantifier
                  |]
        = hang ("letting" <+> pretty name <+> "be") 8 (pretty thingy)

    pretty [xMatch| [ name ] := topLevel.letting.name
                  |  values  := topLevel.letting.typeEnum.values
                  |]
        = hang ("letting" <+> pretty name <+> "be new type enum") 8
               (prettyList Pr.braces "," values)

    pretty [xMatch| [ name ] := topLevel.letting.name
                  | [thingy] := topLevel.letting.typeUnnamed
                  |]
        = hang ("letting" <+> pretty name <+> "be new type of size") 8
               (pretty thingy)

    pretty [xMatch| [x] := topLevel.objective.minimising |]
        = "minimising" <+> pretty [xMake| atTopLevel := [x] |]

    pretty [xMatch| [x] := topLevel.objective.maximising |]
        = "maximising" <+> pretty [xMake| atTopLevel := [x] |]

    pretty [xMatch| xs := topLevel.where |]
        = let xs' = [ [xMake| atTopLevel := [x] |] | x <- xs ] in
            "where" <++> vcat (punctuate comma $ map pretty xs')

    pretty [xMatch| xs := topLevel.suchThat |]
        = let xs' = [ [xMake| atTopLevel := [x] |] | x <- xs ] in
            "such that" <++> vcat (punctuate comma $ map pretty xs')

    pretty [xMatch| [x] := topLevel.branchingOn |]
        = "branching on" <+> pretty [xMake| atTopLevel := [x] |]

    pretty [xMatch| [actual] := withLocals.actual 
                  | locals   := withLocals.locals
                  |]
        = let locals' = [ [xMake| atTopLevel := [x] |] | x <- locals ] in
            Pr.braces $ pretty actual <+> "@" <+> vcat (map pretty locals')

    pretty [xMatch| [a] := typed.left
                  | [b] := typed.right
                  |]
        = Pr.parens $ pretty a <+> ":" <+> pretty b


-- type.*
    pretty [xMatch| [ ] := type.unknown    |] = "?"
    pretty [xMatch| [ ] := type.bool       |] = "bool"
    pretty [xMatch| [ ] := type.int        |] = "int"
    pretty [xMatch| [x] := type.set.inner  |] = "set of"  <+> pretty x
    pretty [xMatch| [x] := type.mset.inner |] = "mset of" <+> pretty x

    pretty [xMatch| [Prim (S nm)] := type.typeEnum |] = pretty nm

    pretty [xMatch| [a] := type.function.innerFrom
                  | [b] := type.function.innerTo
                  |] = "function" <+> pretty a <+> "-->" <+> pretty b

    pretty [xMatch| [   index   ] := type.matrix.index
                  | [innerNested] := type.matrix.inner
                  |]
        = "matrix indexed by" <+> prettyList Pr.brackets "," indices
                              <+> "of" <+> pretty inner
        where
            (indices,inner) = first (index:) $ collect innerNested
            collect [xMatch| [i] := type.matrix.index
                           | [j] := type.matrix.inner
                           |] = first (i:) $ collect j
            collect x = ([],x)

    pretty [xMatch| ts := type.tuple.inners |] =
        if length ts >= 2
            then prettyList Pr.parens "," ts
            else "tuple" <+> prettyList Pr.parens "," ts
    pretty [xMatch| ts := type.relation.inners |] =
        "relation" <+> prettyList Pr.parens "," ts
    pretty [xMatch| [x] := type.partition.inner |] = "partition from" <+> pretty x

-- domain.*

    pretty [xMatch| [d] := domainInExpr |] = "`" <> pretty d <> "`"
    pretty [xMatch| xs  := domain.binOp |] = pretty [xMake| binOp := xs |]

    pretty [xMatch| [Prim (S x)] := domain.reference   |] = pretty x

-- value.*

    pretty [xMatch| [x] := value.literal      |] = pretty x
    pretty [xMatch| xs  := value.tuple.values |]
        = (if length xs < 2 then "tuple" else Pr.empty)
        <+> prettyList Pr.parens "," xs
    pretty [xMatch| xs  := value.matrix  .values
                  | [d] := value.matrix.indexrange
                  |] = let f i = Pr.brackets (i <> ";" <+> pretty d)
                       in  prettyList f "," xs
    pretty [xMatch| xs := value.matrix   .values |] =                prettyList Pr.brackets "," xs
    pretty [xMatch| xs := value.set      .values |] =                prettyList Pr.braces   "," xs
    pretty [xMatch| xs := value.mset     .values |] = "mset"      <> prettyList Pr.parens   "," xs
    pretty [xMatch| xs := value.function .values |] = "function"  <> prettyList Pr.parens   "," xs
    pretty [xMatch| xs := value.relation .values |] = "relation"  <> prettyList Pr.parens   "," xs
    pretty [xMatch| xs := value.partition.values |] = "partition" <> prettyList Pr.parens   "," xs
    pretty [xMatch| xs := part                   |] =                prettyList Pr.braces   "," xs

    pretty [xMatch| [a,b] := mapping |] = pretty a <+> "-->" <+> pretty b

    pretty [xMatch| [app] := quantifierDecl.append
                  | [gua] := quantifierDecl.guard
                  | [ide] := quantifierDecl.identity
                  |]
        = "quantifier" Pr.$$ Pr.braces (
                    Pr.nest 4 ("append  " <+> pretty app) Pr.$$
                    Pr.nest 4 ("guard   " <+> pretty gua) Pr.$$
                    Pr.nest 4 ("identity" <+> pretty ide)
                    )

    pretty [xMatch| [x] := lambda.param
                 | [y] := lambda.body
                 |]
        = Pr.braces $ pretty x <+> "-->" <+> pretty y

    pretty [xMatch| [x] := structural.single |] = pretty x
    pretty [xMatch| xs  := structural.tuple  |] = prettyList Pr.parens "," xs
    pretty [xMatch| xs  := structural.matrix |] = prettyList Pr.brackets "," xs
    pretty [xMatch| xs  := structural.set    |] = prettyList Pr.braces "," xs

    -- :atTopLevel is only used to indicate whether we want a Pr.parens
    -- around a expr-quantified or not.
    pretty [xMatch| [x] := atTopLevel |] = prettyAtTopLevel x

    pretty x@[xMatch| _ := quantified |] = Pr.parens $ prettyQuantified x
    pretty [xMatch| [n] := quanVar.name
                  | [w] := quanVar.within
                  |] = braces (pretty n <+> "|" <+> pretty w)

    pretty [xMatch| [x] := unaryOp.negate |]
        = "-" <> prettyPrec 10000 x

    pretty [xMatch| [x] := unaryOp.factorial |]
        = prettyPrec 10000 x <> "!"

    pretty [xMatch| [x] := unaryOp.not |]
        = "!" <> prettyPrec 10000 x

    pretty [xMatch| [x] := operator.twoBars |]
        = "|" <> pretty x <> "|"

    pretty [xMatch| [a,b] := operator.indices |]
        = "indices" <> parens (pretty a <> "," <+> pretty b)

    pretty x@[xMatch| _ := operator.index |]
        = pretty actual <> prettyListDoc Pr.brackets Pr.comma (map pretty indices)
        where
            (actual,indices) = second reverse $ collect x
            collect [xMatch| [a] := operator.index.left
                           | [b] := operator.index.right |] = second (b:) $ collect a
            collect b = (b,[])

    pretty [xMatch| [ a ] := operator.replace.arg1
                  | [old] := operator.replace.old
                  | [new] := operator.replace.new
                  |] = parens $ parens (pretty a) <+> braces ( pretty old <+> "-->" <+> pretty new )

    pretty [xMatch| [] := binOp.in       |] = "in"
    pretty [xMatch| [] := binOp.subset   |] = "subset"
    pretty [xMatch| [] := binOp.subsetEq |] = "subsetEq"

    pretty x@[xMatch| [Prim (S t)] := binOp.operator
                    | [_] := binOp.left
                    | [_] := binOp.right
                    |]
        | let lexeme = textToLexeme t
        , lexeme `elem` [ Just l | (l,_,_) <- operators ]
        = prettyPrec 0 x

    pretty [xMatch| xs := operator.dontCare     |] = "dontCare"     <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.allDiff      |] = "allDiff"      <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.apart        |] = "apart"        <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.defined      |] = "defined"      <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.flatten      |] = "flatten"      <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.freq         |] = "freq"         <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.hist         |] = "hist"         <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.inverse      |] = "inverse"      <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.max          |] = "max"          <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.min          |] = "min"          <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.normIndices  |] = "normIndices"  <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.participants |] = "participants" <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.parts        |] = "parts"        <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.party        |] = "party"        <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.preImage     |] = "preImage"     <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.range        |] = "range"        <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.together     |] = "together"     <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.toInt        |] = "toInt"        <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.toMSet       |] = "toMSet"       <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.toRelation   |] = "toRelation"   <> prettyList Pr.parens "," xs
    pretty [xMatch| xs := operator.toSet        |] = "toSet"        <> prettyList Pr.parens "," xs

    pretty [xMatch| [actual] := functionApply.actual
                  |   args   := functionApply.args
                  |]
        = pretty actual <> prettyList Pr.parens "," args

    pretty (Tagged t xs) = "{-#" <+> pretty t <++> vcat (map pretty xs) <+> "#-}"


instance Pretty Domain where
    -- domain.*

    pretty DomainBool = "bool"

    pretty (DomainInt []) = "int"
    pretty (DomainInt ranges) = "int" <> prettyList Pr.parens "," ranges

    pretty (DomainEnum name []) = pretty name
    pretty (DomainEnum name ranges) = pretty name <> prettyList Pr.parens "," ranges

    pretty (DomainTuple inners)
        = (if length inners < 2 then "tuple" else Pr.empty)
        <+> prettyList Pr.parens "," inners

    pretty (DomainMatrix index innerNested)
        = "matrix indexed by" <+> prettyList Pr.brackets "," indices
                              <+> "of" <+> pretty inner
        where
            (indices,inner) = first (index:) $ collect innerNested
            collect (DomainMatrix i j) = first (i:) $ collect j
            collect x = ([],x)

    pretty (DomainSet attrs inner) =
        hang ("set" <+> pretty attrs <+> "of") 4 (pretty inner)

    pretty (DomainMSet attrs inner) =
        hang ("mset" <+> pretty attrs <+> "of") 4 (pretty inner)

    pretty (DomainFunction attrs innerFrom innerTo) =
        hang ("function" <+> pretty attrs) 4 $
            hang (pretty innerFrom) 4 $
                "-->" <+> pretty innerTo

    pretty (DomainRelation attrs inners)
        = hang ("relation" <+> pretty attrs <+> "of") 4 (prettyList Pr.parens " *" inners)

    pretty (DomainPartition attrs inner)
        = hang ("partition" <+> pretty attrs <+> "from") 4 (pretty inner)

    -- 
    -- pretty [xMatch| [ x ] := range.single |] = pretty x
    -- pretty [xMatch| [ x ] := range.from   |] = pretty x <> ".."
    -- pretty [xMatch| [ x ] := range.to     |] = ".." <> pretty x
    -- pretty [xMatch| [x,y] := range.fromTo |] = pretty x <> ".." <> pretty y

instance Pretty DomainAttributes where
    pretty (DomainAttributes []) = empty
    pretty (DomainAttributes attrs) = prettyList Pr.parens "," attrs

instance Pretty DomainAttribute where
    pretty (DAName name) = pretty name
    pretty (DANameValue name value) = pretty name <+> pretty value
    pretty DADotDot = ".."

instance Pretty Range where
    pretty (RangeSingle x) = pretty x
    pretty (RangeLowerBounded x) = pretty x <> ".."
    pretty (RangeUpperBounded x) = ".." <> pretty x
    pretty (RangeBounded x y) = pretty x <> ".." <> pretty y


prettyPrec :: Int -> E -> Doc
prettyPrec envPrec x@([xMatch| [Prim (S t)] := binOp.operator
                             | [a]          := binOp.left
                             | [b]          := binOp.right
                             |])
    | let lexeme = textToLexeme t
    , lexeme `elem` [ Just l | (l,_,_) <- operators ]
    = case lexeme of
        Nothing -> prettyAsTree x
        Just l  -> case [ (fixity,prec) | (l',fixity,prec) <- operators, l == l' ] of
            [(FLeft ,prec)] -> parensIf (envPrec > prec) $ Pr.sep [ prettyPrec  prec    a
                                                                  , pretty t
                                                                  , prettyPrec (prec+1) b
                                                                  ]
            [(FNone ,prec)] -> parensIf (envPrec > prec) $ Pr.sep [ prettyPrec (prec+1) a
                                                                  , pretty t
                                                                  , prettyPrec (prec+1) b
                                                                  ]
            [(FRight,prec)] -> parensIf (envPrec > prec) $ Pr.sep [ prettyPrec  prec    a
                                                                  , pretty t
                                                                  , prettyPrec (prec+1) b
                                                                  ]
            _ -> error $ show $ "error in prettyPrec:" <+> prettyAsTree x
-- prettyPrec _ c | trace (show $ "prettyPrec" <+> prettyAsPaths c) False = undefined
prettyPrec _ x = pretty x

prettyAtTopLevel :: E -> Doc
prettyAtTopLevel x@[xMatch| _ := quantified |] = prettyQuantified x
prettyAtTopLevel x = pretty x

prettyQuantified :: E -> Doc
prettyQuantified
    [xMatch| [quantifier] := quantified.quantifier
           | [var]        := quantified.quanVar
           | overDom      := quantified.quanOverDom
           | overOp       := quantified.quanOverOp
           | overExpr     := quantified.quanOverExpr
           | guard        := quantified.guard
           | body         := quantified.body
           |] = 
    let
        header =  pretty quantifier
              <+> pretty var
              <+> ( case overDom of
                        [i] -> Pr.colon  <+> pretty i
                        _ -> Pr.empty
                  )
              <+> ( case (overOp,overExpr) of
                        ([op], [i]) -> pretty op <+> pretty i
                        _ -> Pr.empty
                  )
        hangGuard x = case guard of
                        [ [xMatch| [] := emptyGuard |] ] -> x
                        [ i                            ] -> x <++> (Pr.comma <+> pretty i)
                        _                                -> x
        hangBody x = x <++> ("." <+> pretty body)
    in hangBody $ hangGuard header
prettyQuantified x = prettyNotImplemented x

prettyNotImplemented :: E -> Doc
prettyNotImplemented (Tagged s _) = "{{" <> pretty s <> "}}"
prettyNotImplemented x = "[pretty] catch all case" <++> prettyAsPaths x

instance DebugPretty E where
    debugPretty = pretty
