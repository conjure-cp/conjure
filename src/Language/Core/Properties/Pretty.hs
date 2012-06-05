{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.Pretty where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.EssenceLexer ( textToLexeme )

import Text.PrettyPrint as Pr
import qualified Data.Text as T

(<++>) :: Doc -> Doc -> Doc
a <++> b = hang a 4 b

class Pretty a where
    pretty :: a -> Doc

instance Pretty Spec where
    pretty (Spec (language,version) cs) = vcat
        $  ("language" <+> text language <+> Pr.hcat (intersperse "." (map Pr.int version)))
        : ""
        : map pretty cs
        ++ [""]

instance Pretty Core where

    -- pretty c | trace (show $ "pretty" <+> showAST c) False = undefined

    pretty (L x) = pretty x
    pretty (R x) = pretty x

    pretty ( viewDeep [":metavar"] -> Just [R x]) = "@" <> pretty x

    pretty ( viewDeep [":toplevel",":declaration",":given"]
              -> Just [ Expr ":given-name" [name]
                      , Expr ":given-domain" [domain]
                      ]
           ) = "given" <+> pretty name <> Pr.colon <+> pretty domain
    pretty ( viewDeep [":toplevel",":declaration",":find"]
              -> Just [ Expr ":find-name" [name]
                      , Expr ":find-domain" [domain]
                      ]
           ) = "find" <+> pretty name <> Pr.colon <+> pretty domain
    pretty ( viewDeep [":toplevel",":declaration",":dim"]
              -> Just [ Expr ":dim-name" [name]
                      , Expr ":dim-domain" [domain]
                      ]
           ) = "dim" <+> pretty name <> Pr.colon <+> pretty domain

    pretty ( viewDeep [":toplevel",":declaration",":given"]
              -> Just [ Expr ":given-name" [name]
                      , Expr ":given-typeenum" []
                      ]
           ) = "given" <+> pretty name <+> "new type enum"
    pretty ( viewDeep [":toplevel",":declaration",":nested-dimfind"] -> Just [x] )
        = pretty (Expr ":atTopLevel" [x])
    pretty ( viewDeep [":dimfind"]
              -> Just [ Expr ":dimfind-name"   [name]
                      , Expr ":dimfind-domain" [domain]
                      ]
           ) = "find" <+> pretty name <+> Pr.colon <+> pretty domain


    pretty ( viewDeep [":toplevel",":letting"]
              -> Just [ Expr ":letting-name" [name]
                      , Expr ":letting-expr" [expr]
                      ]
           ) = "letting" <+> pretty name <+> "be" <+> pretty expr

    pretty ( viewDeep [":toplevel",":letting"]
              -> Just [ Expr ":letting-name"   [name]
                      , Expr ":letting-domain" [domain]
                      ]
           ) = "letting" <+> pretty name <+> "be domain" <+> pretty domain

    pretty ( viewDeep [":toplevel",":letting"]
              -> Just [ Expr ":letting-name"       [name]
                      , Expr ":letting-lambda" [quan]
                      ]
           ) = "letting" <+> pretty name <+> "be lambda" <+> pretty quan

    pretty ( viewDeep [":toplevel",":letting"]
              -> Just [ Expr ":letting-name"       [name]
                      , Expr ":letting-quantifier" [quan]
                      ]
           ) = "letting" <+> pretty name <+> "be" <+> pretty quan

    pretty ( viewDeep [":toplevel",":letting"]
              -> Just [ Expr ":letting-name"       [name]
                      , Expr ":letting-type-enum"  [enum]
                      ]
           ) = "letting" <+> pretty name <+> "be new type enum" <+> pretty enum
    pretty ( viewDeep [":toplevel",":letting"]
              -> Just [ Expr ":letting-name"         [name]
                      , Expr ":letting-type-unnamed" [expr]
                      ]
           ) = "letting" <+> pretty name <+> "be new type of size" <+> pretty expr


    pretty ( viewDeep [":type-enum-values"] -> Just xs )
        = prettyListDoc Pr.braces Pr.comma (map pretty xs)



    pretty ( viewDeep [":toplevel",":objective",":minimising"] -> Just [x] )
        = "minimising" <+> pretty x

    pretty ( viewDeep [":toplevel",":objective",":maximising"] -> Just [x] )
        = "maximising" <+> pretty x

    pretty ( viewDeep [":toplevel",":where"] -> Just xs )
        = let xs' = [ Expr ":atTopLevel" [x] | x <- xs ] in
            "where" <++> vcat (punctuate comma $ map pretty xs')

    pretty ( viewDeep [":toplevel",":suchthat"] -> Just xs )
        = let xs' = [ Expr ":atTopLevel" [x] | x <- xs ] in
            "such that" <++> vcat (punctuate comma $ map pretty xs')

    pretty ( viewDeep [":domain"]
              -> Just [R r]
           ) = pretty r

    pretty ( viewDeep [":domain",":domain-bool"]
              -> Just []
           ) = "bool"

    pretty ( viewDeep [":domain",":domain-int",":domain-int-ranges"] -> Just ranges )
        = if null ranges
            then "int"
            else "int" <> prettyListDoc Pr.parens Pr.comma (map pretty ranges)

    pretty ( viewDeep [":domain",":domain-enum"]
              -> Just [ Expr ":domain-enum-name"  [name]
                      ] )
        = pretty name
    pretty ( viewDeep [":domain",":domain-enum"]
              -> Just [ Expr ":domain-enum-name"  [name]
                      , Expr ":domain-enum-range" [range]
                      ] )
        = pretty name <> Pr.parens (pretty range)

    pretty ( viewDeep [":domain",":domain-tuple",":domain-tuple-inners"] -> Just inners )
        =   (if length inners < 2 then "tuple" else Pr.empty)
        <+> prettyListDoc Pr.parens Pr.comma (map pretty inners)

    pretty ( viewDeep [":domain",":domain-matrix"]
              -> Just [ Expr ":domain-matrix-index" [index]
                      , Expr ":domain-matrix-inner" [innerNested]
                      ]
           )
        = "matrix indexed by" <+> prettyListDoc Pr.brackets Pr.comma (map pretty indices)
                              <+> "of" <+> pretty inner
        where
            (indices,inner) = first (index:) $ collect innerNested
            collect ( viewDeep [":domain",":domain-matrix"]
                       -> Just [ Expr ":domain-matrix-index" [i]
                               , Expr ":domain-matrix-inner" [j]
                               ]
                    ) = first (i:) $ collect j
            collect x = ([],x)

    pretty ( viewDeep [":domain",":domain-function"]
              -> Just [ Expr ":domain-function-attributes" [attrs]
                      , Expr ":domain-function-innerfrom"  [innerFrom]
                      , Expr ":domain-function-innerto"    [innerTo]
                      ]
           ) = "function" <+> pretty attrs
                          <+> pretty innerFrom <+> "-->"
                          <+> pretty innerTo

    pretty ( viewDeep [":domain",":domain-set"]
              -> Just [ Expr ":domain-set-attributes" [attrs]
                      , Expr ":domain-set-inner"      [inner]
                      ]
           ) = "set" <+> pretty attrs <+> "of" <+> pretty inner

    pretty ( viewDeep [":domain",":domain-mset"]
              -> Just [ Expr ":domain-mset-attributes" [attrs]
                      , Expr ":domain-mset-inner"      [inner]
                      ]
           ) = "mset" <+> pretty attrs <+> "of" <+> pretty inner

    pretty ( viewDeep [":domain",":domain-relation"]
              -> Just [ Expr ":domain-relation-attributes" [attrs]
                      , Expr ":domain-relation-inners"     inners
                      ]
           ) = "relation" <+> pretty attrs <+> "of" <+> prettyListDoc Pr.parens " *" (map pretty inners)

    pretty ( viewDeep [":domain",":domain-partition"]
              -> Just [ Expr ":domain-partition-attributes" [attrs]
                      , Expr ":domain-partition-inner"      [inner]
                      ]
           ) = "partition" <+> pretty attrs <+> "from" <+> pretty inner


    pretty ( viewDeep [":attributes"] -> Just as )
        = if null as
            then empty
            else prettyListDoc Pr.parens Pr.comma (map pretty as)

    pretty ( viewDeep [":attribute",":attribute-namevalue"] -> Just [name,value] )
        = pretty name <+> pretty value
    pretty ( viewDeep [":attribute-namevalue-name" ] -> Just [R x]) = pretty x
    pretty ( viewDeep [":attribute-namevalue-value"] -> Just [ x ]) = pretty x

    pretty ( viewDeep [":attribute",":attribute-name"] -> Just [name] )
        = pretty name
    pretty ( viewDeep [":attribute-name-name"] -> Just [x]) = pretty x


    pretty ( viewDeep [":range",":range-single"] -> Just [x]   ) = pretty x
    pretty ( viewDeep [":range",":range-from"  ] -> Just [x]   ) = pretty x <> ".."
    pretty ( viewDeep [":range",":range-to"    ] -> Just [x]   ) = ".." <> pretty x
    pretty ( viewDeep [":range",":range-fromto"] -> Just [x,y] ) = pretty x <> ".." <> pretty y

    pretty ( viewDeep [":value",":value-literal"] -> Just [L (B False)]) = "false"
    pretty ( viewDeep [":value",":value-literal"] -> Just [L (B True )]) = "true"
    pretty ( viewDeep [":value",":value-literal"] -> Just [L (I x)]) = Pr.integer x
    pretty ( viewDeep [":value",":value-tuple"  ] -> Just xs )
        =   (if length xs < 2 then "tuple" else Pr.empty)
        <+> prettyListDoc Pr.parens Pr.comma (map pretty xs)
    pretty ( viewDeep [":value",":value-set"  ] -> Just xs )
        =   prettyListDoc Pr.braces Pr.comma (map pretty xs)

    pretty ( viewDeep [":quantifier"]
              -> Just [ Expr ":quantifier-append"   [app]
                      , Expr ":quantifier-guard"    [gua]
                      , Expr ":quantifier-identity" [ide]
                      ]
           ) = "quantifier" Pr.$$ Pr.braces (
                    Pr.nest 4 ("append  " <+> pretty app) Pr.$$
                    Pr.nest 4 ("guard   " <+> pretty gua) Pr.$$
                    Pr.nest 4 ("identity" <+> pretty ide)
                    )


    pretty ( viewDeep [":lambda"]
              -> Just [ Expr ":lambda-param" [x]
                      , Expr ":lambda-body"  [y]
                      ]
           ) = Pr.braces $ pretty x <+> "-->" <+> pretty y


    pretty ( viewDeep [":structural-single"] -> Just [x] )
        = pretty x
    pretty ( viewDeep [":structural-tuple"] -> Just xs )
        = prettyListDoc Pr.parens Pr.comma (map pretty xs)
    pretty ( viewDeep [":structural-matrix"] -> Just xs )
        = prettyListDoc Pr.brackets Pr.comma (map pretty xs)

    -- :atTopLevel is only used to indicate whether we want a Pr.parens
    -- around a expr-quantified or not.
    pretty ( viewDeep [":atTopLevel",":expr-quantified"] -> Just xs )
        = prettyQuantified xs
    pretty ( viewDeep [":atTopLevel"] -> Just [x] ) = pretty x

    pretty ( viewDeep [":expr-quantified"] -> Just xs )
        = Pr.parens $ prettyQuantified xs

    -- pretty (Expr ":operator-=" xs) = vcat $ map pretty xs
    -- pretty (Expr ":operator-index" xs) = vcat $ map pretty xs

    pretty ( viewDeep [":function-apply"]
              -> Just [ Expr ":function-apply-actual" [x]
                      , Expr ":function-apply-args"   xs
                      ]
           ) = pretty x <> prettyListDoc Pr.parens Pr.comma (map pretty xs)

    pretty (viewDeep [":operator-negate"] -> Just [x])
        = "-" <> prettyPrec 10000 x

    pretty (viewDeep [":operator-not"] -> Just [x])
        = "!" <> prettyPrec 10000 x

    pretty (viewDeep [":operator-twobars"] -> Just [x])
        = "|" <> pretty x <> "|"

    pretty x@(viewDeep [":operator-index"] -> Just _)
        = pretty actual <> prettyListDoc Pr.brackets Pr.comma (map pretty indices)
        where
            (actual,indices) = second reverse $ collect x
            collect (Expr ":operator-index" [a,b]) = second (b:) $ collect a
            collect b = (b,[])

    pretty (Expr (Tag t) args)
        | Just rest <- T.stripPrefix ":operator-" t
        , let lexeme = textToLexeme rest
        , lexeme `elem` map Just functionals
        = textToDoc rest <> prettyListDoc Pr.parens Pr.comma (map pretty args)

    pretty x@(Expr (Tag t) [_,_])
        | Just rest <- T.stripPrefix ":operator-" t
        , let lexeme = textToLexeme rest
        , lexeme `elem` [ Just l | (l,_,_) <- operators ]
        = prettyPrec 0 x

    pretty (Expr (Tag t) [])
        | Just rest <- T.stripPrefix ":operator-" t
        = textToDoc rest

    pretty x = showAST x
    -- pretty x = Pr.braces $ "from showAST" <+> stringToDoc (show x)
    -- pretty _ = "foo"
    -- pretty ( view -> ( []
    --                  , []
    --                  )
    --        ) = undefined


instance Pretty Reference where
    pretty (Reference t) = textToDoc t

instance Pretty Literal where
    pretty = showAST


prettyPrec :: Int -> Core -> Doc
-- prettyPrec _ c | trace (show $ "prettyPrec" <+> showAST c) False = undefined
prettyPrec envPrec x@(Expr (Tag t) [a,b])
    | Just rest <- T.stripPrefix ":operator-" t
    , let lexeme = textToLexeme rest
    , lexeme `elem` [ Just l | (l,_,_) <- operators ]
    = case lexeme of
        Nothing -> showAST x
        Just l  -> case [ (fixity,prec) | (l',fixity,prec) <- operators, l == l' ] of
            [(FLeft ,prec)] -> parensIf (envPrec > prec) $ Pr.sep [ prettyPrec  prec    a
                                                                  , textToDoc rest
                                                                  , prettyPrec (prec+1) b
                                                                  ]
            [(FNone ,prec)] -> parensIf (envPrec > prec) $ Pr.sep [ prettyPrec  prec    a
                                                                  , textToDoc rest
                                                                  , prettyPrec (prec+1) b
                                                                  ]
            [(FRight,prec)] -> parensIf (envPrec > prec) $ Pr.sep [ prettyPrec  prec    a
                                                                  , textToDoc rest
                                                                  , prettyPrec (prec+1) b
                                                                  ]
            _ -> error $ show $ "error in prettyPrec:" <+> showAST x
prettyPrec _ x = pretty x

prettyQuantified :: [Core] -> Doc
prettyQuantified xs = 
    let
        quantifier = justOne $ lookUpInExpr ":expr-quantified-quantifier"   xs
        var        = justOne $ lookUpInExpr ":expr-quantified-quanVar"      xs
        overDom    =           lookUpInExpr ":expr-quantified-quanOverDom"  xs
        overOp     =           lookUpInExpr ":expr-quantified-quanOverOp"   xs
        overExpr   =           lookUpInExpr ":expr-quantified-quanOverExpr" xs
        guard      =           lookUpInExpr ":expr-quantified-guard"        xs
        body       = justOne $ lookUpInExpr ":expr-quantified-body"         xs

        justOne (Just [i]) = i
        justOne i = error $ "(justOne)\n" ++ ppShow i

        header =  pretty quantifier
              <+> pretty var
              <+> ( case overDom of
                        Just [i] -> Pr.colon  <+> pretty i
                        _ -> Pr.empty
                  )
              <+> ( case (overOp,overExpr) of
                        (Just [op], Just [i]) -> pretty op <+> pretty i
                        _ -> Pr.empty
                  )
        hangGuard x = case guard of
                        Just [i] -> x <++> (Pr.comma <+> pretty i)
                        _        -> x
        hangBody x = x <++> ("." <+> pretty body)
    in hangBody $ hangGuard header
