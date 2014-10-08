{-# OPTIONS_GHC -fno-warn-orphans #-}

module Conjure.Language.Pretty
    ( module Stuff.Pretty
    , prettyContext
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Stuff.Pretty
import Language.E.Data ( Fixity(..), operators )
import Language.E.Lexer ( textToLexeme )

-- pretty
import Text.PrettyPrint as Pr

-- aeson
import Data.Aeson.Encode.Pretty ( encodePretty )

-- bytestring
import Data.ByteString.Lazy.Char8 ( unpack )


instance Pretty Model where
    pretty (Model lang stmts info) = vcat $ concat
        [ [pretty lang]
        , [""]
        , map pretty stmts
        , [""]
        , [pretty info | info /= def]
        ]

instance Pretty LanguageVersion where
    pretty (LanguageVersion language version) =
        "language" <+> pretty language
                   <+> Pr.hcat (intersperse "." (map pretty version))

instance Pretty Statement where
    pretty (Declaration x) = pretty x
    pretty (SearchOrder nms) = "branching on" <++> prettyList Pr.brackets "," nms
    pretty (Where xs) = "where" <++> vcat (punctuate comma $ map pretty xs)
    pretty (Objective obj x) = pretty obj <++> pretty x
    pretty (SuchThat xs) = "such that" <++> vcat (punctuate comma $ map pretty xs)

instance Pretty Declaration where
    pretty (FindOrGiven h nm d) = hang (pretty h <+> pretty nm <>  ":" ) 8 (pretty d)
    pretty (Letting nm x) = hang ("letting" <+> pretty nm <+> "be") 8 (pretty x)
    pretty (LettingDomainDefn (DDEnum (DomainDefnEnum name values))) =
        if null values
            then hang ("given"   <+> pretty name) 8 "new type enum"
            else hang ("letting" <+> pretty name <+> "be new type enum") 8
                   (prettyList Pr.braces "," values)
    pretty (LettingDomainDefn (DDUnnamed (DomainDefnUnnamed name size))) =
        hang ("letting" <+> pretty name <+> "be new type of size") 8 (pretty size)

instance Pretty FindOrGiven where
    pretty Find = "find"
    pretty Given = "given"

instance Pretty Objective where
    pretty Minimising = "minimising"
    pretty Maximising = "maximising"

instance Pretty ModelInfo where
    pretty = pretty . commentLines . unpack . encodePretty
        where commentLines = unlines . map ("$ "++) . ("Conjure's" :) . lines

instance Pretty Name where
    pretty (Name n) = pretty n

instance Pretty Constant where
    pretty (ConstantBool False) = "false"
    pretty (ConstantBool True) = "true"
    pretty (ConstantInt x) = pretty x
    pretty (ConstantEnum _ x) = pretty x
    pretty (ConstantTuple xs) = (if length xs < 2 then "tuple" else Pr.empty) <+> prettyList Pr.parens "," xs
    pretty (ConstantMatrix index xs) = let f i = Pr.brackets (i <> ";" <+> pretty index) in prettyList f "," xs
    pretty (ConstantSet       xs ) =                prettyList Pr.braces "," xs
    pretty (ConstantMSet      xs ) = "mset"      <> prettyList Pr.parens "," xs
    pretty (ConstantFunction  xs ) = "function"  <> prettyListDoc Pr.parens "," [ pretty a <+> "-->" <+> pretty b | (a,b) <- xs ]
    pretty (ConstantRelation  xss) = "relation"  <> prettyListDoc Pr.parens "," [ pretty (ConstantTuple xs)       | xs <- xss   ]
    pretty (ConstantPartition xss) = "partition" <> prettyListDoc Pr.parens "," [ prettyList Pr.braces "," xs     | xs <- xss   ]

instance Pretty a => Pretty (AbstractLiteral a) where
    pretty (AbsLitTuple xs) = (if length xs < 2 then "tuple" else Pr.empty) <+> prettyList Pr.parens "," xs
    pretty (AbsLitMatrix index xs) = let f i = Pr.brackets (i <> ";" <+> pretty index) in prettyList f "," xs
    pretty (AbsLitSet       xs ) =                prettyList Pr.braces "," xs
    pretty (AbsLitMSet      xs ) = "mset"      <> prettyList Pr.parens "," xs
    pretty (AbsLitFunction  xs ) = "function"  <> prettyListDoc Pr.parens "," [ pretty a <+> "-->" <+> pretty b | (a,b) <- xs ]
    pretty (AbsLitRelation  xss) = "relation"  <> prettyListDoc Pr.parens "," [ pretty (AbsLitTuple xs)         | xs <- xss   ]
    pretty (AbsLitPartition xss) = "partition" <> prettyListDoc Pr.parens "," [ prettyList Pr.braces "," xs     | xs <- xss   ]

instance Pretty Expression where
    pretty (Constant x) = pretty x
    pretty (AbstractLiteral x) = pretty x
    pretty (Domain x) = "`" <> pretty x <> "`"
    pretty (Reference x) = pretty x

    pretty (WithLocals x ss) =
        Pr.braces $ pretty x <+> "@" <+> vcat (map pretty ss)

    pretty (Lambda arg x) = "lambda" <> Pr.parens (fsep [pretty arg, "-->", pretty x])

    pretty x@(Op (Name op) [_,_])
        | let lexeme = textToLexeme op
        , lexeme `elem` [ Just l | (l,_,_) <- operators ]
        = prettyPrec 0 x

    pretty (Op (Name "indexing") [a,b])
        = pretty a <> Pr.brackets (pretty b)
    -- pretty x@(Op (Name "indexing") _)
    --     = pretty actual <> prettyListDoc Pr.brackets Pr.comma (map pretty indices)
    --     where
    --         (actual,indices) = second reverse $ collect x
    --         collect (Op (Name "indexing") [a,b]) = second (b:) $ collect a
    --         collect b = (b,[])

    pretty (Op op xs) = pretty op <> prettyList Pr.parens "," xs

prettyPrec :: Int -> Expression -> Doc
prettyPrec envPrec (Op (Name op) [a,b])
    | let lexeme = textToLexeme op
    , lexeme `elem` [ Just l | (l,_,_) <- operators ]
    = case lexeme of
        Nothing -> bug "prettyPrec"
        Just l  -> case [ (fixity,prec) | (l',fixity,prec) <- operators, l == l' ] of
            [(FLeft ,prec)] -> parensIf (envPrec > prec) $ Pr.fsep [ prettyPrec  prec    a
                                                                   , pretty op
                                                                   , prettyPrec (prec+1) b
                                                                   ]
            [(FNone ,prec)] -> parensIf (envPrec > prec) $ Pr.fsep [ prettyPrec (prec+1) a
                                                                   , pretty op
                                                                   , prettyPrec (prec+1) b
                                                                   ]
            [(FRight,prec)] -> parensIf (envPrec > prec) $ Pr.fsep [ prettyPrec  prec    a
                                                                   , pretty op
                                                                   , prettyPrec (prec+1) b
                                                                   ]
            _ -> bug "prettyPrec"
prettyPrec _ x = pretty x

instance Pretty AbstractPattern where
    pretty (Single nm TypeAny) = pretty nm
    pretty (Single nm ty     ) = pretty nm <+> ":" <+> "`" <> pretty ty <> "`"
    pretty (AbsPatTuple    xs) = (if length xs <= 1 then "tuple" else empty)
                              <> prettyList Pr.parens "," xs
    pretty (AbsPatMatrix   xs) = prettyList Pr.brackets "," xs
    pretty (AbsPatSet      xs) = prettyList Pr.braces "," xs

instance Pretty Type where
    pretty TypeAny = "?"
    pretty TypeBool = "bool"
    pretty TypeInt = "int"
    pretty (TypeEnum (DomainDefnEnum nm _)) = pretty nm
    pretty (TypeUnnamed (DomainDefnUnnamed nm _)) = pretty nm
    pretty (TypeTuple xs) = (if length xs <= 1 then "tuple" else empty)
                         <> prettyList Pr.parens "," xs
    pretty (TypeMatrix index inner) = "matrix indexed by"
                                  <+> Pr.brackets (pretty index)
                                  <+> "of" <+> pretty inner
    pretty (TypeSet x) = "set of" <+> pretty x
    pretty (TypeMSet x) = "mset of" <+> pretty x
    pretty (TypeFunction fr to) = "function" <+> pretty fr <+> "-->" <+> pretty to
    pretty (TypePartition x) = "partition from" <+> pretty x
    pretty (TypeRelation xs) = prettyList Pr.parens " *" xs

instance Pretty DomainDefnUnnamed where
    pretty (DomainDefnUnnamed name size) =
        hang ("letting" <+> pretty name <+> "be new type of size") 8 (pretty size)

instance Pretty DomainDefnEnum where
    pretty (DomainDefnEnum name values) =
        hang ("letting" <+> pretty name <+> "be new type enum"   ) 8 (prettyList Pr.braces "," values)


instance (Pretty r, Pretty a) => Pretty (Domain r a) where
    -- domain.*

    pretty DomainBool = "bool"

    pretty (DomainInt []) = "int"
    pretty (DomainInt ranges) = "int" <> prettyList Pr.parens "," ranges

    pretty (DomainEnum (DomainDefnEnum name _) []) = pretty name
    pretty (DomainEnum (DomainDefnEnum name _) ranges) = pretty name <> prettyList Pr.parens "," ranges

    pretty (DomainUnnamed (DomainDefnUnnamed name _)) = pretty name

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

    pretty (DomainSet r attrs inner) =
        hang ("set" <+> prettyAttrs r attrs <+> "of") 4 (pretty inner)

    pretty (DomainMSet r attrs inner) =
        hang ("mset" <+> prettyAttrs r attrs <+> "of") 4 (pretty inner)

    pretty (DomainFunction r attrs innerFrom innerTo) =
        hang ("function" <+> prettyAttrs r attrs) 4 $
            hang (pretty innerFrom) 4 $
                "-->" <+> pretty innerTo

    pretty (DomainRelation r attrs inners)
        = hang ("relation" <+> prettyAttrs r attrs <+> "of") 4 (prettyList Pr.parens " *" inners)

    pretty (DomainPartition r attrs inner)
        = hang ("partition" <+> prettyAttrs r attrs <+> "from") 4 (pretty inner)

    pretty (DomainOp op xs) = Pr.parens $ foldr (<+>) empty $ intersperse (pretty op) (map pretty xs)

    pretty (DomainHack x) = pretty x

prettyAttrs :: (Pretty a, Pretty b) => a -> b -> Doc
prettyAttrs a bs =
    let prettya = pretty a
    in  if prettya == "()"
            then pretty bs
            else Pr.braces prettya <+> pretty bs

instance Pretty a => Pretty (SetAttr a) where
    pretty SetAttrNone = empty
    pretty (SetAttrSize       a  ) = Pr.parens ("size"    <+> pretty a)
    pretty (SetAttrMinSize    a  ) = Pr.parens ("minSize" <+> pretty a)
    pretty (SetAttrMaxSize    a  ) = Pr.parens ("maxSize" <+> pretty a)
    pretty (SetAttrMinMaxSize a b) = Pr.parens ("minSize" <+> pretty a <+> ", maxSize" <+> pretty b)

instance Pretty a => Pretty (DomainAttributes a) where
    pretty (DomainAttributes []) = empty
    pretty (DomainAttributes attrs) = prettyList Pr.parens "," attrs

instance Pretty a => Pretty (DomainAttribute a) where
    pretty (DAName name) = pretty name
    pretty (DANameValue name value) = pretty name <+> pretty value
    pretty DADotDot = ".."

instance Pretty a => Pretty (Range a) where
    pretty RangeOpen = ".."
    pretty (RangeSingle x) = pretty x
    pretty (RangeLowerBounded x) = pretty x <> ".."
    pretty (RangeUpperBounded x) = ".." <> pretty x
    pretty (RangeBounded x y) = pretty x <> ".." <> pretty y

instance Pretty HasRepresentation where
    pretty NoRepresentation = "∅"
    pretty (HasRepresentation r) = pretty r

prettyContext :: (Pretty a, Pretty b) => [(a,b)] -> [Doc]
prettyContext = map (\ (a,b) -> nest 4 $ pretty a <> ":" <+> pretty b )

