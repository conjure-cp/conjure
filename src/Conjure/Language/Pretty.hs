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

-- pretty
import Text.PrettyPrint as Pr

-- aeson
import Data.Aeson.Encode.Pretty ( encodePretty )

-- bytestring
import Data.ByteString.Lazy.Char8 ( unpack )


instance Pretty Model where
    pretty (Model lang stmts info) = vcat $ concat
        [ [pretty lang, ""]
        , map pretty stmts
        , ["", "$ Conjure's"]
        , [pretty info]
        ]

instance Pretty LanguageVersion where
    pretty (LanguageVersion language version) =
        "language" <+> pretty language
                   <+> Pr.hcat (intersperse "." (map pretty version))

instance Pretty Statement where
    pretty (Declaration x) = pretty x
    pretty (Where x) = "where" <+> pretty x
    pretty (Objective obj x) = pretty obj <+> pretty x
    pretty (SuchThat x) = "such that" <+> pretty x

instance Pretty Declaration where
    pretty (Find    nm d) = hang ("find"    <+> pretty nm <>  ":" ) 8 (pretty d)
    pretty (Given   nm d) = hang ("given"   <+> pretty nm <>  ":" ) 8 (pretty d)
    pretty (Letting nm x) = hang ("letting" <+> pretty nm <+> "be") 8 (pretty x)

instance Pretty Objective where
    pretty Minimising = "minimising"
    pretty Maximising = "maximising"

instance Pretty ModelInfo where
    pretty = pretty . commentLines . unpack . encodePretty . toJSON
        where commentLines = unlines . map ("$ "++) . lines

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

    pretty (DomainEnum name []) = pretty name
    pretty (DomainEnum name ranges) = pretty name <> prettyList Pr.parens "," ranges

    pretty (DomainUnnamed name) = pretty name

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
    pretty attr =
        let
            prettyNoDotDot SetAttrDotDot{} = bug "nested SetAttrDotDot"
            prettyNoDotDot SetAttrNone = ""
            prettyNoDotDot (SetAttrSize       a  ) = "size" <+> pretty a
            prettyNoDotDot (SetAttrMinSize    a  ) = "minSize" <+> pretty a
            prettyNoDotDot (SetAttrMaxSize    a  ) = "maxSize" <+> pretty a
            prettyNoDotDot (SetAttrMinMaxSize a b) = "minSize" <+> pretty a <+> ", maxSize" <+> pretty b
        in
            case attr of
                SetAttrNone -> ""
                SetAttrDotDot without -> Pr.parens (prettyNoDotDot without <+> ", ..")
                _ -> Pr.parens (prettyNoDotDot attr)

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

