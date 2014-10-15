{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Conjure.Language.Pretty
    ( Pretty(..)
    , (<++>), (<+>), (<>)
    , prettyList, prettyListDoc
    , parens, parensIf
    , renderNormal, renderWide
    , hang, hcat
    , prEmpty, prParens, prBrackets, prBraces
    , Doc
    , prettyContext
    ) where

-- conjure
import Conjure.Prelude

-- base
import Text.Printf ( printf )

-- text
import qualified Data.Text as T ( Text, unpack )

-- pretty
import Text.PrettyPrint
    ( parens, brackets, braces, empty       -- will be exported with new names
    , text                                  -- only used in this module
    , style, renderStyle, lineLength
    )


class Show a => Pretty a where
    pretty :: a -> Doc
    prettyPrec :: Int -> a -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty

instance Pretty Doc     where pretty = id
instance Pretty T.Text  where pretty = pretty . T.unpack
instance Pretty String  where pretty = text
instance Pretty ()      where pretty = pretty . show
instance Pretty Bool    where pretty = pretty . show
instance Pretty Int     where pretty = pretty . show
instance Pretty Integer where pretty = pretty . show
instance Pretty Double  where pretty x = pretty (printf "%.2f" x :: String)

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = prettyListDoc parens "," [pretty a, pretty b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pretty (a, b, c) = prettyListDoc parens "," [pretty a, pretty b, pretty c]

instance Pretty a => Pretty (Maybe a) where
    pretty Nothing  = "Nothing"
    pretty (Just x) = "Just" <+> parens (pretty x)


(<++>) :: Doc -> Doc -> Doc
a <++> b = hang a 4 b

prettyList :: Pretty a => (Doc -> Doc) -> Doc -> [a] -> Doc
prettyList wrap punc = prettyListDoc wrap punc . map pretty

prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . fsep . punctuate punc

parensIf :: Bool -> Doc -> Doc
parensIf = wrapIf parens
    where
        wrapIf :: (Doc -> Doc) -> Bool -> Doc -> Doc
        wrapIf wrap c = if c then wrap else id

renderNormal :: Pretty a => a -> String
renderNormal = renderStyle (style { lineLength = 120 }) . pretty

renderWide :: Pretty a => a -> String
renderWide = renderStyle (style { lineLength = 240 }) . pretty

prEmpty :: Doc
prEmpty = empty

prParens :: Doc -> Doc
prParens = parens

prBrackets :: Doc -> Doc
prBrackets = brackets

prBraces :: Doc -> Doc
prBraces = braces

prettyContext :: (Pretty a, Pretty b) => [(a,b)] -> [Doc]
prettyContext = map (\ (a,b) -> nest 4 $ pretty a <> ":" <+> pretty b )

