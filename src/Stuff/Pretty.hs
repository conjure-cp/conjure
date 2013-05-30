{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Stuff.Pretty ( Pretty(..)
                    , (<++>), (<+>), (<>)
                    , prettyList, prettyListDoc
                    , parensIf
                    , renderNormal, renderWide
                    , Doc
                    ) where

import qualified Data.Text as T
import Text.PrettyPrint

import Text.Printf (printf)


class Pretty a where
    pretty :: a -> Doc

instance Pretty Doc     where pretty = id
instance Pretty T.Text  where pretty = pretty . T.unpack
instance Pretty String  where pretty = text
instance Pretty ()      where pretty = pretty . show
instance Pretty Bool    where pretty = pretty . show
instance Pretty Int     where pretty = pretty . show
instance Pretty Integer where pretty = pretty . show
instance Pretty Double  where pretty x = pretty (printf "%.2f" x :: String)

instance (Pretty a, Pretty b) => Pretty (a,b) where
    pretty (a,b) = prettyListDoc parens "," [pretty a, pretty b]

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

