{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Stuff.Pretty ( Pretty(..)
                    , (<++>), (<+>), (<>)
                    , prettyList, prettyListDoc
                    , parensIf
                    , renderPretty
                    ) where

import qualified Data.Text as T
import Text.PrettyPrint


class Pretty a where
    pretty :: a -> Doc

instance Pretty T.Text  where pretty = pretty . T.unpack
instance Pretty String  where pretty = text
instance Pretty ()      where pretty = pretty . show
instance Pretty Int     where pretty = pretty . show
instance Pretty Integer where pretty = pretty . show
instance Pretty Bool    where pretty = pretty . show

(<++>) :: Doc -> Doc -> Doc
a <++> b = hang a 4 b

prettyList :: Pretty a => (Doc -> Doc) -> Doc -> [a] -> Doc
prettyList wrap punc = prettyListDoc wrap punc . map pretty

prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . sep . punctuate punc

parensIf :: Bool -> Doc -> Doc
parensIf = wrapIf parens
    where
        wrapIf :: (Doc -> Doc) -> Bool -> Doc -> Doc
        wrapIf wrap c = if c then wrap else id

renderPretty :: Pretty a => a -> String
-- renderPretty = renderStyle (style { lineLength = 160 }) . pretty
renderPretty = renderStyle (style { lineLength = 120 }) . pretty
