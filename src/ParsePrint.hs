module ParsePrint
    ( ParsePrint(..), fromPairs
    , prettyList, prettyListDoc
    ) where

import Data.Maybe ( fromMaybe )

import ParsecUtils
import PrintUtils


-- a class for those data types which can be parsed and pretty-printed.
-- probably a better idea to parameterise this by the Parser type to make it
-- more general.

class ParsePrint a where
    parse  :: Parser a
    pretty :: a -> Doc
    isoParsePrint :: (Parser a, a -> Doc)

    parse = fst isoParsePrint
    pretty = snd isoParsePrint
    isoParsePrint = (parse, pretty)


fromPairs :: (Eq a, Show a) => [(a,String)] -> (Parser a, a -> Doc)
fromPairs pairs
    = ( choiceTry [ do reserved s; return a | (a,s) <- pairs ]
      , \ a -> text $ fromMaybe (error ("Cannot render: " ++ show a)) (lookup a pairs)
      )


prettyList :: ParsePrint a => (Doc -> Doc) -> Doc -> [a] -> Doc
prettyList wrap punc = prettyListDoc wrap punc . map pretty


prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . sep . punctuate punc

