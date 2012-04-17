{-# LANGUAGE FlexibleContexts #-}

module ParsePrint
    ( ParsePrint(..)
    , prettyList, prettyListDoc
    , runParser
    ) where

import Language.EssenceLexer
import Language.EssenceLexerP
import PrintUtils


-- a class for those data types which can be parsed and pretty-printed.
-- probably a better idea to parameterise this by the Parser type to make it
-- more general.

class (Eq a, Show a) => ParsePrint a where
    parse  :: Parser a
    pretty :: a -> Doc
    fromPairs :: [(a,Lexeme)]

    parse     = msum1 [ do lexeme s; return a | (a,s) <- fromPairs ]
    pretty a  = case lookup a fromPairs of
                    Nothing -> error $ "Cannot render: " ++ show a
                    Just l  -> lexemeFace l
    fromPairs = error "fromPairs not defined."


prettyList :: ParsePrint a => (Doc -> Doc) -> Doc -> [a] -> Doc
prettyList wrap punc = prettyListDoc wrap punc . map pretty


prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . sep . punctuate punc

