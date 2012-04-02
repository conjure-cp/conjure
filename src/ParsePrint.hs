{-# LANGUAGE FlexibleContexts #-}

module ParsePrint
    ( ParsePrint(..)
    , prettyList, prettyListDoc
    , runParser
    ) where

import Control.Monad.Error ( MonadError, throwError )
import Data.Maybe ( fromMaybe )
import qualified Text.Parsec as P ( parse )

import ParsecUtils
import PrintUtils


-- a class for those data types which can be parsed and pretty-printed.
-- probably a better idea to parameterise this by the Parser type to make it
-- more general.

class (Eq a, Show a) => ParsePrint a where
    parse  :: Parser a
    pretty :: a -> Doc
    fromPairs :: [(a,String)]

    parse     = choiceTry [ do reserved s; return a | (a,s) <- fromPairs ]
    pretty a  = text $ fromMaybe (error ("Cannot render: " ++ show a)) (lookup a fromPairs)
    fromPairs = error "fromPairs not defined."


prettyList :: ParsePrint a => (Doc -> Doc) -> Doc -> [a] -> Doc
prettyList wrap punc = prettyListDoc wrap punc . map pretty


prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . sep . punctuate punc


runParser :: (ParsePrint a, MonadError Doc m) => FilePath -> String -> m a
runParser fp s = case P.parse parse fp s of
    Left msg -> throwError $ text $ show msg
    Right a  -> return a
