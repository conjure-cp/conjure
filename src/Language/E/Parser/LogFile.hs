module Language.E.Parser.LogFile where

import Conjure.Prelude
import Language.E.Definition
import Language.E.Parser.Imports
import Language.E.Parser.EssenceFile

parseLogRemovedDecl :: Parser E
parseLogRemovedDecl = do
    identifier <- brackets identifierText
    unless (identifier == "removedDecl") $ fail "fail!"
    tl <- parseTopLevels
    case tl of
        [i] -> return i
        _   -> fail "multiple top levels here, unexpected"

