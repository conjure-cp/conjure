module Language.E.Parser
    ( Parser
    , inCompleteFile
    , parseExpr, parseDomain
    , parseSpec, parseRuleRefn, parseRuleRepr
    , runLexerAndParser, lexAndParse, lexAndParseIO
    ) where

import Language.E.Parser.Imports
import Language.E.Parser.EssenceFile
-- import Language.E.Parser.LogFile

