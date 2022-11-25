{-# LANGUAGE RecordWildCards #-}

module Conjure.Language.ParserC (   
    parseModel ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition

import qualified Conjure.Language.Validator as V
import Conjure.Language.AST.Syntax (ProgramTree)
import Conjure.Language.Parser (Pipeline)
import Conjure.Language.AST.ASTParser (parseProgram)





parseModel :: Pipeline ProgramTree Model
parseModel = (parseProgram,V.strict . V.validateModel,False)