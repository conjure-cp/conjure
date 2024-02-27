{-# LANGUAGE RecordWildCards #-}

module Conjure.Language.ParserC
  ( parseModel,
  )
where

import Conjure.Language.AST.ASTParser (parseProgram)
import Conjure.Language.AST.Syntax (ProgramTree)
import Conjure.Language.Definition
import Conjure.Language.Parser (Pipeline)
import Conjure.Language.Validator qualified as V
import Conjure.Prelude

parseModel :: Pipeline ProgramTree Model
parseModel = (parseProgram, V.validateModel, False)
