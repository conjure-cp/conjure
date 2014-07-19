{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Conjure.Language.Parser
    ( runLexerAndParser
    , parseModel
    , parseRuleRefn
    , parseRuleRepr
    , parseTopLevels
    ) where

-- conjure
import Conjure.Language.Definition
import Conjure.Language.Pretty

import Bug
import Language.E.Parser.Imports
import Language.E.Parser.EssenceFile

import Language.E.Imports


parseModel :: Parser Model
parseModel = specToModel <$> parseSpec

specToModel :: Spec -> Model
specToModel (Spec _ stmt) = Model
    { mStatements = map convStmt (statementAsList stmt)
    , mInfo =
        ModelInfo
            { miRepresentations = []
            , miTrail = []
            }
    }

    where

        convStmt [xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference
                        | [D domain     ] := topLevel.declaration.given.domain
                        |] = Declaration (Given (Name name) domain)
        convStmt [xMatch| [Prim (S name)] := topLevel.declaration.find .name.reference
                        | [D domain     ] := topLevel.declaration.find .domain
                        |] = Declaration (Find (Name name) domain)
        convStmt [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [expr         ] := topLevel.letting.expr
                        |] = Declaration (Letting (Name name) (convExpr expr))
        convStmt x = bug $ "convStmt" <+> pretty (show x)

        convExpr [xMatch| [Prim (B x)] := value.literal |] = C (ConstantBool x)
        convExpr [xMatch| [Prim (I x)] := value.literal |] = C (ConstantInt (fromInteger x))
        convExpr x = x

