{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Conjure.Language.Parser
    ( runLexerAndParser
    , parseModel
    , parseRuleRefn
    , parseRuleRepr
    , parseTopLevels
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Language.E ( Spec(..), E(..), BuiltIn(..), xMatch, viewTaggeds, statementAsList )

import Language.E.Parser.Imports
import Language.E.Parser.EssenceFile



parseModel :: Parser Model
parseModel = specToModel <$> parseSpec

specToModel :: Spec -> Model
specToModel (Spec lang stmt) = Model
    { mLanguage = lang
    , mStatements = map convStmt (statementAsList stmt)
    , mInfo =
        ModelInfo
            { miRepresentations = []
            , miTrail = []
            }
    }

    where

        convStmt :: E -> Statement
        convStmt [xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference
                        | [D domain     ] := topLevel.declaration.given.domain
                        |] = Declaration (Given (Name name) (convDomain domain))
        convStmt [xMatch| [Prim (S name)] := topLevel.declaration.find .name.reference
                        | [D domain     ] := topLevel.declaration.find .domain
                        |] = Declaration (Find (Name name) (convDomain domain))
        convStmt [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                        | [expr         ] := topLevel.letting.expr
                        |] = Declaration (Letting (Name name) (convExpr expr))
        convStmt x = bug $ "convStmt" <+> pretty (show x)

        convExpr :: E -> Expression
        convExpr [xMatch| [Prim (B x)] := value.literal |] = Constant (ConstantBool x)
        convExpr [xMatch| [Prim (I x)] := value.literal |] = Constant (ConstantInt (fromInteger x))
        convExpr x = bug $ "convExpr" <+> pretty (show x)

        convDomain :: Domain () E -> Domain () Expression
        convDomain = fmap convExpr
