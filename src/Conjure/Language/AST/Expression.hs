module Conjure.Language.AST.Expression where
import Conjure.Prelude hiding (many)
import Text.Megaparsec
import Conjure.Language.AST.Helpers
import Conjure.Language.AST.Syntax

parseExpression :: Parser ExpressionNode
parseExpression = do
        IntLiteral . RealToken <$> intLiteral

parseExpressionStrict :: Parser ExpressionNode --can fail
parseExpressionStrict  = do
    expr <- try parseExpression
    case expr of 
        MissingExpressionNode _ -> empty
        _ -> return expr