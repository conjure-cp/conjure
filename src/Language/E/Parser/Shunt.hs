{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Parser.Shunt ( shuntingYardExpr, shuntingYardDomain ) where

import Language.E.Parser.Imports
import Language.E.Imports
import Language.E.Definition
import Language.E.Data ( Fixity(..), operators )
import Language.E.Lexer ( Lexeme(..), lexemeText )


shuntingYardExpr :: Parser [Either Lexeme E] -> Parser E
shuntingYardExpr = shuntingYard $ \ op before after ->
    [xMake| binOp.operator := [Prim (S $ lexemeText op)]
          | binOp.left     := [before]
          | binOp.right    := [after]
          |]

shuntingYardDomain :: Parser [Either Lexeme E] -> Parser E
shuntingYardDomain = shuntingYard $ \ op before after ->
    [xMake| domain.binOp.operator := [Prim (S $ lexemeText op)]
          | domain.binOp.left     := [before]
          | domain.binOp.right    := [after]
          |]

shuntingYard :: (Lexeme -> E -> E -> E) -> Parser [Either Lexeme E] -> Parser E
shuntingYard mergeOp beforeShunt = do
    xs <- fixNegate <$> beforeShunt
    if not $ checkAlternating xs
        then fail "Malformed expression, Shunting Yard failed."
        else shunt mergeOp xs

fixNegate :: [Either Lexeme E] -> [Either Lexeme E]
fixNegate ( Right a
          : Right ([xMatch| [b] := unaryOp.negate |])
          : cs
          ) = fixNegate $ Right a : Left L_Minus : Right b : cs
fixNegate (a:bs) = a : fixNegate bs
fixNegate [] = []

checkAlternating :: [Either a b] -> Bool
checkAlternating [Right _] = True
checkAlternating (Right _:Left _:rest) = checkAlternating rest
checkAlternating _ = False

shunt :: (Lexeme -> E -> E -> E) -> [Either Lexeme E] -> Parser E
shunt mergeOp xs = do
    result <- findPivotOp xs
    case result of
        Left x -> return x
        Right (before, op, after) -> do
            b <- shunt mergeOp before
            a <- shunt mergeOp after
            return (mergeOp op b a)

findPivotOp :: [Either Lexeme E] -> Parser (Either E ([Either Lexeme E], Lexeme, [Either Lexeme E]))
findPivotOp [Right x] = return $ Left x
findPivotOp xs = do
    let
        pivotPrec :: Int
        pivotFixity :: Fixity
        (pivotPrec,pivotFixity) = minimumBy (comparing fst)
                        [ (p, f) | Left l <- xs, (l',f,p) <- operators, l == l' ]

        chck op = case [ p | (l,_,p) <- operators, l == op ] of
                    [p] -> p == pivotPrec
                    _ -> False

        findFirst :: [Either Lexeme E] -> Parser ([Either Lexeme E], Lexeme, [Either Lexeme E])
        findFirst [] = fail "findPivotOp.findFirst"
        findFirst (Left i:is) | chck i = return ([], i, is)
        findFirst (i:is) = do
            (before, op, after) <- findFirst is
            return (i:before, op, after)

        findLast :: [Either Lexeme E] -> Parser ([Either Lexeme E], Lexeme, [Either Lexeme E])
        findLast is = do
            (before, op, after) <- findFirst (reverse is)
            return (reverse after, op, reverse before)

        findOnly :: [Either Lexeme E] -> Parser ([Either Lexeme E], Lexeme, [Either Lexeme E])
        findOnly is = do
            f <- findFirst is
            l <- findLast  is
            if f == l
                then return f
                else fail "Ambiguous use of non-associative operator."

    let
        finder = case pivotFixity of
                    FLeft  -> findLast
                    FNone  -> findOnly
                    FRight -> findFirst
    Right <$> finder xs

