{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Parser.EssenceFile.Expr
    ( parseExpr
    , parseQuantifiedExpr
    , parseMetaVariable
    , parseReference
    ) where

import Bug
import Language.E.Parser.Imports
import Language.E.Parser.Shunt ( shuntingYardExpr )
import {-# SOURCE #-} Language.E.Parser.EssenceFile.Domain ( parseDomain )
import {-# SOURCE #-} Language.E.Parser.EssenceFile.Value ( parseValue )
import {-# SOURCE #-} Language.E.Parser.EssenceFile.Declaration ( parseTopLevels )

import Language.E.Imports
import Language.E.Definition
import Language.E.Data ( operators, functionals )
import Language.E.Lexer ( Lexeme(..), lexemeFace )

import Data.String ( fromString )

import Text.Parsec ( (<?>), try )
import Text.Parsec.Combinator ( between, optionMaybe, sepBy1 )



parseMetaVariable :: Parser E
parseMetaVariable = do
    let isMeta LMetaVar {} = True
        isMeta _           = False
    LMetaVar iden <- satisfyT isMeta
    return [xMake| metavar := [Prim (S iden)] |]

parseExpr :: Parser E
parseExpr = shuntingYardExpr parseBeforeShunt
    where
        parseBeforeShunt :: Parser [Either Lexeme E]
        parseBeforeShunt = some $ msum
            [ Right <$> try parseAtomicExpr
            , Left  <$> parseOp
            ]


parseAtomicExpr :: Parser E
parseAtomicExpr = do
    let
        prefixes = do
            fs <- some $ msum parsePrefixes
            return $ foldr1 (.) fs
        postfixes = do
            fs <- some $ msum parsePostfixes
            return $ foldr1 (.) (reverse fs)
        withPrefix  x = try x <|> do f <- prefixes; i <- x; return $ f i
        withPostfix x = do i <- x; mf <- optionMaybe postfixes; return $ case mf of Nothing -> i
                                                                                    Just f  -> f i
    withPrefix (withPostfix parseAtomicExprNoPrePost) <?> "expression"

parseAtomicExprNoPrePost :: Parser E
parseAtomicExprNoPrePost = msum $ map try
    $ parseOthers ++
    [ parseQuantifiedExpr parseExpr
    , parseMetaVariable
    , parseReference
    , parseValue
    , parseDomainAsExpr
    , parseWithLocals
    , parens parseExpr
    ]

parseDomainAsExpr :: Parser E
parseDomainAsExpr = do
    d <- betweenTicks parseDomain
    return [xMake| domainInExpr := [D d]
                 |]

parsePrefixes :: [Parser (E -> E)]
parsePrefixes = [parseUnaryMinus, parseUnaryNot]
    where
        parseUnaryMinus = do
            lexeme L_Minus
            return $ \ x -> [xMake| unaryOp.negate := [x] |]
        parseUnaryNot = do
            lexeme L_ExclamationMark
            return $ \ x -> [xMake| unaryOp.not := [x] |]

parsePostfixes :: [Parser (E -> E)]
parsePostfixes = [parseIndexed,parseFactorial,parseFuncApply,parseReplace]
    where
        parseIndexed :: Parser (E -> E)
        parseIndexed = do
            let
                pIndexer = try pRList <|> parseExpr
                pRList   = do
                    i <- optionMaybe parseExpr
                    dot; dot
                    j <- optionMaybe parseExpr
                    return $ case (i,j) of
                        (Nothing, Nothing) -> [xMake| slicer := [] |]
                        (Just a , Nothing) -> [xMake| slicer.from := [a] |]
                        (Nothing, Just a ) -> [xMake| slicer.to   := [a] |]
                        (Just a , Just b ) -> [xMake| slicer.from := [a]
                                                    | slicer.to   := [b]
                                                    |]
            is <- brackets $ pIndexer `sepBy1` comma
            return $ \ x -> foldl (\ m' i -> [xMake| operator.index.left  := [m']
                                                   | operator.index.right := [i]
                                                   |] ) x is
        parseFactorial :: Parser (E -> E)
        parseFactorial = do
            lexeme L_ExclamationMark
            return $ \ x -> [xMake| unaryOp.factorial := [x] |]
        parseFuncApply :: Parser (E -> E)
        parseFuncApply = parens $ do
            xs <- parseExpr `sepBy1` comma
            return $ \ x -> [xMake| functionApply.actual := [x]
                                  | functionApply.args   := xs
                                  |]
        parseReplace :: Parser (E -> E)
        parseReplace = braces $ do
            let one = do
                    i <- parseExpr
                    lexeme L_LongArrow
                    j <- parseExpr
                    return (i,j)
            pairs <- one `sepBy1` comma
            return $ \ x -> foldl (\ m' (i,j) -> [xMake| operator.replace.arg1 := [m']
                                                       | operator.replace.old  := [i]
                                                       | operator.replace.new  := [j]
                                                       |] ) x pairs

parseOthers :: [Parser E]
parseOthers = [ parseFunctional l
              | l <- functionals
              ] ++ [parseTyped, parseTwoBars]
    where

        parseTwoBars :: Parser E
        parseTwoBars = do
            x <- between (lexeme L_Bar) (lexeme L_Bar) parseExpr
            return [xMake| operator.twoBars := [x] |]

        parseTyped :: Parser E
        parseTyped = parens $ do
            x <- parseExpr
            lexeme L_Colon
            y <- parseDomainAsExpr
            return [xMake| typed.left  := [x]
                         | typed.right := [y]
                         |]

        parseFunctional :: Lexeme -> Parser E
        parseFunctional l = do
            lexeme l
            xs <- parens $ parseExpr `sepBy1` comma
            return $ case (l,xs) of
                (L_image, y:ys) -> [xMake| functionApply.actual := [y]
                                         | functionApply.args   := ys
                                         |]
                _ -> Tagged "operator" [Tagged (fromString $ show $ lexemeFace l) xs]

parseWithLocals :: Parser E
parseWithLocals = braces $ do
    i  <- parseExpr
    lexeme L_At
    js <- parseTopLevels
    return [xMake| withLocals.actual := [i]
                 | withLocals.locals := js
                 |]

parseReference :: Parser E
parseReference = do
    x <- identifierText
    return [xMake| reference := [Prim (S x)]
                 |]


parseOp :: Parser Lexeme
parseOp = msum [ do lexeme x; return x | (x,_,_) <- operators ]
    <?> "operator"

parseQuantifiedExpr :: Parser E -> Parser E
parseQuantifiedExpr parseBody = do
        let pOp = msum [ [xMake| binOp.subset   := [] |] <$ lexeme L_subset
                       , [xMake| binOp.subsetEq := [] |] <$ lexeme L_subsetEq
                       , [xMake| binOp.in       := [] |] <$ lexeme L_in
                       ]
        qnName   <- parseMetaVariable <|> parseReference
        qnVars   <- parseStructural `sepBy1` comma
        qnDom    <- optionMaybe (colon *> parseDomain)
        qnExpr   <- optionMaybe ((,) <$> pOp <*> parseExpr)
        case (qnDom,qnExpr) of
            (Nothing, Nothing) -> fail "expecting something to quantify over"
            _ -> return ()
        qnGuard <- optionMaybe (comma *> parseExpr)
        qnBody  <- dot *> parseBody <?> "expecting body of a quantified expression"

        let emptyGuard = [ [xMake| emptyGuard := [] |] ]

        let
            singleStructurals = [ i | [xMatch| [i] := structural.single |] <- concatMap universe qnVars ]

            idenToSingleStructural i | i `elem` singleStructurals = [xMake| structural.single := [i] |]
            idenToSingleStructural (Tagged t xs) = Tagged t $ map idenToSingleStructural xs
            idenToSingleStructural i = i

        let
            fixedQuanDoms  = map idenToSingleStructural $ case qnDom  of Just a     -> [D a]; _ -> []
            fixedQuanOps   = map idenToSingleStructural $ case qnExpr of Just (a,_) -> [a]; _ -> []
            fixedQuanExprs = map idenToSingleStructural $ case qnExpr of Just (_,a) -> [a]; _ -> []
            fixedGuards    = map idenToSingleStructural $ case qnGuard of Nothing -> emptyGuard ; Just g  -> [g]
            fixedBodys     = map idenToSingleStructural [qnBody]

        let
            f []     = bug "The Impossible has happenned. in parseQuantifiedExpr.f"
            f [i]    = [xMake| quantified.quantifier   := [qnName]
                             | quantified.quanVar      := [i]
                             | quantified.quanOverDom  := fixedQuanDoms
                             | quantified.quanOverOp   := fixedQuanOps
                             | quantified.quanOverExpr := fixedQuanExprs
                             | quantified.guard        := fixedGuards
                             | quantified.body         := fixedBodys
                             |]
            f (i:is) = [xMake| quantified.quantifier   := [qnName]
                             | quantified.quanVar      := [i]
                             | quantified.quanOverDom  := fixedQuanDoms
                             | quantified.quanOverOp   := fixedQuanOps
                             | quantified.quanOverExpr := fixedQuanExprs
                             | quantified.guard        := emptyGuard
                             | quantified.body         := [f is]
                             |]
        return $ f qnVars

parseStructural :: Parser E
parseStructural = msum
    [ parseMetaVariable
    , do
        x <- parseReference
        return [xMake| structural.single := [x] |]
    , do
        void $ optionMaybe $ lexeme L_tuple
        xs <- parens $ parseStructural `sepBy1` comma
        return [xMake| structural.tuple := xs |]
    , do
        xs <- brackets $ parseStructural `sepBy1` comma
        return [xMake| structural.matrix := xs |]
    , do
        xs <- braces $ parseStructural `sepBy1` comma
        return [xMake| structural.set := xs |]
    ]

