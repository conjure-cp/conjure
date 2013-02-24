{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Parser.EssenceFile where

import Language.E.Parser.Imports
import Language.E.Parser.Shunt ( shuntingYardExpr, shuntingYardDomain )

import Stuff.Generic
import Language.E.Imports
import Language.E.Definition
import Language.E.Data ( operators, functionals )
import Language.E.Lexer ( Lexeme(..), lexemeFace )

import Data.String ( fromString )

import Text.Parsec ( (<?>), try )
import Text.Parsec.Combinator ( between, optionMaybe, sepBy, sepBy1, sepEndBy1, eof )

import qualified Data.Text as T


_testParsePrint :: T.Text -> IO ()
_testParsePrint = _testParsePrint' (inCompleteFile parseExpr)

lexAndParseIO :: Parser a -> T.Text -> IO a
lexAndParseIO p t = do
    let res = runLexerAndParser p "" t
    case res of
        Left  e -> error $ show e
        Right x -> return x


parseSpec :: Parser Spec
parseSpec = inCompleteFile $ do
    let
        pLanguage :: Parser Version
        pLanguage = do
            l  <- lexeme L_language *> identifierText
            is <- sepBy1 integer dot
            return (l, map fromInteger is)
    l  <- pLanguage
    xs <- many parseTopLevels
    return $ Spec l $ listAsStatement $ concat xs


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
    return [xMake| domainInExpr := [d]
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
parsePostfixes = [parseIndexed,parseFuncApply,parseReplace]
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
parseWithLocals = parens $ do
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

parseValue :: Parser E
parseValue = msum ( map try
    [ pBool, pInt
    , pMatrix, pMatrix', pTupleWith, pTupleWithout
    , pSet, pMSet
    , pFunction, pRelation, pPartition
    ] ) <?> "value"
    where
        pBool = do
            x <- Prim (B False) <$ lexeme L_false
                 <|>
                 Prim (B True)  <$ lexeme L_true
            return [xMake| value.literal := [x] |]

        pInt = do
            x <- Prim . I <$> integer
            return [xMake| value.literal := [x] |]

        pMatrix = do
            xs <- brackets (sepBy parseExpr comma)
            return [xMake| value.matrix.values := xs |]

        pMatrix' = brackets $ do
            xs <- sepBy parseExpr comma
            lexeme L_SemiColon
            r <- parseDomain
            return [xMake| value.matrix.values     := xs
                         | value.matrix.indexrange := [r]
                         |]
        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ sepBy parseExpr comma
            return [xMake| value.tuple.values := xs |]

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseExpr comma
            return [xMake| value.tuple.values := xs |]

        pSet = do
            xs <- braces (sepBy parseExpr comma)
            return [xMake| value.set.values := xs |]

        pMSet = do
            lexeme L_mset
            xs <- parens (sepBy parseExpr comma)
            return [xMake| value.mset.values := xs |]

        pFunction = do
            lexeme L_function
            xs <- parens (sepBy inner comma)
            return [xMake| value.function.values := xs |]
            where
                inner = do
                    (i,j) <- arrowedPair parseExpr
                    return [xMake| mapping := [i,j] |]

        pRelation = do
            lexeme L_relation
            xs <- parens (sepBy (try pTupleWith <|> pTupleWithout) comma)
            return [xMake| value.relation.values := xs |]

        pPartition = do
            lexeme L_partition
            xs <- parens (sepBy inner comma)
            return [xMake| value.partition := xs|]
            where
                inner = do
                    is <- braces (sepBy parseExpr comma)
                    return [xMake| part := is |]

parseRange :: Parser E
parseRange = msum [try pRange, pSingle]
    where
        pRange = do
            fr <- optionMaybe parseExpr
            dot; dot
            to <- optionMaybe parseExpr
            return $ case (fr,to) of
                (Nothing, Nothing) -> [xMake| range.open   := []    |]
                (Just x , Nothing) -> [xMake| range.from   := [x]   |]
                (Nothing, Just y ) -> [xMake| range.to     := [y]   |]
                (Just x , Just y ) -> [xMake| range.fromTo := [x,y] |]
        pSingle = do
            x <- parseExpr
            return [xMake| range.single := [x] |]

parseDomain :: Parser E
parseDomain
    = shuntingYardDomain
    $ some
    $ msum [ Right <$> try pDomainAtom
           , Left  <$> parseOp'
           ]
    where
        parseOp' = msum [ do lexeme x; return x | x <- [L_Minus, L_union, L_intersect] ] <?> "operator"
        pDomainAtom = msum $ map try
            [ pBool, pInt, pEnum
            , pMatrix, pTupleWithout, pTupleWith
            , pSet, pMSet, pFunction, pFunction'
            , pRelation, pRelation'
            , pPartition
            , parseMetaVariable, pParens
            ]

        pParens = parens parseDomain

        pBool = do
            lexeme L_bool
            return [xMake| domain.bool := [] |]

        pInt = do
            lexeme L_int
            mxs <- optionMaybe $ parens $ parseRange `sepBy` comma
            let xs = fromMaybe [] mxs
            return [xMake| domain.int.ranges := xs |]

        pEnum = do
            r <- parseReference
            xs <- optionMaybe $ parens $ parseRange `sepBy` comma
            case xs of
                Nothing -> return r
                Just ys -> return [xMake| domain.enum.name   := [r]
                                        | domain.enum.ranges := ys
                                        |]

        pMatrix = do
            lexeme L_matrix
            lexeme L_indexed
            lexeme L_by
            xs <- brackets (parseDomain `sepBy1` comma)
            lexeme L_of
            y  <- parseDomain
            return $
                foldr (\ i j -> [xMake| domain.matrix.index := [i]
                                      | domain.matrix.inner := [j]
                                      |]
                      ) y xs

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ parseDomain `sepBy` comma
            return [xMake| domain.tuple.inners := xs |]

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseDomain comma
            return [xMake| domain.tuple.inners := xs |]

        pSet = do
            lexeme L_set
            x <- parseAttributes
            y <- lexeme L_of >> parseDomain
            return [xMake| domain.set.attributes := [x]
                         | domain.set.inner      := [y]
                         |]

        pMSet = do
            lexeme L_mset
            x <- parseAttributes
            y <- lexeme L_of >> parseDomain
            return [xMake| domain.mset.attributes := [x]
                         | domain.mset.inner      := [y]
                         |]

        pFunction = do
            lexeme L_function
            (y,z) <- arrowedPair parseDomain
            return [xMake| domain.function.attributes.attrCollection := []
                         | domain.function.innerFrom  := [y]
                         | domain.function.innerTo    := [z]
                         |]

        pFunction' = do
            lexeme L_function
            x <- parseAttributes
            y <- parseDomain
            lexeme L_LongArrow
            z <- parseDomain
            return [xMake| domain.function.attributes := [x]
                         | domain.function.innerFrom  := [y]
                         | domain.function.innerTo    := [z]
                         |]

        pRelation' = do
            lexeme L_relation
            return [xMake| type.relation.inners.type.unknown := [] |]

        pRelation = do
            lexeme L_relation
            x  <- parseAttributes
            lexeme L_of
            ys <- parens (parseDomain `sepBy` lexeme L_Times)
            return [xMake| domain.relation.attributes := [x]
                         | domain.relation.inners     := ys
                         |]

        pPartition = do
            lexeme L_partition
            x <- parseAttributes
            lexeme L_from
            y <- parseDomain
            return [xMake| domain.partition.attributes := [x]
                         | domain.partition.inner      := [y]
                         |]

parseAttributes :: Parser E
parseAttributes = do
    xs <- parens (parseAttribute `sepBy` comma) <|> return []
    return [xMake| attrCollection := map snd xs
                 |]
    -- return [xMake| attrCollection := map snd $ sortBy (comparing fst) xs
    --              |]
    where
        parseAttribute = msum [try parseNameValue, try parseName, parseDontCare]
        parseNameValue = do
            n <- parseReference
            v <- parseExpr
            return
                ( Just n
                , [xMake| attribute.nameValue.name  := [n]
                        | attribute.nameValue.value := [v]
                        |]
                )
        parseName = do
            n <- parseReference
            return
                ( Just n
                , [xMake| attribute.name := [n]
                        |]
                )
        parseDontCare = do
            dot; dot
            return
                ( Nothing
                , [xMake| attribute.dontCare := []
                        |]
                )

parseTopLevels :: Parser [E]
parseTopLevels = do
    let one = msum $ map try
                [ do
                    lexeme L_find
                    decls <- flip sepBy1 comma $ do
                        is <- parseReference `sepBy1` comma
                        j  <- colon >> parseDomain
                        return [ [xMake| topLevel.declaration.find.name   := [i]
                                       | topLevel.declaration.find.domain := [j]
                                       |]
                               | i <- is ]
                    return $ concat decls
                    <?> "find statement"
                , do
                    lexeme L_given
                    decls <- flip sepBy1 comma $ do
                        is <- parseReference `sepBy1` comma
                        msum
                            [ do
                                colon
                                j <- parseDomain
                                return [ [xMake| topLevel.declaration.given.name   := [i]
                                               | topLevel.declaration.given.domain := [j]
                                               |]
                                       | i <- is ]
                            , do
                                lexeme L_new
                                msum
                                    [ do
                                        lexeme L_type
                                        lexeme L_enum
                                        return [ [xMake| topLevel.declaration.given.name     := [i]
                                                       | topLevel.declaration.given.typeEnum := []
                                                       |]
                                               | i <- is ]
                                    , do
                                        lexeme L_domain
                                        lexeme L_int
                                        return [ [xMake| topLevel.declaration.given.name    := [i]
                                                       | topLevel.declaration.given.typeInt := []
                                                       |]
                                               | i <- is ]
                                    ]
                            ]
                    return $ concat decls
                    <?> "given statement"
                , do
                    lexeme L_letting
                    decls <- flip sepBy1 comma $ do
                        is <- (try parseMetaVariable <|> parseReference) `sepBy1` comma
                        lexeme L_be
                        msum
                            [ do
                                lexeme L_new
                                lexeme L_type
                                msum
                                    [ do
                                        lexeme L_of
                                        lexeme $ LIdentifier "size"
                                        j <- parseExpr
                                        return [ [xMake| topLevel.letting.name := [i]
                                                       | topLevel.letting.typeUnnamed := [j]
                                                       |]
                                               | i <- is
                                               ]
                                    , do
                                        lexeme L_enum
                                        ys <- braces (parseReference `sepBy` comma) <|> return []
                                        return [ [xMake| topLevel.letting.name := [i]
                                                       | topLevel.letting.typeEnum.values := ys
                                                       |]
                                               | i <- is
                                               ]
                                    ]
                            , do
                                lexeme L_domain
                                j <- parseDomain
                                return [ [xMake| topLevel.letting.name   := [i]
                                               | topLevel.letting.domain := [j]
                                               |]
                                       | i <- is
                                       ]
                            , do
                                j <- parseExpr
                                return [ [xMake| topLevel.letting.name := [i]
                                               | topLevel.letting.expr := [j]
                                               |]
                                       | i <- is
                                       ]
                            , do
                                j <- parseLambda L_lambda
                                return [ [xMake| topLevel.letting.name   := [i]
                                               | topLevel.letting.lambda := [j]
                                               |]
                                       | i <- is
                                       ]
                            , do
                                j <- parseQuanDecl
                                return [ [xMake| topLevel.letting.name       := [i]
                                               | topLevel.letting.quantifier := [j]
                                               |]
                                       | i <- is
                                       ]
                            ]
                    return $ concat decls
                    <?> "letting statement"
                , do
                    lexeme L_dim
                    is <- parseReference `sepBy1` comma
                    j  <- colon >> parseDomain
                    return [ [xMake| topLevel.declaration.dim.name   := [i]
                                   | topLevel.declaration.dim.domain := [j]
                                   |]
                           | i <- is
                           ]
                , do
                    let dimfind = do
                            lexeme L_find
                            i <- parseExpr
                            colon
                            j <- parseDomain
                            return [xMake| dimFind.name   := [i]
                                         | dimFind.domain := [j]
                                         |]
                    let nested = try dimfind <|> try (parseQuantifiedExpr nested) <|> parens nested
                    i <- nested
                    return [ [xMake| topLevel.declaration.nestedDimFind := [i]
                                   |]
                           ]
                , do
                    lexeme L_where
                    xs <- parseExpr `sepEndBy1` comma
                    return [ [xMake| topLevel.where := [x] |]
                           | x <- xs ]
                , do
                    lexeme L_such
                    lexeme L_that
                    xs <- parseExpr `sepEndBy1` comma
                    return [ [xMake| topLevel.suchThat := [x] |]
                           | x <- xs ]
                , do
                    lexeme L_minimising
                    x <- parseExpr
                    return [ [xMake| topLevel.objective.minimising := [x]
                                   |]
                           ]
                , do
                    lexeme L_maximising
                    x <- parseExpr
                    return [ [xMake| topLevel.objective.maximising := [x]
                                   |]
                           ]
                ]
    concat <$> some one

parseLambda :: Lexeme -> Parser E
parseLambda l = do
    lexeme l
    braces $ do
        param <- parseExpr
        lexeme L_LongArrow
        body  <- parseExpr
        return [xMake| lambda.param := [param]
                     | lambda.body  := [body]
                     |]

parseQuanDecl :: Parser E
parseQuanDecl = do
    lexeme L_quantifier
    braces $ do
        append   <- parseLambda $ LIdentifier "append"
        guard    <- parseLambda $ LIdentifier "guard"
        identity <- lexeme (LIdentifier "identity") *> parseExpr
        return [xMake| quantifierDecl.append   := [append]
                     | quantifierDecl.guard    := [guard]
                     | quantifierDecl.identity := [identity]
                     |]

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
            fixedQuanDoms  = map idenToSingleStructural $ case qnDom  of Just a     -> [a]; _ -> []
            fixedQuanOps   = map idenToSingleStructural $ case qnExpr of Just (a,_) -> [a]; _ -> []
            fixedQuanExprs = map idenToSingleStructural $ case qnExpr of Just (_,a) -> [a]; _ -> []
            fixedGuards    = map idenToSingleStructural $ case qnGuard of Nothing -> emptyGuard ; Just g  -> [g]
            fixedBodys     = map idenToSingleStructural [qnBody]

        let
            f []     = error "The Impossible has happenned. in parseQuantifiedExpr.f"
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
        xs <- parens $ parseStructural `sepBy1` comma
        return [xMake| structural.tuple := xs |]
    , do
        xs <- brackets $ parseStructural `sepBy1` comma
        return [xMake| structural.matrix := xs |]
    ]

parseRuleRefn :: T.Text -> Parser [RuleRefn]
parseRuleRefn t = inCompleteFile $ do
    level <- optionMaybe (brackets (fromInteger <$> integer))
    let
        one = do
            pattern   <- parseExpr
            templates <- some (lexeme L_SquigglyArrow >> parseExpr)
            locals    <- concat <$> many parseTopLevels
            return ( t
                   , level
                   , [xMake| rulerefn.pattern   := [pattern]
                           | rulerefn.templates := templates
                           | rulerefn.locals    := locals
                           |]
                   )
    some one

parseRuleReprCase :: Parser RuleReprCase
parseRuleReprCase = do
    lexeme L_CaseSeparator
    dom    <- parseDomain
    mcons  <- optionMaybe (lexeme L_SquigglyArrow >> parseExpr)
    locals <- concat <$> many parseTopLevels
    return (dom, mcons, locals)


parseRuleRepr :: T.Text -> Parser RuleRepr
parseRuleRepr t = inCompleteFile $ do
    let arr i = lexeme L_SquigglyArrow >> i
    nmRepr <- arr identifierText
    domOut <- arr parseDomain
    mcons  <- optionMaybe $ arr parseExpr
    locals <- concat <$> many parseTopLevels
    cases  <- some parseRuleReprCase
    return ( t
           , nmRepr
           , domOut
           , mcons
           , locals
           , cases
           )


inCompleteFile :: Parser a -> Parser a
inCompleteFile parser = do
    result <- parser
    eof
    return result


arrowedPair :: Parser a -> Parser (a,a)
arrowedPair p = do
    i <- p
    lexeme L_LongArrow
    j <- p
    return (i,j)

