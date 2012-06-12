{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Parser where

import Control.Applicative

import Language.EssenceLexer
import Language.EssenceLexerP

import Language.Core.Imports hiding ( (<?>), (<??>) )
import Language.Core.Definition
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty


runP :: (Functor m, Monad m, ShowAST a) => Maybe FilePath -> Parser a -> Text -> CompT m a
runP mfp pa te = do
    case lexAndParse mfp pa te of
        Left  e  -> err ErrParsing e
        Right xs -> case xs of
            []  -> err ErrParsing "No parse."
            [x] -> return x
            _   -> err ErrParsing $ Nested (Just "Ambiguous parse.") (map (singletonNested . showAST) xs)

test_ParsePrint' :: (Show a, ShowAST a, Pretty a) => Parser a -> Text -> IO (Maybe a)
test_ParsePrint' p t = do
    xs <- lexAndParseIO (p <* eof) t
    mapM_ (print . showAST) xs
    mapM_ (print . pretty ) xs
    case xs of
        []  -> do putStrLn "No parse."; return Nothing
        [a] -> return $ Just a
        _   -> do putStrLn "Ambiguous parse."; return Nothing

test_ParsePrint :: Text -> IO (Maybe Core)
test_ParsePrint = test_ParsePrint' parseExpr

parseSpec :: Parser Spec
parseSpec = do
    let
        pLanguage :: Parser (String,[Int])
        pLanguage = do
            l  <- lexeme L_language *> identifier
            is <- sepBy1 integer dot
            return (l, map fromInteger is)
    whiteSpace
    l  <- pLanguage
    xs <- many parseTopLevels
    eof
    return $ Spec l $ concat xs


parseMetaVariable :: Parser Core
parseMetaVariable = do
    LMetaVar iden <- satisfy $ \ i -> case i of LMetaVar {} -> True; _ -> False
    return $ Expr ":metavar" [R $ Reference iden]

parseExpr :: Parser Core
parseExpr = do
    xs <- fixNegate <$> parseBeforeShunt
    if not $ checkAlternating xs
        then do
            -- fail $ ppShow xs
            failWithMsg "Shunting Yard failed. How dare you."
        else shunt xs
    where
        fixNegate :: [Either Lexeme Core] -> [Either Lexeme Core]
        fixNegate (Right a:Right (Expr ":operator-negate" [b]):cs) = fixNegate $ Right a : Left L_Minus : Right b : cs
        fixNegate (a:bs) = a : fixNegate bs
        fixNegate [] = []

        checkAlternating :: [Either a b] -> Bool
        checkAlternating [Right _] = True
        checkAlternating (Right _:Left _:rest) = checkAlternating rest
        checkAlternating _ = False

        shunt :: [Either Lexeme Core] -> Parser Core
        shunt xs = do
            result <- findPivotOp xs
            case result of
                Left x -> return x
                Right (before, op, after) -> do
                    b <- shunt before
                    a <- shunt after
                    return $ Expr (Tag $ ":operator-" `mappend` lexemeText op) [b,a]

        findPivotOp :: [Either Lexeme Core] -> Parser (Either Core ([Either Lexeme Core], Lexeme, [Either Lexeme Core]))
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

                findFirst :: [Either Lexeme Core] -> Parser ([Either Lexeme Core], Lexeme, [Either Lexeme Core])
                findFirst [] = failWithMsg "findPivotOp.findFirst"
                findFirst (Left i:is) | chck i = return ([], i, is)
                findFirst (i:is) = do
                    (before, op, after) <- findFirst is
                    return (i:before, op, after)
        
                findLast :: [Either Lexeme Core] -> Parser ([Either Lexeme Core], Lexeme, [Either Lexeme Core])
                findLast is = do
                    (before, op, after) <- findFirst (reverse is)
                    return (reverse after, op, reverse before)

                findOnly :: [Either Lexeme Core] -> Parser ([Either Lexeme Core], Lexeme, [Either Lexeme Core])
                findOnly is = do
                    f <- findFirst is
                    l <- findLast  is
                    if f == l
                        then return f
                        else failWithMsg "Ambiguous use of non-associative operator."

            let
                finder = case pivotFixity of
                            FLeft  -> findLast
                            FNone  -> findOnly
                            FRight -> findFirst
            Right <$> finder xs


parseAtomicExpr :: Parser Core
parseAtomicExpr = do
    let
        prefixes = do
            fs <- some $ msum1 parsePrefixes
            return $ foldr1 (.) fs
        postfixes = do
            fs <- some $ msum1 parsePostfixes
            return $ foldr1 (.) (reverse fs)
        withPrefix  x = x <||> do f <- prefixes; i <- x; return $ f i
        withPostfix x = do i <- x; mf <- optionMaybe postfixes; return $ case mf of Nothing -> i
                                                                                    Just f  -> f i
    withPrefix $ withPostfix parseAtomicExpr_NoPrePost

parseAtomicExpr_NoPrePost :: Parser Core
parseAtomicExpr_NoPrePost = msum1
    $ parseOthers ++ 
    [ parseQuantifiedExpr parseExpr
    , parseMetaVariable
    , parseReference
    , parseValue
    , betweenTicks parseDomain
    , parseWithLocals
    , parens parseExpr
    ]

parsePrefixes :: [Parser (Core -> Core)]
parsePrefixes = [parseUnaryMinus, parseUnaryNot]
    where
        parseUnaryMinus = do
            lexeme L_Minus
            return $ \ x -> Expr ":operator-negate" [x]
        parseUnaryNot = do
            lexeme L_ExclamationMark
            return $ \ x -> Expr ":operator-not" [x]

parsePostfixes :: [Parser (Core -> Core)]
parsePostfixes = [parseIndexed,parseFuncApply,parseReplace]
    where
        parseIndexed :: Parser (Core -> Core)
        parseIndexed = do
            let
                pIndexer = pRList <||> parseExpr
                pRList   = do
                    i <- optionMaybe parseExpr
                    dot; dot
                    j <- optionMaybe parseExpr
                    return $ Expr ":slicer" $ [ Expr ":slicer-from" [a] | Just a <- [i] ]
                                           ++ [ Expr ":slicer-to"   [a] | Just a <- [j] ]
            is <- brackets $ pIndexer `sepBy1` comma
            return $ \ x -> foldl (\ m' i -> Expr ":operator-index" [m', i] ) x is
        parseFuncApply :: Parser (Core -> Core)
        parseFuncApply = parens $ do
            xs <- parseExpr `sepBy1` comma
            return $ \ x -> Expr ":function-apply" [ Expr ":function-apply-actual" [x]
                                                   , Expr ":function-apply-args"   xs
                                                   ]
        parseReplace :: Parser (Core -> Core)
        parseReplace = braces $ do
            let one = do
                    i <- parseExpr
                    lexeme L_LongArrow
                    j <- parseExpr
                    return (i,j)
            pairs <- one `sepBy1` comma
            return $ \ x -> foldl (\ m' (i,j) -> Expr ":operator-replace" [m',i,j] ) x pairs

parseOthers :: [Parser Core]
parseOthers = [ parseFunctional l
              | l <- functionals
              ] ++ [parseTyped, parseTwoBars]
    where

        parseTwoBars :: Parser Core
        parseTwoBars = do
            x <- between (lexeme L_Bar) (lexeme L_Bar) parseExpr
            return $ Expr ":operator-twobars" [x]

        parseTyped :: Parser Core
        parseTyped = parens $ do
            x <- parseExpr
            lexeme L_Colon
            y <- betweenTicks parseDomain
            return $ Expr ":typed" [x,y]

        parseFunctional :: Lexeme -> Parser Core
        parseFunctional l = do
            lexeme l
            xs <- parens $ parseExpr `sepBy1` comma
            return $ Expr (Tag $ ":operator-" `mappend` lexemeText l) xs


        -- helper op@Negate       = genPrefix   op
        -- helper op@Factorial    = genPostfix  op
        -- helper op@Not          = genPrefix   op
        -- helper op@Max          = genLispy    op   [1,2]
        -- helper op@Min          = genLispy    op   [1,2]
        -- helper op@ToSet        = genLispy    op   [1]
        -- helper op@ToMSet       = genLispy    op   [1]
        -- helper op@ToRelation   = genLispy    op   [1]
        -- helper op@Defined      = genLispy    op   [1]
        -- helper op@Range        = genLispy    op   [1]
        -- helper op@Image        = genLispy    op   [2]
        -- helper op@PreImage     = genLispy    op   [2]
        -- helper op@Inverse      = genLispy    op   [2]
        -- helper op@Together     = genLispy    op   [3]
        -- helper op@Apart        = genLispy    op   [3]
        -- helper op@Party        = genLispy    op   [2]
        -- helper op@Participants = genLispy    op   [1]
        -- helper op@Parts        = genLispy    op   [1]
        -- helper op@Freq         = genLispy    op   [2]
        -- helper op@Hist         = genLispy    op   [2]
        -- helper op@HasType      = genInfix    op    1000 InfixN
        -- helper op@HasDomain    = genInfix    op    1000 InfixN
        -- helper op@AllDiff      = genLispy    op   [1]
        -- helper op@ToInt        = genLispy    op   [1]
        -- helper op@Flatten      = genLispy    op   [1,2]
        -- helper op@NormIndices  = genLispy    op   [1]
        -- helper TwoBars         = OpSpecial (pa <??> "|expression|") pr
        --     where
        --         pa = between (lexeme L_Bar) (lexeme L_Bar) $ do i <- parse; return $ EOp TwoBars [i]
        --         pr (EOp TwoBars [x]) = "|" <> pretty x <> "|"
        --         pr x = error $ "pretty TwoBars: " ++ show x
        -- 
        -- helper Index           = OpPostfix (pa <?> "indexed expression") pr
        --     where
        --         pa = do
        --             let pIndexer = pRList <||> parse
        --                 pRList   = do
        --                     i <- optionMaybe parse
        --                     dot; dot
        --                     j <- optionMaybe parse
        --                     return $ D $ DInt $ RFromTo [Right (i,j)]
        --             is <- brackets $ pIndexer `sepBy1` comma
        --             return (\ x -> foldl (\ m' i -> EOp Index [m', i]) x is)
        --         pr (EOp Index [m,i]) =
        --             let
        --                 f (EOp Index [x,y]) = second (prettyIndexProject y:) (f x)
        --                 f x = (x,[])
        --                 (a,bs) = f m
        --             in
        --                 pretty a <> prettyListDoc Pr.brackets Pr.comma (reverse (prettyIndexProject i:bs))
        --         pr x = error $ "pretty Index: " ++ show x
        --         prettyIndexProject (D (DInt i)) = pretty i
        --         prettyIndexProject i = pretty i
        -- helper Replace = OpPostfix pa pr









parseWithLocals :: Parser Core
parseWithLocals = parens $ do
    i  <- parseExpr
    lexeme L_At
    js <- parseTopLevels
    return $ Expr ":withlocals" [ Expr ":actual" [i]
                                , Expr ":locals" js
                                ]

parseReference :: Parser Core
parseReference = do
    x <- identifierText
    return $ R $ Reference x

parseBeforeShunt :: Parser [Either Lexeme Core]
parseBeforeShunt = many $ msum1
    [ Right <$> parseAtomicExpr
    , Left  <$> parseOp
    ]

parseOp :: Parser Lexeme
parseOp = msum1 [ do lexeme x; return x | (x,_,_) <- operators ]

parseValue :: Parser Core
parseValue = do
    x <- msum1
            [ pBool, pInt
            , pMatrix, pMatrix', pTuple
            , pSet, pMSet
            , pFunction, pRelation, pPartition
            ] <?> "expecting value"
    return $ Expr ":value" [x]
    where
        pBool = do
            x <- L (B False) <$ lexeme L_false
                 <|>
                 L (B True)  <$ lexeme L_true
            return $
                Expr ":value-literal" [x]

        pInt = do
            x <- L . I <$> integer
            return $
                Expr ":value-literal" [x]

        pMatrix = do
            xs <- brackets (sepBy parseExpr comma)
            return $
                Expr ":value-matrix"
                    [ Expr ":value-matrix-values" xs
                    ]

        pMatrix' = brackets $ do
            xs <- sepBy parseExpr comma
            lexeme L_SemiColon
            r <- parseRange
            return $
                Expr ":value-matrix"
                    [ Expr ":value-matrix-values"  xs
                    , Expr ":value-matrix-indexrange" [r]
                    ]

        pTuple = pTupleWith <|> pTupleWithout

        pTupleWith = do
            lexeme L_tuple
            xs <- parens $ sepBy parseExpr comma
            return $ Expr ":value-tuple" xs

        pTupleWithout = do
            xs <- parens $ countSepAtLeast 2 parseExpr comma
            return $ Expr ":value-tuple" xs            

        pSet = do
            xs <- braces (sepBy parseExpr comma)
            return $ Expr ":value-set" xs

        pMSet = do
            lexeme L_mset
            xs <- parens (sepBy parseExpr comma)
            return $Expr ":value-mset" xs

        pFunction = do
            lexeme L_function
            xs <- parens (sepBy inner comma)
            return $ Expr ":value-function" xs
            where
                inner = do
                    i <- parseExpr
                    lexeme L_LongArrow
                    j <- parseExpr
                    return $ Expr ":value-function-mapping" [i,j]

        pRelation = do
            lexeme L_relation
            xs <- parens (sepBy pTuple comma)
            return $ Expr ":value-relation" $ map (\ x -> Expr ":value" [x]) xs

        pPartition = do
            lexeme L_partition
            xs <- parens (sepBy inner comma)
            return $ Expr ":value-partition" xs
            where
                inner = Expr ":value-partition-part" <$> braces (sepBy parseExpr comma)


parseRange :: Parser Core
parseRange = do
    r <- msum1 [pRange, pSingle]
    return $ Expr ":range" [r]
    where
        pRange = do
            fr <- optionMaybe parseExpr
            dot; dot
            to <- optionMaybe parseExpr
            return $ case (fr,to) of
                (Nothing, Nothing) -> Expr ":range-open"   []
                (Just x , Nothing) -> Expr ":range-from"   [x]
                (Nothing, Just y ) -> Expr ":range-to"     [y]
                (Just x , Just y ) -> Expr ":range-fromto" [x,y]
        pSingle = do
            x <- parseExpr
            return $ Expr ":range-single" [x]


parseDomain :: Parser Core
parseDomain = msum1 [pDomain, parseMetaVariable]
    where
        pDomain = do
            x <- msum1
                [ pParens, pBool, pInt, pEnum
                , pMatrix, pTuple
                , pSet, pMSet, pFunction, pRelation, pPartition
                ]
            return $ case x of
                Expr ":domain" _ -> x
                _                -> Expr ":domain" [x]

        pParens = parens parseDomain

        pBool = do
            lexeme L_bool
            return $ Expr ":domain-bool" []

        pInt = do
            lexeme L_int
            mxs <- optionMaybe $ parens $ parseRange `sepBy` comma
            let xs = case mxs of Nothing -> []; Just i -> i
            return $ Expr ":domain-int" [ Expr ":domain-int-ranges" xs ]

        pEnum = do
            r <- parseReference
            xs <- optionMaybe $ parens $ parseRange `sepBy` comma
            case xs of
                Nothing -> return r
                Just ys -> return $ Expr ":domain-enum" [ Expr ":domain-enum-name" [r]
                                                        , Expr ":domain-enum-range" ys
                                                        ]

        pMatrix = do
            lexeme L_matrix
            lexeme L_indexed
            lexeme L_by
            xs <- brackets (parseDomain `sepBy1` comma)
            lexeme L_of
            y  <- parseDomain
            return $
                foldr (\ i j ->
                    Expr ":domain"
                        [ Expr ":domain-matrix"
                            [ Expr ":domain-matrix-index" [i]
                            , Expr ":domain-matrix-inner" [j]
                            ]
                        ]
                      ) y xs

        pTuple = do
            void $ optionMaybe $ lexeme L_tuple
            xs <- parens (parseDomain `sepBy` comma)
            return $
                Expr ":domain-tuple"
                    [ Expr ":domain-tuple-inners" xs
                    ]
        
        pSet = do
            lexeme L_set
            xs <- parseAttributes
            lexeme L_of
            y  <- parseDomain
            return $
                Expr ":domain-set"
                    [ Expr ":domain-set-attributes" [ Expr ":attributes" xs ]
                    , Expr ":domain-set-inner"     [y]
                    ]

        pMSet = do
            lexeme L_mset
            xs <- parseAttributes
            lexeme L_of
            y  <- parseDomain
            return $
                Expr ":domain-mset"
                    [ Expr ":domain-mset-attributes" [ Expr ":attributes" xs ]
                    , Expr ":domain-mset-inner"     [y]
                    ]

        pFunction = do
            lexeme L_function
            xs <- parseAttributes
            y  <- parseDomain
            lexeme L_LongArrow
            z  <- parseDomain
            return $
                Expr ":domain-function"
                    [ Expr ":domain-function-attributes" [ Expr ":attributes" xs ]
                    , Expr ":domain-function-innerfrom" [y]
                    , Expr ":domain-function-innerto"   [z]
                    ]

        pRelation = do
            lexeme L_relation
            xs <- parseAttributes
            lexeme L_of
            ys <- parens (parseDomain `sepBy` lexeme L_Times)
            return $
                Expr ":domain-relation"
                    [ Expr ":domain-relation-attributes" [ Expr ":attributes" xs ]
                    , Expr ":domain-relation-inners"    ys
                    ]

        pPartition = do
            lexeme L_partition
            xs <- parseAttributes
            lexeme L_from
            y  <- parseDomain
            return $
                Expr ":domain-partition"
                    [ Expr ":domain-partition-attributes" [ Expr ":attributes" xs ]
                    , Expr ":domain-partition-inner"     [y]
                    ]


parseAttributes :: Parser [Core]
parseAttributes = do
    xs <- parens (parseAttribute `sepBy` comma) <|> return []
    return $ map snd $ sortBy (comparing fst) xs
    where
        parseAttribute = do
            (n,x) <- msum1 [parseNameValue, parseName, parseDontCare]
            return $ (n, Expr ":attribute" [x])
        parseNameValue = do
            n <- parseReference
            v <- parseExpr
            return
                ( Just n
                , Expr ":attribute-namevalue"
                    [ Expr ":attribute-namevalue-name"  [n]
                    , Expr ":attribute-namevalue-value" [v]
                    ]
                )
        parseName = do
            n <- parseReference
            return
                ( Just n
                , Expr ":attribute-name"
                    [ Expr ":attribute-name-name"  [n]
                    ]
                )
        parseDontCare = do
            dot; dot
            return
                ( Nothing
                , Expr ":attribute-dontcare" []
                )


parseTopLevels :: Parser [Core]
parseTopLevels = do
    let one = msum1
                [ do
                    lexeme L_find
                    decls <- flip sepBy1 comma $ do
                        is <- parseReference `sepBy1` comma
                        colon
                        j  <- parseDomain
                        return
                            [ Expr ":declaration"
                                [ Expr ":find"
                                    [ Expr ":find-name" [i]
                                    , Expr ":find-domain" [j]
                                    ]
                                ]
                            | i <- is ]
                    return $ concat decls
                    <?> "find statement"
                , do
                    lexeme L_given
                    decls <- flip sepBy1 comma $ do
                        is <- parseReference `sepBy1` comma
                        msum1
                            [ do
                                colon
                                j <- parseDomain
                                return
                                    [ Expr ":declaration"
                                        [ Expr ":given"
                                            [ Expr ":given-name" [i]
                                            , Expr ":given-domain" [j]
                                            ]
                                        ]
                                    | i <- is ]
                            , do
                                lexeme L_new
                                lexeme L_type
                                lexeme L_enum
                                return
                                    [ Expr ":declaration"
                                        [ Expr ":given"
                                            [ Expr ":given-name" [i]
                                            , Expr ":given-typeenum" []
                                            ]
                                        ]
                                    | i <- is ]
                            ]
                    return $ concat decls
                    <?> "given statement"
                , do
                    lexeme L_letting
                    decls <- flip sepBy1 comma $ do
                        is <- parseReference `sepBy1` comma
                        lexeme L_be
                        msum1
                            [ do
                                lexeme L_new
                                lexeme L_type
                                msum1
                                    [ do
                                        lexeme L_of
                                        lexeme $ LIdentifier "size"
                                        j <- parseExpr
                                        return
                                            [ Expr ":letting"
                                                [ Expr ":letting-name" [i]
                                                , Expr ":letting-type-unnamed" [j]
                                                ]
                                            | i <- is
                                            ]
                                    , do
                                        lexeme L_enum
                                        ys <- braces (parseReference `sepBy` comma) <|> return []
                                        return
                                            [ Expr ":letting"
                                                [ Expr ":letting-name" [i]
                                                , Expr ":letting-type-enum"
                                                    [ Expr ":type-enum-values" ys
                                                    ]
                                                ]
                                            | i <- is
                                            ]
                                    ]
                            , do
                                lexeme L_domain
                                j <- parseDomain
                                return
                                    [ Expr ":letting"
                                        [ Expr ":letting-name"   [i]
                                        , Expr ":letting-domain" [j]
                                        ]
                                    | i <- is
                                    ]
                            , do
                                j <- parseExpr
                                return
                                    [ Expr ":letting"
                                        [ Expr ":letting-name" [i]
                                        , Expr ":letting-expr" [j]
                                        ]
                                    | i <- is
                                    ]
                            , do
                                j <- parseLambda L_lambda
                                return
                                    [ Expr ":letting"
                                        [ Expr ":letting-name"   [i]
                                        , Expr ":letting-lambda" [j]
                                        ]
                                    | i <- is
                                    ]
                            , do
                                j <- parseQuanDecl
                                return
                                    [ Expr ":letting"
                                        [ Expr ":letting-name"       [i]
                                        , Expr ":letting-quantifier" [j]
                                        ]
                                    | i <- is
                                    ]
                            ]
                    return $ concat decls
                    <?> "letting statement"
                , do
                    lexeme L_dim
                    is <- parseReference `sepBy1` comma
                    colon
                    j  <- parseDomain
                    return
                        [ Expr ":declaration"
                            [ Expr ":dim"
                                [ Expr ":dim-name" [i]
                                , Expr ":dim-domain" [j]
                                ]
                            ]
                        | i <- is ]
                , do
                    let dimfind = do
                            lexeme L_find
                            i <- parseExpr
                            colon
                            j <- parseDomain
                            return $ Expr ":dimfind"
                                [ Expr ":dimfind-name"   [i]
                                , Expr ":dimfind-domain" [j]
                                ]
                    let nested = dimfind <|> parseQuantifiedExpr nested <|> parens nested
                    i <- nested
                    return
                        [ Expr ":declaration"
                            [ Expr ":nested-dimfind"
                                [i]
                            ]
                        ]
                , do
                    lexeme L_where
                    xs <- parseExpr `sepBy1` comma
                    return [ Expr ":where" [x] | x <- xs ]
                , do
                    lexeme L_such
                    lexeme L_that
                    xs <- parseExpr `sepBy1` comma
                    return [ Expr ":suchthat" [x] | x <- xs ]
                , do
                    lexeme L_minimising
                    x <- parseExpr
                    return [ Expr ":objective" [ Expr ":minimising" [x] ] ]
                , do
                    lexeme L_maximising
                    x <- parseExpr
                    return [ Expr ":objective" [ Expr ":maximising" [x] ] ]
                ]
    map (\ x -> Expr ":toplevel" [x]) . concat <$> some one

parseLambda :: Lexeme -> Parser Core
parseLambda l = do
    lexeme l
    braces $ do
        param <- parseExpr
        lexeme L_LongArrow
        body  <- parseExpr
        return $
            Expr ":lambda"
                [ Expr ":lambda-param" [param]
                , Expr ":lambda-body"  [body]
                ]

parseQuanDecl :: Parser Core
parseQuanDecl = do
    lexeme L_quantifier
    braces $ do
        append   <- parseLambda $ LIdentifier "append"
        guard    <- parseLambda $ LIdentifier "guard"
        identity <- lexeme (LIdentifier "identity") *> parseExpr
        return $
            Expr ":quantifier"
                [ Expr ":quantifier-append"   [append]
                , Expr ":quantifier-guard"    [guard]
                , Expr ":quantifier-identity" [identity]
                ]



parseQuantifiedExpr :: Parser Core -> Parser Core
parseQuantifiedExpr parseBody = do
        let pOp = msum1 [ Tag (":operator-" `mappend` lexemeText L_subset  ) <$ lexeme L_subset
                        , Tag (":operator-" `mappend` lexemeText L_subsetEq) <$ lexeme L_subsetEq
                        , Tag (":operator-" `mappend` lexemeText L_in      ) <$ lexeme L_in
                        ]
        -- let pOp = msum1 [ Tag (lexemeText L_subset  ) <$ lexeme L_subset
        --                 , Tag (lexemeText L_subsetEq) <$ lexeme L_subsetEq
        --                 , Tag (lexemeText L_in      ) <$ lexeme L_in
        --                 ]
        qnName   <- parseMetaVariable <|> parseReference
        qnVars   <- parseStructural `sepBy1` comma
        qnDom    <- optionMaybe (colon *> parseDomain)
        qnExpr   <- optionMaybe ((,) <$> pOp <*> parseExpr)
        case (qnDom,qnExpr) of
            (Nothing, Nothing) -> failWithMsg "expecting something to quantify over"
            _ -> return ()
        qnGuard <- optionMaybe (comma *> parseExpr)
        qnBody  <- dot *> ( parseBody <??> "expecting body of a quantified expression" )
        let emptyGuard      = [ Expr ":expr-quantified-guard" [ Expr ":empty-guard" [] ] ]
        let emptyGuardOr Nothing  = emptyGuard
            emptyGuardOr (Just a) = [ Expr ":expr-quantified-guard" [a] ]

        let
            f []     = error "The Impossible has happenned. in parseQuantifiedExpr.f"
            f [i]    = Expr ":expr-quantified" $ concat
                        [ [ Expr ":expr-quantified-quantifier"  [qnName] ]
                        , [ Expr ":expr-quantified-quanVar"     [i] ]
                        , [ Expr ":expr-quantified-quanOverDom" [a]
                          | Just a <- [qnDom]
                          ]
                        , concat [ [ Expr ":expr-quantified-quanOverOp"   [Expr a []]
                                   , Expr ":expr-quantified-quanOverExpr" [b]
                                   ]
                                 | Just (a,b) <- [qnExpr]
                                 ]
                        , emptyGuardOr qnGuard
                        , [ Expr ":expr-quantified-body"         [qnBody]  ]
                        ]
                        -- QuantifiedExpr qnName i qnDom qnExpr qnGuard qnBody
            f (i:is) = Expr ":expr-quantified" $ concat
                        [ [ Expr ":expr-quantified-quantifier"  [qnName] ]
                        , [ Expr ":expr-quantified-quanVar"     [i] ]
                        , [ Expr ":expr-quantified-quanOverDom" [a]
                          | Just a <- [qnDom]
                          ]
                        , concat [ [ Expr ":expr-quantified-quanOverOp"   [Expr a []]
                                   , Expr ":expr-quantified-quanOverExpr" [b]
                                   ]
                                 | Just (a,b) <- [qnExpr]
                                 ]
                        , emptyGuard
                        , [ Expr ":expr-quantified-body"         [f is]  ]
                        ]
                        -- QuantifiedExpr qnName i qnDom qnExpr (QuanGuard []) (Q $ f is)
        return $ f qnVars

parseStructural :: Parser Core
parseStructural = msum1
    [ do
        x <- parseMetaVariable
        return $ Expr ":structural-single" [x]
    , do
        x <- parseReference
        return $ Expr ":structural-single" [x]
    , do
        xs <- parens $ parseStructural `sepBy1` comma
        return $ Expr ":structural-tuple" xs
    , do
        xs <- brackets $ parseStructural `sepBy1` comma
        return $ Expr ":structural-matrix" xs
    ]

parseRuleRefn :: Text -> Parser RuleRefn
parseRuleRefn t = do
    whiteSpace
    level     <- optionMaybe (brackets (fromInteger <$> integer))
    pattern   <- parseExpr
    templates <- some (lexeme L_SquigglyArrow >> parseExpr)
    locals    <- concat <$> many parseTopLevels
    eof
    return ( t
           , level
           , Expr ":rulerefn"
                [ Expr ":rulerefn-pattern"   [pattern]
                , Expr ":rulerefn-templates" templates
                , Expr ":rulerefn-locals"    locals
                ]
           )
