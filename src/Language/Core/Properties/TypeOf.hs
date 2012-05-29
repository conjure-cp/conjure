{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Properties.TypeOf where


import Language.Core
import Language.Core.Properties.Pretty
import Language.Core.Parser
import Language.EssenceLexerP ( lexAndParseIO, eof )


errInvariant :: (Monad m, Pretty a) => a -> CompT m b
errInvariant p = err $ "TypeOf, invariant violation in:" <+> pretty p

errCannot :: (Monad m, Pretty a) => a -> CompT m b
errCannot p = err $ "Cannot determine the type of" <+> pretty p

errMismatch :: (Monad m, Pretty a) => a -> CompT m b
errMismatch p = err $ "Type error in" <+> pretty p

tester_typeOfDomain :: Text -> IO ()
tester_typeOfDomain t = do
    xs <- lexAndParseIO (parseDomain <* eof)  t
    case xs of
        [x] -> do
            y  <- runCompIO def def (typeOf x)
            print $ pretty x
            print $ pretty y
        _ -> do
            mapM_ (print . pretty) xs

tester_typeOf :: Text -> IO ()
tester_typeOf t = do
    xs <- lexAndParseIO (parseExpr <* eof)  t
    case xs of
        [x] -> do
            y  <- runCompIO def def (typeOf x)
            print $ pretty x
            print $ pretty y
        _ -> do
            mapM_ (print . pretty) xs


class TypeOf a where
    typeOf :: Monad m => a -> CompT m Core

instance TypeOf Core where
    typeOf (L x) = typeOf x
    typeOf (R x) = typeOf x
    typeOf p@(Expr ":range"  [d]) =
        case d of
            Expr ":range-open" []  -> errCannot p
            Expr ":range-from" [x] -> do
                tx <- typeOf x
                return $
                    Expr ":type"
                        [ Expr ":type-range" [tx]
                        ]
            Expr ":range-to" [x] -> do
                tx <- typeOf x
                return $
                    Expr ":type"
                        [ Expr ":type-range" [tx]
                        ]
            Expr ":range-fromto" [x,y] -> do
                tx <- typeOf x
                ty <- typeOf y
                if tx == ty
                    then return $
                            Expr ":type"
                                [ Expr ":type-range" [tx]
                                ]
                    else errMismatch p
            Expr ":range-single" [x] -> do
                tx <- typeOf x
                return $
                    Expr ":type"
                        [ Expr ":type-range" [tx]
                        ]
            _ -> errInvariant p
    typeOf p@(Expr ":domain" [d]) = do
        x <- case d of
            Expr ":domain-bool"   _  ->
                return $ Expr ":type-bool" []
            Expr ":domain-int"    _  ->
                return $ Expr ":type-int"  []
            Expr ":domain-enum"   xs ->
                case lookUpInExpr ":domain-enum-name" xs of
                    Just [a] -> return $ Expr ":type-enum" [ Expr ":type-enum-name" [a] ]
                    _ -> errInvariant p
            Expr ":domain-matrix" xs ->
                case ( lookUpInExpr ":domain-matrix-index" xs
                     , lookUpInExpr ":domain-matrix-inner" xs
                     ) of
                         (Just [a], Just [b]) -> do
                            ta <- typeOf a
                            tb <- typeOf b
                            return $
                                Expr ":type-matrix"
                                    [ Expr ":type-matrix-index" [ta]
                                    , Expr ":type-matrix-inner" [tb]
                                    ]
                         _ -> errInvariant p
            Expr ":domain-tuple" [Expr ":domain-tuple-inners" xs] -> do
                txs <- mapM typeOf xs
                return $
                    Expr ":type-tuple"
                        [ Expr ":type-tuple-inners" txs
                        ]
            Expr ":domain-set" xs ->
                case lookUpInExpr ":domain-set-inner" xs of
                    Just [a] -> do
                        ta <- typeOf a
                        return $
                            Expr ":type-set"
                                [ Expr ":type-set-inner" [ta] ]
                    _ -> errInvariant p
            Expr ":domain-mset" xs ->
                case lookUpInExpr ":domain-mset-inner" xs of
                    Just [a] -> do
                        ta <- typeOf a
                        return $
                            Expr ":type-mset"
                                [ Expr ":type-mset-inner" [ta] ]
                    _ -> errInvariant p
            Expr ":domain-function" xs ->
                case ( lookUpInExpr ":domain-function-innerfrom" xs
                     , lookUpInExpr ":domain-function-innerto" xs
                     ) of
                         (Just [a], Just [b]) -> do
                             ta <- typeOf a
                             tb <- typeOf b
                             return $
                                Expr ":type-function"
                                    [ Expr ":type-function-innerfrom" [ta]
                                    , Expr ":type-function-innerto" [tb]
                                    ]
                         _ -> errInvariant p
            Expr ":domain-relation" xs ->
                case lookUpInExpr ":domain-relation-inners" xs of
                    Just as -> do
                        tas <- mapM typeOf as
                        return $
                            Expr ":type-relation"
                                [ Expr ":type-relation-inners" tas
                                ]
                    _ -> errInvariant p
            Expr ":domain-partition" xs ->
                case lookUpInExpr ":domain-partition-inner" xs of
                    Just [a] -> do
                        ta <- typeOf a
                        return $
                            Expr ":type-partition"
                                [ Expr ":type-partition-inner" [ta]
                                ]
                    _ -> errInvariant p
            _ -> errInvariant p
        return $ Expr ":type" [x]
    typeOf p@(Expr ":value"  [d]) =
        case d of
            Expr ":value-literal" [L x] -> typeOf x
            Expr ":value-matrix" xs ->
                case ( lookUpInExpr ":value-matrix-indexrange"  xs
                     , lookUpInExpr ":value-matrix-values" xs
                     ) of
                         (Just [a], Just ys) -> do
                             ta  <- typeOf a
                             tys <- mapM typeOf ys
                             case tys of
                                 [] -> errCannot p
                                 (i:is) ->
                                    if all (i==) is
                                        then return $
                                                Expr ":type"
                                                    [ Expr ":type-matrix"
                                                        [ Expr ":type-matrix-indexrange" [ta]
                                                        , Expr ":type-matrix-values"     [i]
                                                        ]
                                                    ]
                                        else errMismatch p
                         (Nothing, Just ys) -> do
                             tys <- mapM typeOf ys
                             case tys of
                                 [] -> errCannot p
                                 (i:is) ->
                                    if all (i==) is
                                        then return $
                                                Expr ":type"
                                                    [ Expr ":type-matrix"
                                                        [Expr ":type-matrix-values" [i]]
                                                    ]
                                        else errMismatch p
                         _ -> errInvariant p
            Expr ":value-tuple" xs -> do
                txs <- mapM typeOf xs
                return $
                    Expr ":type"
                        [ Expr ":type-tuple" txs
                        ]
            Expr ":value-set" xs -> do
                txs <- mapM typeOf xs
                case txs of
                    [] -> errCannot p
                    (i:is) ->
                        if all (i==) is
                            then return $ Expr ":type" [Expr ":type-set" [i]]
                            else errMismatch p
            Expr ":value-mset" xs -> do
                txs <- mapM typeOf xs
                case txs of
                    [] -> errCannot p
                    (i:is) ->
                        if all (i==) is
                            then return $ Expr ":type" [Expr ":type-mset" [i]]
                            else errMismatch p
            Expr ":value-function" xs -> do
                let
                    getOut (Expr ":value-function-mapping" [a,b]) = do
                        ta <- typeOf a
                        tb <- typeOf b
                        return (ta,tb)
                    getOut _ = errInvariant p
                ys <- mapM getOut xs
                case ys of
                    [] -> errCannot p
                    (i@(a,b):is) ->
                        if all (i==) is
                            then return $ Expr ":type" [Expr ":type-function" [a,b]]
                            else errMismatch p
            Expr ":value-relation" xs -> do
                txs <- mapM typeOf xs
                case txs of
                    [] -> errCannot p
                    (i:is) ->
                        if all (i==) is
                            then return $ Expr ":type" [Expr ":type-relation" [i]]
                            else errMismatch p
            Expr ":value-partition" xs -> do
                let
                    getOut (Expr ":value-partition-part" as) = do
                        tas <- mapM typeOf as
                        case tas of
                            [] -> errCannot p
                            (i:is) ->
                                if all (i==) is
                                    then return $ Expr ":type" [i]
                                    else errMismatch p
                    getOut _ = errInvariant p
                ys <- mapM getOut xs
                case ys of
                    [] -> errCannot p
                    (i:is) ->
                        if all (i==) is
                            then return $
                                    Expr ":type"
                                        [ Expr ":type-partition"
                                            [ Expr ":type-partition-inner" [i]
                                            ]
                                        ]
                            else errMismatch p
            _ -> errInvariant p
    typeOf p     = errInvariant p

instance TypeOf Literal where
    typeOf (B {}) = return $ Expr ":type" [Expr ":type-bool" []]
    typeOf (I {}) = return $ Expr ":type" [Expr ":type-int"  []]

instance TypeOf Reference where
    typeOf r = core <?> "Reference.typeOf:" <+> pretty r
        where
            core = do
                val <- lookUpRef r
                typeOf val
