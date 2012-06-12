{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.TypeOf ( typeOf, typeUnify ) where

import Language.Core.Imports
import Language.Core.Definition

import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty
import Language.Core.Parser
import Language.EssenceLexerP ( lexAndParseIO, eof )


errInvariant :: (Monad m, Pretty a) => Doc -> a -> CompT m b
-- errInvariant p = err $ "TypeOf, invariant violation in:" <+> (stringToDoc $ show p)
-- errInvariant p = err $ "TypeOf, invariant violation in:" <+> (stringToDoc $ ppShow p)
-- errInvariant p = err $ "TypeOf, invariant violation in:" <+> showAST p
errInvariant doc p =
    err ErrTypeOf $ singletonNested
                  $ "TypeOf, invariant violation in (" <+> doc <+> ")" <++> pretty p

errCannot :: (Monad m, ShowAST a) => a -> CompT m b
errCannot p =
    err ErrTypeOf $ singletonNested
                  $ "Cannot determine the type of" <+> showAST p

errMismatch :: (Monad m, Pretty a) => Doc -> a -> CompT m b
errMismatch doc p =
    err ErrTypeOf $ singletonNested
                  $ "Type error in" <+> pretty p $$ doc
                  

_tester_typeOfDomain :: Text -> IO ()
_tester_typeOfDomain t = do
    xs <- lexAndParseIO (parseDomain <* eof)  t
    case xs of
        [x] -> do
            y  <- runCompIO def def (typeOf x)
            print $ showAST x
            print $ showAST y
        _ -> do
            mapM_ (print . showAST) xs

_tester_typeOf :: Text -> IO ()
_tester_typeOf t = do
    xs <- lexAndParseIO (parseExpr <* eof)  t
    case xs of
        [x] -> do
            y  <- runCompIO def def (typeOf x)
            print $ showAST x
            print $ showAST y
        _ -> do
            mapM_ (print . showAST) xs


class TypeOf a where
    typeOf :: (Functor m, Monad m) => a -> CompT m Core

instance TypeOf Core where
    typeOf (L x) = typeOf x
    typeOf (R x) = typeOf x
    typeOf ( viewDeep [":metavar"] -> Just [R x] ) = typeOf ("@" `mappend` x)

    typeOf ( viewDeep [":toplevel",":declaration",":find"]
              -> Just [ Expr ":find-name" _
                      , Expr ":find-domain" [domain]
                      ]
           ) = typeOf domain

    typeOf p@( viewDeep [":type"] -> Just _) = return p

    typeOf ( viewDeep [":empty-guard"] -> Just _ ) = return $ Expr ":type" [Expr ":type-bool" []]

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
                    else errMismatch "range" p
            Expr ":range-single" [x] -> do
                tx <- typeOf x
                return $
                    Expr ":type"
                        [ Expr ":type-range" [tx]
                        ]
            _ -> errInvariant "range" p
    typeOf p@(Expr ":domain" [d]) = do
        x <- case d of
            R r -> typeOf r
            Expr ":domain-bool"   _  ->
                return $ Expr ":type-bool" []
            Expr ":domain-int"    _  ->
                return $ Expr ":type-int"  []
            Expr ":domain-enum"   xs ->
                case lookUpInExpr ":domain-enum-name" xs of
                    Just [a] -> return $ Expr ":type-enum" [ Expr ":type-enum-name" [a] ]
                    _ -> errInvariant "domain-enum" p
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
                         _ -> errInvariant "domain-matrix" p
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
                    _ -> errInvariant "domain-set" p
            Expr ":domain-mset" xs ->
                case lookUpInExpr ":domain-mset-inner" xs of
                    Just [a] -> do
                        ta <- typeOf a
                        return $
                            Expr ":type-mset"
                                [ Expr ":type-mset-inner" [ta] ]
                    _ -> errInvariant "domain-mset" p
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
                         _ -> errInvariant "domain-function" p
            Expr ":domain-relation" xs ->
                case lookUpInExpr ":domain-relation-inners" xs of
                    Just as -> do
                        tas <- mapM typeOf as
                        return $
                            Expr ":type-relation"
                                [ Expr ":type-relation-inners" tas
                                ]
                    _ -> errInvariant "domain-relation" p
            Expr ":domain-partition" xs ->
                case lookUpInExpr ":domain-partition-inner" xs of
                    Just [a] -> do
                        ta <- typeOf a
                        return $
                            Expr ":type-partition"
                                [ Expr ":type-partition-inner" [ta]
                                ]
                    _ -> errInvariant "domain-partition" p
            _ -> errInvariant "domain" p
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
                                        else errMismatch "value" p
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
                                        else errMismatch "value" p
                         _ -> errInvariant "value-matrix" p
            Expr ":value-tuple" xs -> do
                txs <- mapM typeOf xs
                return $
                    Expr ":type"
                        [ Expr ":type-tuple" txs
                        ]
            Expr ":value-set" xs -> do
                txs <- mapM typeOf xs
                case txs of
                    [] -> return $ Expr ":type" [
                                   Expr ":type-set" [
                                   Expr ":type-set-inner" [
                                   Expr ":type-unknown" [
                                   ]]]]
                    (i:is) -> do
                        flags <- mapM (typeUnify i) is
                        if and flags
                            then return $ Expr ":type" [
                                          Expr ":type-set" [
                                          Expr ":type-set-inner" [i]
                                          ]]
                            else errMismatch "value-set" p
            Expr ":value-mset" xs -> do
                txs <- mapM typeOf xs
                case txs of
                    [] -> return $ Expr ":type" [
                                   Expr ":type-mset" [
                                   Expr ":type-mset-inner" [
                                   Expr ":type-unknown" [
                                   ]]]]
                    (i:is) -> do
                        flags <- mapM (typeUnify i) is
                        if and flags
                            then return $ Expr ":type" [
                                          Expr ":type-mset" [
                                          Expr ":type-mset-inner" [i]
                                          ]]
                            else errMismatch "value-mset" p
            Expr ":value-function" xs -> do
                let
                    getOut (Expr ":value-function-mapping" [a,b]) = do
                        ta <- typeOf a
                        tb <- typeOf b
                        return (ta,tb)
                    getOut _ = errInvariant "value-function" p
                ys <- mapM getOut xs
                case ys of
                    [] -> errCannot p
                    (i@(a,b):is) ->
                        if all (i==) is
                            then return $ Expr ":type" [Expr ":type-function" [a,b]]
                            else errMismatch "value-function" p
            Expr ":value-relation" xs -> do
                txs <- mapM typeOf xs
                case txs of
                    [] -> errCannot p
                    (i:is) ->
                        if all (i==) is
                            then return $ Expr ":type" [Expr ":type-relation" [i]]
                            else errMismatch "value-relation" p
            Expr ":value-partition" xs -> do
                let
                    getOut (Expr ":value-partition-part" as) = do
                        tas <- mapM typeOf as
                        case tas of
                            [] -> errCannot p
                            (i:is) ->
                                if all (i==) is
                                    then return $ Expr ":type" [i]
                                    else errMismatch "value-partition" p
                    getOut _ = errInvariant "value-partition" p
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
                            else errMismatch "value-partition" p
            _ -> errInvariant "value" p

    typeOf p@( Expr ":quanVar" [ Expr ":quanVar-name" _
                               , Expr ":quanVar-within"
                                    [ Expr ":expr-quantified" xs ]
                               ]
             ) = do
        let
            quanOverDom'  = lookUpInExpr ":expr-quantified-quanOverDom"   xs
            quanOverOp'   = lookUpInExpr ":expr-quantified-quanOverOp"    xs
            quanOverExpr' = lookUpInExpr ":expr-quantified-quanOverExpr"  xs
        case (quanOverDom', quanOverOp', quanOverExpr') of
            (Just [quanOverDom], Nothing, Nothing) ->
                typeOf quanOverDom
            (Nothing, Just [Expr ":operator-in" []], Just [x]) -> do
                tx <- typeOf x
                innerTypeOf tx
            (Nothing, Just [Expr ":operator-subset" []], Just [x]) -> do
                tx <- typeOf x
                return tx
            (Nothing, Just [Expr ":operator-subsetEq" []], Just [x]) -> do
                tx <- typeOf x
                return tx
            _ -> errInvariant "quanVar" p

    typeOf p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ R quantifier           ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , Just [ Expr ":structural-single" [R r]
               ]                          <- lookUpInExpr ":expr-quantified-quanVar"      xs
        , Just [ qnGuard ]                <- lookUpInExpr ":expr-quantified-guard"        xs
        , Just [ qnBody  ]                <- lookUpInExpr ":expr-quantified-body"         xs
        = do
            bindersBefore <- gets binders
            let restoreState = modify $ \ st -> st { binders = bindersBefore }
            addBinder r
                    $ Expr ":quanVar" [ Expr ":quanVar-name"   [R r]
                                      , Expr ":quanVar-within" [p]
                                      ]
            tyGuard    <- typeOf qnGuard
            tyBody     <- typeOf qnBody

            unless (tyGuard == Expr ":type" [Expr ":type-bool" []])
                $ errMismatch "guard must be a boolean expression" p

            result <- case () of
                _ | quantifier `elem` ["forAll", "exists"] -> do
                    unless (tyBody == Expr ":type" [Expr ":type-bool" []])
                        $ errMismatch "body must be a boolean expression" p
                    return $ Expr ":type" [Expr ":type-bool" []]
                _ | quantifier == "sum" -> do
                    unless (tyBody == Expr ":type" [Expr ":type-int" []])
                        $ errMismatch "body must be an integer expression" p
                    return $ Expr ":type" [Expr ":type-int" []]
                _ -> errInvariant "typeOf" p

            restoreState
            return result

    typeOf _p@( viewDeep [":operator-toSet"]
                 -> Just [a]
              ) = do
        ta <- typeOf a
        let
            checkAndReturn ( viewDeep [":type",":type-mset",":type-mset-inner"] -> Just [b] )
                = return $ Expr ":type"
                         [ Expr ":type-set"
                         [ Expr ":type-set-inner" [b]]]
            checkAndReturn q = errInvariant "toSet" q
        checkAndReturn ta

    typeOf _p@( viewDeep [":operator-twobars"]
                 -> Just [a]
              ) = do
        ta <- typeOf a
        let
            checkAndReturn  q@( viewDeep [":type",":type-int"] -> Just _ )
                = return q
            checkAndReturn _q@( viewDeep [":type",":type-set"] -> Just _ )
                = return $ Expr ":type" [Expr ":type-int"  []]
            checkAndReturn _q@( viewDeep [":type",":type-mset"] -> Just _ )
                = return $ Expr ":type" [Expr ":type-int"  []]
            checkAndReturn  q = errInvariant "twobars" q
        checkAndReturn ta

    typeOf p@( viewDeep [":operator-toInt"]
                -> Just [a]
             ) = do
        ta <- typeOf a
        flag <- typeUnify ta $ Expr ":type" [Expr ":type-bool" []]
        if flag
            then return $ Expr ":type" [Expr ":type-int" []]
            else errMismatch "expecting a boolean expression inside toInt" p

    typeOf p@( viewDeep [":operator-union"]
                -> Just [a,b]
             ) = do
        ta <- typeOf a
        tb <- typeOf b
        flag <- typeUnify ta tb
        if flag
            then return ta
            else errMismatch "operator-union" p

    typeOf p@( viewDeep [":operator-intersect"]
                -> Just [a,b]
             ) = do
        ta <- typeOf a
        tb <- typeOf b
        flag <- typeUnify ta tb
        if flag
            then return ta
            else errMismatch "operator-intersect" p

    typeOf p@( viewDeep [":operator-+"] -> Just [a,b] ) = intToIntToInt p a b
    typeOf p@( viewDeep [":operator-*"] -> Just [a,b] ) = intToIntToInt p a b
    typeOf p@( viewDeep [":operator-/"] -> Just [a,b] ) = intToIntToInt p a b
    typeOf p@( viewDeep [":operator-%"] -> Just [a,b] ) = intToIntToInt p a b

    -- Int -> Int -> Int
    -- set of a -> set of a -> set of a
    -- mset of a -> mset of a -> mset of a
    typeOf p@( viewDeep [":operator--"]
                -> Just [a,b]
             ) = do
        ta <- typeOf a
        tb <- typeOf b
        flag <- typeUnify ta tb
        if not flag
            then errMismatch "operator (-)" p
            else do
                let
                    checkAndReturn q@( viewDeep [":type",":type-int" ] -> Just [ ]) = return q
                    checkAndReturn q@( viewDeep [":type",":type-set" ] -> Just [_]) = return q
                    checkAndReturn q@( viewDeep [":type",":type-mset"] -> Just [_]) = return q
                    checkAndReturn q = errMismatch "checkAndReturn" q
                checkAndReturn ta

    typeOf p = do
        mkLog "typeOf" $ "catch all case" <++> pretty p
        return $ Expr ":type" [Expr ":type-unknown" []]

instance TypeOf Literal where
    typeOf (B {}) = return $ Expr ":type" [Expr ":type-bool" []]
    typeOf (I {}) = return $ Expr ":type" [Expr ":type-int"  []]

instance TypeOf Reference where
    typeOf "_" = return $ Expr ":type-unknown" []
    typeOf r = do
        val <- lookUpRef r
        typeOf val

intToIntToInt :: (Monad m, Functor m, Pretty a, TypeOf a) => a -> a -> a -> CompT m Core
intToIntToInt p a b = do
    ta <- typeOf a
    tb <- typeOf b
    flag <- typeUnify ta tb
    if not flag
        then errMismatch "intToIntToInt" p
        else do
            let
                checkAndReturn q@( viewDeep [":type",":type-int" ] -> Just [ ]) = return q
                checkAndReturn _ = errMismatch "intToIntToInt.checkAndReturn" p
            checkAndReturn ta


innerTypeOf :: Monad m => Core -> CompT m Core
innerTypeOf ( viewDeep [":type",":type-set" ,":type-set-inner" ] -> Just [t] ) = return t
innerTypeOf ( viewDeep [":type",":type-mset",":type-mset-inner"] -> Just [t] ) = return t
innerTypeOf p = errInvariant "innerTypeOf" p

typeUnify :: (Functor m, Monad m) => Core -> Core -> CompT m Bool
typeUnify (viewDeep [":type-unknown"] -> Just []) _ = return True
typeUnify _ (viewDeep [":type-unknown"] -> Just []) = return True
typeUnify (Expr t1 xs1) (Expr t2 xs2)
    | t1 == t2
    , length xs1 == length xs2
    = and <$> zipWithM typeUnify xs1 xs2
typeUnify x y = do
    mkLog "typeUnify" $ "default case" <++>
                        vcat [ pretty x
                             , "~~"
                             , pretty y
                             ]
    return $ x == y
