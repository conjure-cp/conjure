{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.Simplify.Internal where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.MatchBind ( match )
import Language.Core.Properties.TypeOf
import Language.Core.Properties.DomainOf
import Language.Core.Properties.ShowAST
import Language.Core.Properties.Pretty


class Simplify a where
    simplify :: (Functor m, Monad m) => a -> WriterT Any (CompT m) Core

instance Simplify Core where

    simplify p@(L {}) = return p
    simplify p@(R {}) = return p

    -- simplify (Expr ":negate" [Expr ":value" [Expr ":value-literal" [L (I i)]]])
    --     = do
    --         tell $ Any True
    --         return $ Expr ":value" [Expr ":value-literal" [L $ I $ negate i]]

    simplify ( viewDeep [":metavar"] -> Just [R x] ) = simplify ("@" `mappend` x)

    simplify  p@( viewDeep [":operator-hastype"] -> Just [a,b] ) = do
        lift $ mkLog "simplify" $ pretty p
        ta   <- lift $ typeOf a
        tb   <- lift $ typeOf b
        -- lift $ mkLog "debug" $ vcat [ pretty p
        --                             , stringToDoc $ show a, pretty ta
        --                             , stringToDoc $ show b, pretty tb
        --                             ]
        flag <- lift $ typeUnify ta tb
        tell $ Any True
        return $ L $ B flag
    simplify  p@( viewDeep [":operator-hasdomain"] -> Just [a,b] ) = do
        lift $ mkLog "simplify" $ pretty p
        da   <- lift $ domainOf a
        db   <- lift $ domainOf b
        flag <- lift $ domainUnify da db
        tell $ Any True
        return $ L $ B flag

    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B True)]]
                           , _
                           ]
                 ) = returnTrue
    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ _
                           , Expr ":value" [Expr ":value-literal" [L (B True)]]
                           ]
                 ) = returnTrue

    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B False)]]
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-\\/"]
                   -> Just [ x
                           , Expr ":value" [Expr ":value-literal" [L (B False)]]
                           ]
                 ) = tell (Any True) >> return x


    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ Expr ":empty-guard" []
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ x
                           , Expr ":empty-guard" []
                           ]
                 ) = tell (Any True) >> return x

    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B True)]]
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ x
                           , Expr ":value" [Expr ":value-literal" [L (B True)]]
                           ]
                 ) = tell (Any True) >> return x

    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B False)]]
                           ,_
                           ]
                 ) = returnFalse
    simplify _p@( viewDeep [":operator-/\\"]
                   -> Just [ _
                           , Expr ":value" [Expr ":value-literal" [L (B False)]]
                           ]
                 ) = returnFalse

    simplify _p@( viewDeep [":operator-not"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (B b)]]
                           ]
                 ) = do
                     tell (Any True)
                     return $ valueBool $ not b


    simplify _p@( viewDeep [":operator-+"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (I 0)]]
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-+"]
                   -> Just [ x
                           , Expr ":value" [Expr ":value-literal" [L (I 0)]]
                           ]
                 ) = tell (Any True) >> return x


    simplify _p@( viewDeep [":operator-*"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (I 1)]]
                           , x
                           ]
                 ) = tell (Any True) >> return x
    simplify _p@( viewDeep [":operator-*"]
                   -> Just [ x
                           , Expr ":value" [Expr ":value-literal" [L (I 1)]]
                           ]
                 ) = tell (Any True) >> return x


    simplify _p@( viewDeep [":operator-*"]
                   -> Just [ Expr ":value" [Expr ":value-literal" [L (I 0)]]
                           , _
                           ]
                 ) = do tell (Any True)
                        return $ Expr ":value" [Expr ":value-literal" [L (I 0)]]
    simplify _p@( viewDeep [":operator-*"]
                   -> Just [ _
                           , Expr ":value" [Expr ":value-literal" [L (I 0)]]
                           ]
                 ) = do tell (Any True)
                        return $ Expr ":value" [Expr ":value-literal" [L (I 0)]]


    -- simplify _p@( viewDeep [":operator-\\/"] -> Just [a,b] ) = do
    --     a' <- simplify a
    --     b' <- simplify b
    --     return $ Expr ":operator-\\/" [a',b']
    simplify _p@( viewDeep [":operator-="] -> Just [R a,R b] ) | a == b = do
        tell $ Any True
        returnTrue

    simplify _p@( viewDeep [":operator-="    ] -> Just [ Expr ":value" [Expr ":value-literal" [L a]]
                                                       , Expr ":value" [Expr ":value-literal" [L b]]
                                                       ] ) = intToIntToBool (==) a b
    simplify _p@( viewDeep [":operator-!="   ] -> Just [ Expr ":value" [Expr ":value-literal" [L a]]
                                                       , Expr ":value" [Expr ":value-literal" [L b]]
                                                       ] ) = intToIntToBool (/=) a b
    simplify _p@( viewDeep [":operator->="   ] -> Just [ Expr ":value" [Expr ":value-literal" [L a]]
                                                       , Expr ":value" [Expr ":value-literal" [L b]]
                                                       ] ) = intToIntToBool (>=) a b
    simplify _p@( viewDeep [":operator->"    ] -> Just [ Expr ":value" [Expr ":value-literal" [L a]]
                                                       , Expr ":value" [Expr ":value-literal" [L b]]
                                                       ] ) = intToIntToBool (>) a b
    simplify _p@( viewDeep [":operator-<="   ] -> Just [ Expr ":value" [Expr ":value-literal" [L a]]
                                                       , Expr ":value" [Expr ":value-literal" [L b]]
                                                       ] ) = intToIntToBool (<=) a b
    simplify _p@( viewDeep [":operator-<"    ] -> Just [ Expr ":value" [Expr ":value-literal" [L a]]
                                                       , Expr ":value" [Expr ":value-literal" [L b]]
                                                       ] ) = intToIntToBool (<) a b
    simplify _p@( viewDeep [":operator-toInt"] -> Just [ Expr ":value" [Expr ":value-literal" [L (B a)]]
                                                       ] ) = do tell (Any True)
                                                                return $ valueInt $ if a then 1 else 0
    simplify _p@( viewDeep [":operator-not"  ] -> Just [ Expr ":value" [Expr ":value-literal" [L (B a)]]
                                                       ] ) = do tell (Any True)
                                                                return $ valueBool $ not a


    simplify _p@( viewDeep [":operator-="] -> Just [ Expr ":operator-" [Expr ":value-literal" [L (B a)]]
                                                       ] ) = do tell (Any True)
                                                                return $ valueInt $ if a then 1 else 0

    simplify _p@( viewDeep [":operator-+"    ] -> Just [ Expr ":value" [Expr ":value-literal" [L (I a)]]
                                                       , Expr ":value" [Expr ":value-literal" [L (I b)]]
                                                       ] ) = intToIntToInt (+) a b
    simplify _p@( viewDeep [":operator--"    ] -> Just [ Expr ":value" [Expr ":value-literal" [L (I a)]]
                                                       , Expr ":value" [Expr ":value-literal" [L (I b)]]
                                                       ] ) = intToIntToInt (-) a b
    simplify _p@( viewDeep [":operator-*"    ] -> Just [ Expr ":value" [Expr ":value-literal" [L (I a)]]
                                                       , Expr ":value" [Expr ":value-literal" [L (I b)]]
                                                       ] ) = intToIntToInt (*) a b
    simplify _p@( viewDeep [":operator-/"    ] -> Just [ Expr ":value" [Expr ":value-literal" [L (I a)]]
                                                       , Expr ":value" [Expr ":value-literal" [L (I b)]]
                                                       ] ) | b /= 0 = intToIntToInt div a b
    simplify _p@( viewDeep [":operator-%"    ] -> Just [ Expr ":value" [Expr ":value-literal" [L (I a)]]
                                                       , Expr ":value" [Expr ":value-literal" [L (I b)]]
                                                       ] ) | b /= 0 = intToIntToInt mod a b

    simplify _p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ body       ] <- lookUpInExpr ":expr-quantified-body" xs
        , Just [ R "forAll" ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , valueFalse == body
        = returnFalse

    simplify _p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ body         ] <- lookUpInExpr ":expr-quantified-body" xs
        , Just [ R quantifier ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , qnIdentity quantifier == body
        = do
            tell (Any True)
            return body

    simplify _p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ R quantifier           ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , Just [ Expr ":operator-in" [] ] <- lookUpInExpr ":expr-quantified-quanOverOp"   xs
        , Just [ qnOverExpr             ] <- lookUpInExpr ":expr-quantified-quanOverExpr" xs
        , Just [ Expr ":value"
               [ Expr ":value-set" vs
               ]]                         <- lookUpInExpr ":expr-quantified-quanOverExpr" xs
        , Just [ Expr ":structural-single" [qnVar]
               ]                          <- lookUpInExpr ":expr-quantified-quanVar"      xs
        , Just [ qnGuard ]                <- lookUpInExpr ":expr-quantified-guard"        xs
        , Just [ qnBody  ]                <- lookUpInExpr ":expr-quantified-body"         xs
        = quantificationOverValueSet_In quantifier qnOverExpr vs qnVar qnGuard qnBody
    simplify _p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ R quantifier           ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , Just [ Expr ":operator-in" [] ] <- lookUpInExpr ":expr-quantified-quanOverOp"   xs
        , Just [ qnOverExpr             ] <- lookUpInExpr ":expr-quantified-quanOverExpr" xs
        , Just [ Expr ":typed" [ Expr ":value" [Expr ":value-set" vs], _ ]
                                        ] <- lookUpInExpr ":expr-quantified-quanOverExpr" xs
        , Just [ Expr ":structural-single" [qnVar]
               ]                          <- lookUpInExpr ":expr-quantified-quanVar"      xs
        , Just [ qnGuard ]                <- lookUpInExpr ":expr-quantified-guard"        xs
        , Just [ qnBody  ]                <- lookUpInExpr ":expr-quantified-body"         xs
        = quantificationOverValueSet_In quantifier qnOverExpr vs qnVar qnGuard qnBody


    simplify _p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ R quantifier           ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , Just [ Expr ":operator-in" [] ] <- lookUpInExpr ":expr-quantified-quanOverOp"   xs
        , Just [ Expr ":value" [
                 Expr ":value-mset" vs
               ]                        ] <- lookUpInExpr ":expr-quantified-quanOverExpr" xs
        , Just [ Expr ":structural-single" [qnVar]
               ]                          <- lookUpInExpr ":expr-quantified-quanVar"      xs
        , Just [ qnGuard ]                <- lookUpInExpr ":expr-quantified-guard"        xs
        , Just [ qnBody  ]                <- lookUpInExpr ":expr-quantified-body"         xs
        = quantificationOverValueMSet_In quantifier vs qnVar qnGuard qnBody
    simplify _p@( viewDeep [":expr-quantified"] -> Just xs )
        | Just [ R quantifier           ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
        , Just [ Expr ":operator-in" [] ] <- lookUpInExpr ":expr-quantified-quanOverOp"   xs
        , Just [ Expr ":typed" [ Expr ":value" [Expr ":value-mset" vs], _ ]
                                        ] <- lookUpInExpr ":expr-quantified-quanOverExpr" xs
        , Just [ Expr ":structural-single" [qnVar]
               ]                          <- lookUpInExpr ":expr-quantified-quanVar"      xs
        , Just [ qnGuard ]                <- lookUpInExpr ":expr-quantified-guard"        xs
        , Just [ qnBody  ]                <- lookUpInExpr ":expr-quantified-body"         xs
        = quantificationOverValueMSet_In quantifier vs qnVar qnGuard qnBody


    simplify p@(Expr t xs) = do
        ys <- mapM simplify xs
        let result = Expr t ys
        lift $ mkLog "simplify-generic-case"
             $ "generic case:" <++> vcat [ pretty p
                                         , pretty result
                                         ]
        return result
    -- simplify x = do
    --     lift $ mkLog "simplify" $ "default case:" <++>
    --                             vcat [ pretty x
    --                                  , showAST x
    --                                  , stringToDoc $ show x
    --                                  ]
    --     return x

instance Simplify Reference where
    simplify r =
        catchError
            ( do
                val <- lift $ lookUpRef r
                tell (Any True)
                simplify val
            )
            (\ _ -> lift $ err ErrUndefinedReference
                            $ singletonNested
                            $ "Reference.simplify:" <+> showAST r
            )
        -- lift $
        --     catchIf
        --         core
        --         (ErrLookUpRef==)
        --         $ \ _ -> 
        -- where
        --     core = do
        --         val <- lift $ lookUpRef r
        --         tell $ Any True
        --         simplify val


returnTrue :: (Functor m, Monad m) => WriterT Any (CompT m) Core
returnTrue = tell (Any True) >> return valueTrue

returnFalse :: (Functor m, Monad m) => WriterT Any (CompT m) Core
returnFalse = tell (Any True) >> return valueFalse

domainUnify :: Monad m => Core -> Core -> CompT m Bool
domainUnify y x = do
    mkLog "domainUnify" $ pretty x <+> "~~" <++> pretty y
    match x y


intToIntToBool :: Monad m => (Literal -> Literal -> Bool) -> Literal -> Literal -> WriterT Any (CompT m) Core
intToIntToBool f a b = do
    tell (Any True)
    return $ valueBool $ f a b

intToIntToInt :: Monad m => (Integer -> Integer -> Integer) -> Integer -> Integer -> WriterT Any (CompT m) Core
intToIntToInt f a b = do
    tell (Any True)
    return $ valueInt $ f a b


quantificationOverValueMSet_In :: Monad m
    => Reference
    -> [Core]
    -> Core
    -> Core
    -> Core
    -> WriterT Any (CompT m) Core
quantificationOverValueMSet_In quantifier vs qnVar qnGuard qnBody = do
    tell $ Any True
    let
        guardOp (Expr ":empty-guard" []) b = return b
        guardOp a b = case quantifier of
                        "forAll" -> return $ Expr ":operator-->"  [a, b]
                        "exists" -> return $ Expr ":operator-/\\" [a, b]
                        "sum"    -> return $ Expr ":operator-*"   [a, b]
                        _        -> lift $ err ErrInvariant
                                            $ singletonNested
                                            $ "unknown quantifier in simplify" <+> pretty quantifier
        glueOp a b = case quantifier of
                        "forAll" -> return $ Expr ":operator-/\\" [a, b]
                        "exists" -> return $ Expr ":operator-\\/" [a, b]
                        "sum"    -> return $ Expr ":operator-+"   [a, b]
                        _        -> lift $ err ErrInvariant
                                            $ singletonNested
                                            $ "unknown quantifier in simplify" <+> pretty quantifier
        identity = case quantifier of
                        "forAll" -> return valueTrue
                        "exists" -> return valueFalse
                        "sum"    -> return (valueInt 0)
                        _        -> lift $ err ErrInvariant
                                            $ singletonNested
                                            $ "unknown quantifier in simplify" <+> pretty quantifier

    identity' <- identity
    vs' <- sequence [ guardOp (replaceCore qnVar v qnGuard)
                              (replaceCore qnVar v qnBody)
                    | v <- vs ]
    foldM glueOp identity' vs'


quantificationOverValueSet_In :: (Functor m, Monad m)
    => Reference
    -> Core
    -> [Core]
    -> Core
    -> Core
    -> Core
    -> WriterT Any (CompT m) Core
quantificationOverValueSet_In quantifier qnOverExpr vs qnVar qnGuard qnBody = do
    tell $ Any True
    let
        guardOp (Expr ":empty-guard" []) b = return b
        guardOp a b = case quantifier of
                        "forAll" -> return $ Expr ":operator-->"  [a, b]
                        "exists" -> return $ Expr ":operator-/\\" [a, b]
                        "sum"    -> return $ Expr ":operator-*"   [a, b]
                        _        -> lift $ err ErrInvariant
                                            $ singletonNested
                                            $ "unknown quantifier in simplify" <+> pretty quantifier
        glueOp a b = case quantifier of
                        "forAll" -> return $ Expr ":operator-/\\" [a, b]
                        "exists" -> return $ Expr ":operator-\\/" [a, b]
                        "sum"    -> return $ Expr ":operator-+"   [a, b]
                        _        -> lift $ err ErrInvariant
                                            $ singletonNested
                                            $ "unknown quantifier in simplify" <+> pretty quantifier
    let identity' = qnIdentity quantifier
    tyOverExpr <- lift $ typeOf qnOverExpr
    tyInner    <- lift $ innerTypeOf "in simplify" tyOverExpr
    case quantifier of
        "sum" -> do
            vs' <- sequence [ guardOp theGuard
                                      (replaceCore qnVar v qnBody)
                            | (v, rest) <- withRestToL vs
                            , let theGuard =
                                    if not $ null $ vs \\ [v]
                                        then Expr ":operator-/\\"
                                              [ replaceCore qnVar v qnGuard
                                              , Expr ":operator-toInt" [
                                                Expr ":operator-not" [
                                                Expr ":operator-in"
                                                    [ v
                                                    , Expr ":typed"
                                                        [ Expr ":value" [ Expr ":value-mset" rest ]
                                                        , Expr ":domain-in-expr" [
                                                          Expr ":type" [
                                                          Expr ":type-mset" [
                                                          Expr ":type-mset-inner" [
                                                          tyInner
                                                          ]]]]]
                                                    ]
                                                ]]
                                               ]
                                        else replaceCore qnVar v qnGuard
                            ]
            foldM glueOp identity' vs'
        _     -> do
            vs' <- sequence [ guardOp (replaceCore qnVar v qnGuard)
                                      (replaceCore qnVar v qnBody)
                            | v <- vs ]
            foldM glueOp identity' vs'

qnIdentity :: Reference -> Core
qnIdentity "forAll" = valueTrue
qnIdentity "exists" = valueFalse
qnIdentity "sum"    = valueInt 0
qnIdentity qn       = error $ show
                            $ "unknown quantifier" <+> pretty qn
