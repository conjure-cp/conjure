{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.E.TypeOf where

import Stuff.Generic
import Stuff.NamedLog
import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.TH
import Language.E.Lexer ( runLexer )
import Language.E.Parser ( runParser, inCompleteFile, parseExpr )
import Language.E.Pretty

import qualified Data.Text as T


typeUnify :: Monad m => E -> E -> CompE m Bool
typeUnify [xMatch| _ := type.unknown |] _ = return True
typeUnify _ [xMatch| _ := type.unknown |] = return True
typeUnify [xMatch| [a] := type.set.inner |]
          [xMatch| [b] := type.set.inner |] = typeUnify a b
typeUnify [xMatch| [a] := type.mset.inner |]
          [xMatch| [b] := type.mset.inner |] = typeUnify a b
typeUnify x y = do
    mkLog "debug:typeUnify" $ pretty x <+> "~~" <+> pretty y
    return (x == y)

mostKnown :: Monad m => E -> E -> CompE m E
mostKnown [xMatch| _ := type.unknown |] x = return x
mostKnown x [xMatch| _ := type.unknown |] = return x
mostKnown [xMatch| [a] := type.set.inner |]
          [xMatch| [b] := type.set.inner |] = do
              x <- mostKnown a b
              return [xMake| type.set.inner := [x] |]
mostKnown [xMatch| [a] := type.mset.inner |]
          [xMatch| [b] := type.mset.inner |] = do
              x <- mostKnown a b
              return [xMake| type.mset.inner := [x] |]

mostKnown x y = do
    mkLog "debug:mostKnown" $ pretty x <+> "~~" <+> pretty y
    return x


test_TypeOf :: T.Text -> IO ()
test_TypeOf t = do
    let res = (runLexer >=> runParser (inCompleteFile parseExpr) "") t
    case res of
        Left  e -> print e
        Right x -> do
            print $ pretty x
            -- print $ prettyAsTree x
            let (results, globalSt) = runIdentity $ runCompE $ typeOf x
            print $ prettyLogs $ logs globalSt
            forM_ results $ \ (result, _) -> do
                case result of
                    Left  e -> error (show e)
                    Right y -> print $ pretty y


typeOf :: Monad m => E -> CompE m E

-- typeOf p | trace ("typeOf: " ++ (show $ pretty p)) False = undefined

typeOf (Prim (B {})) = return [xMake| type.bool := [] |]
typeOf (Prim (I {})) = return [xMake| type.int  := [] |]

typeOf p@[xMatch| _ := type |] = return p

typeOf [xMatch| [Prim (S i)] := reference |] = do
    bs <- getsLocal binders
    if i == "_"
        then return [xMake| type.unknown := [] |]
        else case [ x | Binder nm x <- bs, nm == i ] of
                (x:_) -> typeOf x
                _   -> do
                    let bsText = sep $ map (\ (Binder nm _) -> stringToDoc nm ) bs
                    err ErrFatal $ "Undefined reference: " <+> pretty i $$ bsText

typeOf [xMatch| [Prim (S i)] := metavar |] = do
    let j = '&' : i
    bs <- getsLocal binders
    case [ x | Binder nm x <- bs, nm == j ] of
        [x] -> typeOf x
        _   -> err ErrFatal $ "Undefined reference: " <+> pretty j

typeOf [xMatch| [d] := topLevel.declaration.find.domain |] = typeOf d

typeOf [xMatch| [d] := typed.right |] = typeOf d

-- domain.*

typeOf [xMatch| [d] := domainInExpr |] = typeOf d

typeOf [xMatch| _ := domain.int |] = return [xMake| type.int := [] |]

typeOf [xMatch| [i] := domain.set.inner |] = do
    ti <- typeOf i
    return [xMake| type.set.inner := [ti] |]

typeOf [xMatch| [i] := domain.mset.inner |] = do
    ti <- typeOf i
    return [xMake| type.mset.inner := [ti] |]

-- value.*

typeOf [xMatch| [i] := value.literal |] = typeOf i

typeOf   [xMatch| [] := value.set.values |] = return [xMake| type.unknown := [] |]
typeOf p@[xMatch| xs := value.set.values |] = do
    (tx:txs) <- mapM typeOf xs
    if all (tx==) txs
        then return [xMake| type.set.inner := [tx] |]
        else err ErrFatal $ "Type error in: " <+> pretty p

typeOf   [xMatch| [] := value.mset.values |] = return [xMake| type.unknown := [] |]
typeOf p@[xMatch| xs := value.mset.values |] = do
    (tx:txs) <- mapM typeOf xs
    if all (tx==) txs
        then return [xMake| type.mset.inner := [tx] |]
        else err ErrFatal $ "Type error in: " <+> pretty p


-- expressions

typeOf p@[eMatch| &a = &b |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    tya <- typeOf a
    tyb <- typeOf b
    if tya == tyb
        then return [xMake| type.bool := [] |]
        else err'

typeOf p@[eMatch| ! &a |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    tya <- typeOf a
    case tya of
        [xMatch| [] := type.bool |] -> return [xMake| type.bool := [] |]
        _ -> err'

typeOf p@[eMatch| toInt(&a) |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    tya <- typeOf a
    case tya of
        [xMatch| [] := type.bool |] -> return [xMake| type.int := [] |]
        _ -> err'

typeOf [xMatch| [i] := quanVar.within.quantified.quanOverDom |] = typeOf i

typeOf [xMatch| [i] := quanVar.within.quantified.quanOverExpr
              | [ ] := quanVar.within.quantified.quanOverOp.binOp.in
              |] = do
    ti <- typeOf i
    case ti of
        [xMatch| [j] := type.set.inner  |] -> return j
        [xMatch| [j] := type.mset.inner |] -> return j
        _ -> err ErrFatal $ "Cannot determine the inner type of: " <+> prettyAsPaths ti

typeOf [xMatch| [i] := quanVar.within.quantified.quanOverExpr
              | [ ] := quanVar.within.quantified.quanOverOp.binOp.subset
              |] = typeOf i

typeOf [xMatch| [i] := quanVar.within.quantified.quanOverExpr
              | [ ] := quanVar.within.quantified.quanOverOp.binOp.subsetEq
              |] = typeOf i

typeOf [xMatch| [Prim (S "forAll")] := quantified.quantifier.reference |] = do
    return [xMake| type.bool := [] |]

typeOf [xMatch| [Prim (S "exists")] := quantified.quantifier.reference |] = do
    return [xMake| type.bool := [] |]

typeOf [xMatch| [Prim (S "sum")] := quantified.quantifier.reference |] = do
    return [xMake| type.int := [] |]

typeOf p@[xMatch| [x] := operator.twoBars |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    tx <- typeOf x
    case tx of
        [xMatch| [] := type.int |]                 -> return [xMake| type.int := [] |]
        [xMatch| [] := type. set.inner.type.int |] -> return [xMake| type.int := [] |]
        [xMatch| [] := type.mset.inner.type.int |] -> return [xMake| type.int := [] |]
        -- _ -> err ErrFatal $ "Type error in:" <+> prettyAsPaths tx
        _ -> err'

typeOf p@[xMatch| [x] := operator.toSet |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    tx <- typeOf x
    case tx of
        [xMatch| [innerTy] := type.mset.inner |] -> return [xMake| type.set.inner := [innerTy] |]
        -- _ -> err ErrFatal $ "Type error in:" <+> prettyAsPaths tx
        _ -> err'

typeOf p@[eMatch| &a intersect &b |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    ta <- typeOf a
    tb <- typeOf b
    case (ta, tb) of
        ([xMatch| [ia] := type.set.inner |], [xMatch| [ib] := type.set.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else err'
        ([xMatch| [ia] := type.mset.inner |], [xMatch| [ib] := type.mset.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else err'
        _ -> err'

typeOf p@[eMatch| &a union &b |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    ta <- typeOf a
    tb <- typeOf b
    case (ta, tb) of
        ([xMatch| [ia] := type.set.inner |], [xMatch| [ib] := type.set.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else err'
        ([xMatch| [ia] := type.mset.inner |], [xMatch| [ib] := type.mset.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else err'
        _ -> err'

typeOf p@[eMatch| &a - &b |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    ta <- typeOf a
    tb <- typeOf b
    case (ta, tb) of
        ([xMatch| [] := type.int |], [xMatch| [] := type.int |]) -> return [xMake| type.int := [] |]
        ([xMatch| [ia] := type.set.inner |], [xMatch| [ib] := type.set.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else err'
        ([xMatch| [ia] := type.mset.inner |], [xMatch| [ib] := type.mset.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else err'
        _ -> err'

typeOf p@[xMatch| [Prim (S operator)] := binOp.operator
                | [a] := binOp.left
                | [b] := binOp.right
                |] | operator `elem` words "+ - * /" = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        ( [xMatch| [] := type.int |] , [xMatch| [] := type.int |] ) -> return [xMake| type.int := [] |]
        _ -> err'

typeOf p@[xMatch| [Prim (S operator)] := binOp.operator
                | [a] := binOp.left
                | [b] := binOp.right
                |] | operator `elem` words "/\\ \\/ => <=>" = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        ( [xMatch| [] := type.bool |] , [xMatch| [] := type.bool |] ) -> return [xMake| type.bool := [] |]
        _ -> err'

typeOf p@[eMatch| max(&a) |] = do
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    ta <- typeOf a
    case ta of
        [xMatch| [] := type.set.inner.type.int |] -> return [xMake| type.int := [] |]
        _ -> err'

typeOf p@[eMatch| max(&a,&b) |] = do
    mkLog "in (max/2)" $ pretty a $$ pretty b
    let err' = err ErrFatal $ "Type error in:" <+> pretty p
    ta <- typeOf a
    mkLog "ta" $ pretty ta
    tb <- typeOf b
    mkLog "ta" $ pretty tb
    case (ta,tb) of
        ( [xMatch| [] := type.int |] , [xMatch| [] := type.int |] ) -> return [xMake| type.int := [] |]
        _ -> err'

typeOf [xMatch| [i] := withLocals.actual
              | js  := withLocals.locals |] = mapM_ processStatement js >> typeOf i

-- typeOf e = err ErrFatal $ "Cannot determine the type of:" <+> prettyAsPaths e
typeOf e = err ErrFatal $ "Cannot determine the type of:" <+> pretty e





--     typeOf ( viewDeep [":metavar"] -> Just [R x] ) = typeOf ("@" `mappend` x)
-- 
--     typeOf ( viewDeep [":domain-in-expr"] -> Just [ d ] ) = typeOf d
--     typeOf ( viewDeep [":typed"]          -> Just [_,d] ) = typeOf d
-- 
--     typeOf ( viewDeep [":toplevel",":declaration",":find"]
--               -> Just [ Expr ":find-name" _
--                       , Expr ":find-domain" [domain]
--                       ]
--            ) = typeOf domain
-- 
--     typeOf p@( viewDeep [":type"] -> Just _) = return p
-- 
--     typeOf ( viewDeep [":empty-guard"] -> Just _ ) = return $ Expr ":type" [Expr ":type-bool" []]
-- 
--     typeOf p@(Expr ":range"  [d]) =
--         case d of
--             Expr ":range-open" []  -> errCannot p
--             Expr ":range-from" [x] -> do
--                 tx <- typeOf x
--                 return $
--                     Expr ":type"
--                         [ Expr ":type-range" [tx]
--                         ]
--             Expr ":range-to" [x] -> do
--                 tx <- typeOf x
--                 return $
--                     Expr ":type"
--                         [ Expr ":type-range" [tx]
--                         ]
--             Expr ":range-fromto" [x,y] -> do
--                 tx <- typeOf x
--                 ty <- typeOf y
--                 if tx == ty
--                     then return $
--                             Expr ":type"
--                                 [ Expr ":type-range" [tx]
--                                 ]
--                     else errMismatch "range" p
--             Expr ":range-single" [x] -> do
--                 tx <- typeOf x
--                 return $
--                     Expr ":type"
--                         [ Expr ":type-range" [tx]
--                         ]
--             _ -> errInvariant "range" p
--     typeOf p@(Expr ":domain" [d]) = do
--         x <- case d of
--             R r -> typeOf r
--             Expr ":domain-bool"   _  ->
--                 return $ Expr ":type-bool" []
--             Expr ":domain-int"    _  ->
--                 return $ Expr ":type-int"  []
--             Expr ":domain-enum"   xs ->
--                 case lookUpInExpr ":domain-enum-name" xs of
--                     Just [a] -> return $ Expr ":type-enum" [ Expr ":type-enum-name" [a] ]
--                     _ -> errInvariant "domain-enum" p
--             Expr ":domain-matrix" xs ->
--                 case ( lookUpInExpr ":domain-matrix-index" xs
--                      , lookUpInExpr ":domain-matrix-inner" xs
--                      ) of
--                          (Just [a], Just [b]) -> do
--                             ta <- typeOf a
--                             tb <- typeOf b
--                             return $
--                                 Expr ":type-matrix"
--                                     [ Expr ":type-matrix-index" [ta]
--                                     , Expr ":type-matrix-inner" [tb]
--                                     ]
--                          _ -> errInvariant "domain-matrix" p
--             Expr ":domain-tuple" [Expr ":domain-tuple-inners" xs] -> do
--                 txs <- mapM typeOf xs
--                 return $
--                     Expr ":type-tuple"
--                         [ Expr ":type-tuple-inners" txs
--                         ]
--             Expr ":domain-set" xs ->
--                 case lookUpInExpr ":domain-set-inner" xs of
--                     Just [a] -> do
--                         ta <- typeOf a
--                         return $
--                             Expr ":type-set"
--                                 [ Expr ":type-set-inner" [ta] ]
--                     _ -> errInvariant "domain-set" p
--             Expr ":domain-mset" xs ->
--                 case lookUpInExpr ":domain-mset-inner" xs of
--                     Just [a] -> do
--                         ta <- typeOf a
--                         return $
--                             Expr ":type-mset"
--                                 [ Expr ":type-mset-inner" [ta] ]
--                     _ -> errInvariant "domain-mset" p
--             Expr ":domain-function" xs ->
--                 case ( lookUpInExpr ":domain-function-innerfrom" xs
--                      , lookUpInExpr ":domain-function-innerto" xs
--                      ) of
--                          (Just [a], Just [b]) -> do
--                              ta <- typeOf a
--                              tb <- typeOf b
--                              return $
--                                 Expr ":type-function"
--                                     [ Expr ":type-function-innerfrom" [ta]
--                                     , Expr ":type-function-innerto" [tb]
--                                     ]
--                          _ -> errInvariant "domain-function" p
--             Expr ":domain-relation" xs ->
--                 case lookUpInExpr ":domain-relation-inners" xs of
--                     Just as -> do
--                         tas <- mapM typeOf as
--                         return $
--                             Expr ":type-relation"
--                                 [ Expr ":type-relation-inners" tas
--                                 ]
--                     _ -> errInvariant "domain-relation" p
--             Expr ":domain-partition" xs ->
--                 case lookUpInExpr ":domain-partition-inner" xs of
--                     Just [a] -> do
--                         ta <- typeOf a
--                         return $
--                             Expr ":type-partition"
--                                 [ Expr ":type-partition-inner" [ta]
--                                 ]
--                     _ -> errInvariant "domain-partition" p
--             _ -> errInvariant "domain" p
--         return $ Expr ":type" [x]
--     typeOf p@(Expr ":value"  [d]) =
--         case d of
--             Expr ":value-literal" [L x] -> typeOf x
--             Expr ":value-matrix" xs ->
--                 case ( lookUpInExpr ":value-matrix-indexrange"  xs
--                      , lookUpInExpr ":value-matrix-values" xs
--                      ) of
--                          (Just [a], Just ys) -> do
--                              ta  <- typeOf a
--                              tys <- mapM typeOf ys
--                              case tys of
--                                  [] -> errCannot p
--                                  (i:is) ->
--                                     if all (i==) is
--                                         then return $
--                                                 Expr ":type"
--                                                     [ Expr ":type-matrix"
--                                                         [ Expr ":type-matrix-indexrange" [ta]
--                                                         , Expr ":type-matrix-values"     [i]
--                                                         ]
--                                                     ]
--                                         else errMismatch "value" p
--                          (Nothing, Just ys) -> do
--                              tys <- mapM typeOf ys
--                              case tys of
--                                  [] -> errCannot p
--                                  (i:is) ->
--                                     if all (i==) is
--                                         then return $
--                                                 Expr ":type"
--                                                     [ Expr ":type-matrix"
--                                                         [Expr ":type-matrix-values" [i]]
--                                                     ]
--                                         else errMismatch "value" p
--                          _ -> errInvariant "value-matrix" p
--             Expr ":value-tuple" xs -> do
--                 txs <- mapM typeOf xs
--                 return $
--                     Expr ":type"
--                         [ Expr ":type-tuple" txs
--                         ]
--             Expr ":value-set" xs -> do
--                 txs <- mapM typeOf xs
--                 case txs of
--                     [] -> return $ Expr ":type" [
--                                    Expr ":type-set" [
--                                    Expr ":type-set-inner" [
--                                    Expr ":type" [
--                                    Expr ":type-unknown" [
--                                    ]]]]]
--                     (i:is) -> do
--                         flags <- mapM (typeUnify i) is
--                         if and flags
--                             then return $ Expr ":type" [
--                                           Expr ":type-set" [
--                                           Expr ":type-set-inner" [i]
--                                           ]]
--                             else errMismatch "value-set" p
--             Expr ":value-mset" xs -> do
--                 txs <- mapM typeOf xs
--                 case txs of
--                     [] -> return $ Expr ":type" [
--                                    Expr ":type-mset" [
--                                    Expr ":type-mset-inner" [
--                                    Expr ":type" [
--                                    Expr ":type-unknown" [
--                                    ]]]]]
--                     (i:is) -> do
--                         flags <- mapM (typeUnify i) is
--                         if and flags
--                             then return $ Expr ":type" [
--                                           Expr ":type-mset" [
--                                           Expr ":type-mset-inner" [i]
--                                           ]]
--                             else errMismatch "value-mset" p
--             Expr ":value-function" xs -> do
--                 let
--                     getOut (Expr ":value-function-mapping" [a,b]) = do
--                         ta <- typeOf a
--                         tb <- typeOf b
--                         return (ta,tb)
--                     getOut _ = errInvariant "value-function" p
--                 ys <- mapM getOut xs
--                 case ys of
--                     [] -> errCannot p
--                     (i@(a,b):is) ->
--                         if all (i==) is
--                             then return $ Expr ":type" [Expr ":type-function" [a,b]]
--                             else errMismatch "value-function" p
--             Expr ":value-relation" xs -> do
--                 txs <- mapM typeOf xs
--                 case txs of
--                     [] -> errCannot p
--                     (i:is) ->
--                         if all (i==) is
--                             then return $ Expr ":type" [Expr ":type-relation" [i]]
--                             else errMismatch "value-relation" p
--             Expr ":value-partition" xs -> do
--                 let
--                     getOut (Expr ":value-partition-part" as) = do
--                         tas <- mapM typeOf as
--                         case tas of
--                             [] -> errCannot p
--                             (i:is) ->
--                                 if all (i==) is
--                                     then return $ Expr ":type" [i]
--                                     else errMismatch "value-partition" p
--                     getOut _ = errInvariant "value-partition" p
--                 ys <- mapM getOut xs
--                 case ys of
--                     [] -> errCannot p
--                     (i:is) ->
--                         if all (i==) is
--                             then return $
--                                     Expr ":type"
--                                         [ Expr ":type-partition"
--                                             [ Expr ":type-partition-inner" [i]
--                                             ]
--                                         ]
--                             else errMismatch "value-partition" p
--             _ -> errInvariant "value" p
-- 
--     typeOf p@( Expr ":quanVar" [ Expr ":quanVar-name" _
--                                , Expr ":quanVar-within"
--                                     [ Expr ":expr-quantified" xs ]
--                                ]
--              ) = do
--         let
--             quanOverDom'  = lookUpInExpr ":expr-quantified-quanOverDom"   xs
--             quanOverOp'   = lookUpInExpr ":expr-quantified-quanOverOp"    xs
--             quanOverExpr' = lookUpInExpr ":expr-quantified-quanOverExpr"  xs
--         case (quanOverDom', quanOverOp', quanOverExpr') of
--             (Just [quanOverDom], Nothing, Nothing) ->
--                 typeOf quanOverDom
--             (Nothing, Just [Expr ":operator-in" []], Just [x]) -> do
--                 tx <- typeOf x
--                 bs <- gets binders
--                 -- mkLog ":typeOf-before-innerTypeOf" $ vcat [ pretty p
--                 --                                           , "--"
--                 --                                           , pretty x
--                 --                                           , "--"
--                 --                                           , pretty tx
--                 --                                           , stringToDoc $ show [ r | Binder (Reference r) _ <- bs ]
--                 --                                           ]
--                 r <- innerTypeOf (vcat ["here", pretty x, pretty tx]) tx
--                 -- mkLog ":typeOf-after-innerTypeOf" $ pretty r
--                 return r
--             (Nothing, Just [Expr ":operator-subset" []], Just [x]) -> do
--                 tx <- typeOf x
--                 return tx
--             (Nothing, Just [Expr ":operator-subsetEq" []], Just [x]) -> do
--                 tx <- typeOf x
--                 return tx
--             _ -> errInvariant "quanVar" p
-- 
--     typeOf p@( viewDeep [":expr-quantified"] -> Just xs )
--         | Just [ R quantifier           ] <- lookUpInExpr ":expr-quantified-quantifier"   xs
--         , Just [ Expr ":structural-single" [R r]
--                ]                          <- lookUpInExpr ":expr-quantified-quanVar"      xs
--         , Just [ qnGuard ]                <- lookUpInExpr ":expr-quantified-guard"        xs
--         , Just [ qnBody  ]                <- lookUpInExpr ":expr-quantified-body"         xs
--         = do
--             bindersBefore <- gets binders
--             let restoreState = modify $ \ st -> st { binders = bindersBefore }
--             addBinder r
--                     $ Expr ":quanVar" [ Expr ":quanVar-name"   [R r]
--                                       , Expr ":quanVar-within" [p]
--                                       ]
--             tyGuard    <- typeOf qnGuard
--             tyBody     <- typeOf qnBody
-- 
--             unless (tyGuard == Expr ":type" [Expr ":type-bool" []])
--                 $ errMismatch "guard must be a boolean expression" p
-- 
--             result <- case () of
--                 _ | quantifier `elem` ["forAll", "exists"] -> do
--                     unless (tyBody == Expr ":type" [Expr ":type-bool" []])
--                         $ errMismatch "body must be a boolean expression" p
--                     return $ Expr ":type" [Expr ":type-bool" []]
--                 _ | quantifier == "sum" -> do
--                     unless (tyBody == Expr ":type" [Expr ":type-int" []])
--                         $ errMismatch "body must be an integer expression" p
--                     return $ Expr ":type" [Expr ":type-int" []]
--                 _ -> errInvariant "typeOf" p
-- 
--             restoreState
--             -- mkLog "debug" $ vcat [ pretty p, pretty result ]
--             return result
-- 
--     typeOf _p@( viewDeep [":operator-toSet"]
--                  -> Just [a]
--               ) = do
--         ta <- typeOf a
--         let
--             checkAndReturn ( viewDeep [":type",":type-mset",":type-mset-inner"] -> Just [b] )
--                 = return $ Expr ":type"
--                          [ Expr ":type-set"
--                          [ Expr ":type-set-inner" [b]]]
--             checkAndReturn q = errInvariant "toSet" q
--         checkAndReturn ta
-- 
--     typeOf _p@( viewDeep [":operator-twobars"]
--                  -> Just [a]
--               ) = do
--         ta <- typeOf a
--         let
--             checkAndReturn  q@( viewDeep [":type",":type-int"] -> Just _ )
--                 = return q
--             checkAndReturn _q@( viewDeep [":type",":type-set"] -> Just _ )
--                 = return $ Expr ":type" [Expr ":type-int"  []]
--             checkAndReturn _q@( viewDeep [":type",":type-mset"] -> Just _ )
--                 = return $ Expr ":type" [Expr ":type-int"  []]
--             checkAndReturn  q = errInvariant "twobars" q
--         checkAndReturn ta
-- 
--     typeOf p@( viewDeep [":operator-toInt"]
--                 -> Just [a]
--              ) = do
--         ta <- typeOf a
--         flag <- typeUnify ta $ Expr ":type" [Expr ":type-bool" []]
--         if flag
--             then return $ Expr ":type" [Expr ":type-int" []]
--             else errMismatch "expecting a boolean expression inside toInt" p
-- 
--     typeOf p@( viewDeep [":operator-union"]
--                 -> Just [a,b]
--              ) = do
--         ta <- typeOf a
--         tb <- typeOf b
--         flag <- typeUnify ta tb
--         if flag
--             then return ta
--             else errMismatch "operator-union" p
-- 
--     typeOf p@( viewDeep [":operator-intersect"]
--                 -> Just [a,b]
--              ) = do
--         ta <- typeOf a
--         tb <- typeOf b
--         flag <- typeUnify ta tb
--         if flag
--             then return ta
--             else errMismatch "operator-intersect" p
-- 
--     typeOf p@( viewDeep [":operator-+"] -> Just [a,b] ) = intToIntToInt p a b
--     typeOf p@( viewDeep [":operator-*"] -> Just [a,b] ) = intToIntToInt p a b
--     typeOf p@( viewDeep [":operator-/"] -> Just [a,b] ) = intToIntToInt p a b
--     typeOf p@( viewDeep [":operator-%"] -> Just [a,b] ) = intToIntToInt p a b
-- 
--     -- Int -> Int -> Int
--     -- set of a -> set of a -> set of a
--     -- mset of a -> mset of a -> mset of a
--     typeOf p@( viewDeep [":operator--"]
--                 -> Just [a,b]
--              ) = do
--         ta <- typeOf a
--         tb <- typeOf b
--         flag <- typeUnify ta tb
--         if not flag
--             then errMismatch "operator (-)" p
--             else do
--                 let
--                     checkAndReturn q@( viewDeep [":type",":type-int" ] -> Just [ ]) = return q
--                     checkAndReturn q@( viewDeep [":type",":type-set" ] -> Just [_]) = return q
--                     checkAndReturn q@( viewDeep [":type",":type-mset"] -> Just [_]) = return q
--                     checkAndReturn q = errMismatch "checkAndReturn" q
--                 checkAndReturn ta
-- 
--     typeOf p = do
--         mkLog "typeOf" $ "catch all case" <++> pretty p
--         return $ Expr ":type" [Expr ":type-unknown" []]
-- 
-- instance TypeOf Literal where
--     typeOf (B {}) = return $ Expr ":type" [Expr ":type-bool" []]
--     typeOf (I {}) = return $ Expr ":type" [Expr ":type-int"  []]
-- 
-- instance TypeOf Reference where
--     typeOf "_" = return $ Expr ":type" [Expr ":type-unknown" []]
--     typeOf r = do
--         val <- lookUpRef r
--         typeOf val
-- 
-- intToIntToInt :: (Monad m, Functor m, Pretty a, TypeOf a) => a -> a -> a -> CompT m Core
-- intToIntToInt p a b = do
--     ta <- typeOf a
--     tb <- typeOf b
--     flag <- typeUnify ta tb
--     if not flag
--         then errMismatch "intToIntToInt" p
--         else do
--             let
--                 checkAndReturn q@( viewDeep [":type",":type-int" ] -> Just [ ]) = return q
--                 checkAndReturn _ = errMismatch "intToIntToInt.checkAndReturn" p
--             checkAndReturn ta


innerTypeOf :: Monad m => Doc -> E -> CompE m E
innerTypeOf _ [xMatch| [ty] := type. set.inner |] = return ty
innerTypeOf _ [xMatch| [ty] := type.mset.inner |] = return ty
innerTypeOf doc p = err ErrFatal $ vcat [ "innerTypeOf", pretty p, doc ]

-- typeUnify :: (Functor m, Monad m) => Core -> Core -> CompT m Bool
-- typeUnify (viewDeep [":type",":type-unknown"] -> Just []) _ = return True
-- typeUnify _ (viewDeep [":type",":type-unknown"] -> Just []) = return True
-- typeUnify (Expr t1 xs1) (Expr t2 xs2)
--     | t1 == t2
--     , length xs1 == length xs2
--     = and <$> zipWithM typeUnify xs1 xs2
-- typeUnify x y = do
--     mkLog "typeUnify" $ "default case" <++>
--                         vcat [ pretty x
--                              , "~~"
--                              , pretty y
--                              ]
--     return $ x == y
