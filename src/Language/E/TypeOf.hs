{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.TypeOf where

import Stuff.Generic
import Stuff.NamedLog
import Stuff.FunkyT

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.TH
import {-# SOURCE #-} Language.E.Evaluator.ToInt
import Language.E.Lexer ( runLexer )
import Language.E.Parser ( runParser, inCompleteFile, parseExpr )
import Language.E.Pretty

import qualified Data.Text as T


typeUnify :: Monad m => E -> E -> CompE m Bool
typeUnify [xMatch| _ := type.unknown |] _ = return True
typeUnify _ [xMatch| _ := type.unknown |] = return True
typeUnify
    [xMatch| [a] := type.set.inner |]
    [xMatch| [b] := type.set.inner |]
    = typeUnify a b
typeUnify
    [xMatch| [a] := type.mset.inner |]
    [xMatch| [b] := type.mset.inner |]
    = typeUnify a b
typeUnify
    [xMatch| [aFr] := type.function.innerFrom
           | [aTo] := type.function.innerTo
           |]
    [xMatch| [bFr] := type.function.innerFrom
           | [bTo] := type.function.innerTo
           |]
    = (&&) <$> typeUnify aFr bFr <*> typeUnify aTo bTo
typeUnify x y = do
    mkLog "missing:typeUnify" $ pretty x <+> "~~" <+> pretty y
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
    mkLog "missing:mostKnown" $ pretty x <+> "~~" <+> pretty y
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
            printLogs $ getLogs globalSt
            forM_ results $ \ (result, _) ->
                case result of
                    Left  e -> error (show e)
                    Right y -> print $ pretty y


typeErrorIn :: Monad m => E -> CompE m a
typeErrorIn p = err ErrFatal $ "Type error in: " <+> prettyAsPaths p


typeOf :: (Functor m, Monad m) => E -> CompE m E

-- typeOf p | trace ("typeOf: " ++ (show $ pretty p)) False = undefined

typeOf (Prim (B {})) = return [xMake| type.bool := [] |]
typeOf (Prim (I {})) = return [xMake| type.int  := [] |]

typeOf p@[xMatch| _ := type |] = return p

typeOf [xMatch| [Prim (S i')] := reference |] = do
    let i = head $ splitOn "#" i'
    bs <- getsLocal binders
    if i == "_"
        then return [xMake| type.unknown := [] |]
        else case [ x | Binder nm x <- bs, nm == i ] of
                (x:_) -> typeOf x
                _   -> do
                    let bsText = prettyList id "," [ nm | Binder nm _ <- bs ]
                    err ErrFatal $ "(typeOf) Undefined reference:" <+> pretty i
                                 $$ nest 4 ("Current bindings:" <+> bsText)

typeOf [xMatch| [Prim (S i)] := metavar |] = do
    let j = '&' : i
    bs <- getsLocal binders
    case [ x | Binder nm x <- bs, nm == j ] of
        [x] -> typeOf x
        -- _   -> return p
        _   -> err ErrFatal $ "Undefined reference: " <+> pretty j

typeOf [xMatch| [i] := structural.single |] = typeOf i

typeOf [xMatch| [d] := topLevel.declaration.find .domain |] = typeOf d
typeOf [xMatch| [d] := topLevel.declaration.given.domain |] = typeOf d

typeOf [xMatch| [d] := typed.right |] = typeOf d

-- domain.*

typeOf [xMatch| [d] := domainInExpr |] = typeOf d

typeOf [xMatch| _ := domain.bool |] = return [xMake| type.bool := [] |]
typeOf [xMatch| _ := domain.int  |] = return [xMake| type.int  := [] |]

typeOf [xMatch| [index] := domain.matrix.index
              | [inner] := domain.matrix.inner
              |] = do
    tIndex <- typeOf index
    tInner <- typeOf inner
    return [xMake| type.matrix.index := [tIndex]
                 | type.matrix.inner := [tInner]
                 |]

typeOf [xMatch| ds := domain.tuple.inners |] = do
    ts <- mapM typeOf ds
    return [xMake| type.tuple.inners := ts |]

typeOf [xMatch| [fr] := domain.function.innerFrom
              | [to] := domain.function.innerTo
              |] = do
    frTy <- typeOf fr
    toTy <- typeOf to
    return [xMake| type.function.innerFrom := [frTy]
                 | type.function.innerTo   := [toTy]
                 |]

typeOf [xMatch| [i] := domain.set.inner |] = do
    ti <- typeOf i
    return [xMake| type.set.inner := [ti] |]

typeOf [xMatch| [i] := domain.mset.inner |] = do
    ti <- typeOf i
    return [xMake| type.mset.inner := [ti] |]

typeOf [xMatch| xs := domain.relation.inners |] = do
    txs <- mapM typeOf xs
    return [xMake| type.relation.inners := txs |]

-- value.*

typeOf [xMatch| [i] := value.literal |] = typeOf i

typeOf [xMatch| xs := value.tuple.values |] = do
    txs <- mapM typeOf xs
    return [xMake| type.tuple.inners := txs |]

typeOf   [xMatch| [] := value.set.values |] = return [xMake| type.unknown := [] |]
typeOf p@[xMatch| xs := value.set.values |] = do
    (tx:txs) <- mapM typeOf xs
    if all (tx==) txs
        then return [xMake| type.set.inner := [tx] |]
        else typeErrorIn p

typeOf   [xMatch| [] := value.mset.values |] = return [xMake| type.unknown := [] |]
typeOf p@[xMatch| xs := value.mset.values |] = do
    (tx:txs) <- mapM typeOf xs
    if all (tx==) txs
        then return [xMake| type.mset.inner := [tx] |]
        else typeErrorIn p


-- expressions

typeOf p@[eMatch| &a = &b |] = do
    tya <- typeOf a
    tyb <- typeOf b
    if tya == tyb
        then return [xMake| type.bool := [] |]
        else typeErrorIn p

typeOf p@[eMatch| ! &a |] = do
    tya <- typeOf a
    case tya of
        [xMatch| [] := type.bool |] -> return [xMake| type.bool := [] |]
        _ -> typeErrorIn p

typeOf p@[eMatch| toInt(&a) |] = do
    tya <- typeOf a
    case tya of
        [xMatch| [] := type.bool |] -> return [xMake| type.int := [] |]
        _ -> typeErrorIn p

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

typeOf [xMatch| [Prim (S "forAll")] := quantified.quantifier.reference |] =
    return [xMake| type.bool := [] |]

typeOf [xMatch| [Prim (S "exists")] := quantified.quantifier.reference |] =
    return [xMake| type.bool := [] |]

typeOf [xMatch| [Prim (S "sum")] := quantified.quantifier.reference |] =
    return [xMake| type.int := [] |]

typeOf p@[xMatch| [x] := operator.twoBars |] = do
    tx <- typeOf x
    case tx of
        [xMatch| [] := type.int |]                 -> return [xMake| type.int := [] |]
        [xMatch| [] := type. set.inner.type.int |] -> return [xMake| type.int := [] |]
        [xMatch| [] := type.mset.inner.type.int |] -> return [xMake| type.int := [] |]
        -- _ -> err ErrFatal $ "Type error in:" <+> prettyAsPaths tx
        _ -> typeErrorIn p

typeOf p@[xMatch| [m,i'] := operator.indices |] = do
    i <- case i' of
        [xMatch| [Prim (I i)] := value.literal |] -> return i
        _ -> typeErrorIn p
    tm <- typeOf m

    let
        getIndex 0 [xMatch| [indexTy] := type.matrix.index
                          |] = return indexTy
        getIndex n [xMatch| [innerTy] := type.matrix.inner
                          |] = getIndex (n-1) innerTy
        getIndex _ _ = typeErrorIn p

    getIndex i tm

typeOf p@[xMatch| [x] := operator.toSet |] = do
    tx <- typeOf x
    case tx of
        [xMatch| [innerTy] := type.mset.inner |] -> return [xMake| type.set.inner := [innerTy] |]
        [xMatch| innerTys  := type.relation.inners |] -> return [xMake| type.set.inner.type.tuple.inners := innerTys |]
        [xMatch| [innerFr] := type.function.innerFrom
               | [innerTo] := type.function.innerTo
               |] -> return [xMake| type.set.inner.type.tuple.inners := [innerFr,innerTo] |]
        -- _ -> err ErrFatal $ "Type error in:" <+> prettyAsPaths tx
        _ -> typeErrorIn p

typeOf p@[xMatch| [x] := operator.toMSet |] = do
    tx <- typeOf x
    case tx of
        [xMatch| [innerTy] := type.set.inner |] -> return [xMake| type.mset.inner := [innerTy] |]
        [xMatch| innerTys  := type.relation.inners |] -> return [xMake| type.mset.inner.type.tuple.inners := innerTys |]
        [xMatch| [innerFr] := type.function.innerFrom
               | [innerTo] := type.function.innerTo
               |] -> return [xMake| type.mset.inner.type.tuple.inners := [innerFr,innerTo] |]
        -- _ -> err ErrFatal $ "Type error in:" <+> prettyAsPaths tx
        _ -> typeErrorIn p

typeOf p@[eMatch| &a intersect &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta, tb) of
        ([xMatch| [ia] := type.set.inner |], [xMatch| [ib] := type.set.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else typeErrorIn p
        ([xMatch| [ia] := type.mset.inner |], [xMatch| [ib] := type.mset.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else typeErrorIn p
        _ -> typeErrorIn p

typeOf p@[eMatch| &a union &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta, tb) of
        ([xMatch| [ia] := type.set.inner |], [xMatch| [ib] := type.set.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else typeErrorIn p
        ([xMatch| [ia] := type.mset.inner |], [xMatch| [ib] := type.mset.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else typeErrorIn p
        _ -> typeErrorIn p

typeOf p@[eMatch| &a - &b |] = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta, tb) of
        ([xMatch| [] := type.int |], [xMatch| [] := type.int |]) -> return [xMake| type.int := [] |]
        ([xMatch| [ia] := type.set.inner |], [xMatch| [ib] := type.set.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else typeErrorIn p
        ([xMatch| [ia] := type.mset.inner |], [xMatch| [ib] := type.mset.inner |]) -> do
            res <- typeUnify ia ib
            if res
                then mostKnown ta tb
                else typeErrorIn p
        _ -> typeErrorIn p

typeOf p@[xMatch| [Prim (S operator)] := binOp.operator
                | [a] := binOp.left
                | [b] := binOp.right
                |] | operator `elem` words "+ - * / %" = do
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        ( [xMatch| [] := type.int |] , [xMatch| [] := type.int |] ) -> return [xMake| type.int := [] |]
        _ -> typeErrorIn p

typeOf p@[xMatch| [Prim (S operator)] := binOp.operator
                | [a] := binOp.left
                | [b] := binOp.right
                |] | operator `elem` words "/\\ \\/ => <=>" = do
    tya <- typeOf a
    tyb <- typeOf b
    case (tya, tyb) of
        ( [xMatch| [] := type.bool |] , [xMatch| [] := type.bool |] ) -> return [xMake| type.bool := [] |]
        _ -> typeErrorIn p

typeOf p@[eMatch| max(&a) |] = do
    ta <- typeOf a
    case ta of
        [xMatch| [] := type. set.inner.type.int |] -> return [xMake| type.int := [] |]
        [xMatch| [] := type.mset.inner.type.int |] -> return [xMake| type.int := [] |]
        _ -> typeErrorIn p

typeOf p@[eMatch| max(&a,&b) |] = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta,tb) of
        ( [xMatch| [] := type.int |] , [xMatch| [] := type.int |] ) -> return [xMake| type.int := [] |]
        _ -> typeErrorIn p

typeOf p@[eMatch| min(&a) |] = do
    ta <- typeOf a
    case ta of
        [xMatch| [] := type. set.inner.type.int |] -> return [xMake| type.int := [] |]
        [xMatch| [] := type.mset.inner.type.int |] -> return [xMake| type.int := [] |]
        _ -> typeErrorIn p

typeOf p@[eMatch| min(&a,&b) |] = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta,tb) of
        ( [xMatch| [] := type.int |] , [xMatch| [] := type.int |] ) -> return [xMake| type.int := [] |]
        _ -> typeErrorIn p

typeOf [xMatch| [i] := withLocals.actual
              | js  := withLocals.locals |] = mapM_ processStatement js >> typeOf i

typeOf p@[xMatch| [f] := functionApply.actual
                | [x] := functionApply.args
                |] = do
    tyF <- typeOf f
    tyX <- typeOf x
    case tyF of
        [xMatch| [fr] := type.function.innerFrom
               | [to] := type.function.innerTo
               |] | fr == tyX -> return to
        _ -> typeErrorIn p

typeOf p@[eMatch| preImage(&f,&x) |] = do
    tyF <- typeOf f
    tyX <- typeOf x
    case tyF of
        [xMatch| [fr] := type.function.innerFrom
               | [to] := type.function.innerTo
               |] | to == tyX -> return [xMake| type.set.inner := [fr] |]
        _ -> typeErrorIn p

typeOf p@[xMatch| [f] := operator.defined |] = do
    tyF <- typeOf f
    case tyF of
        [xMatch| [fr] := type.function.innerFrom
               |] -> return [xMake| type.set.inner := [fr] |]   
        _ -> typeErrorIn p

typeOf p@[xMatch| [f] := operator.range |] = do
    tyF <- typeOf f
    case tyF of
        [xMatch| [to] := type.function.innerTo
               |] -> return [xMake| type.set.inner := [to] |]
        _ -> typeErrorIn p

typeOf p@[xMatch| [m] := operator.index.left
                | [i] := operator.index.right
                |] = do
    tyM <- typeOf m
    tyI <- typeOf i
    ret <- case tyM of
        [xMatch| [ind] := type.matrix.index
               | [inn] := type.matrix.inner
               |] | ind == tyI -> return inn
        [xMatch| ts := type.tuple.inners |] -> do
            mint <- toInt i
            case mint of
                Just int | int >= 1 && int <= genericLength ts -> return $ ts `genericIndex` (int - 1)
                _ -> typeErrorIn p
        _ -> typeErrorIn p
    return ret

typeOf e = err ErrFatal $ "Cannot determine the type of:" <+> prettyAsPaths e
-- typeOf e = err ErrFatal $ "Cannot determine the type of:" <+> pretty e



innerTypeOf :: Monad m => Doc -> E -> CompE m E
innerTypeOf _ [xMatch| [ty] := type. set.inner |] = return ty
innerTypeOf _ [xMatch| [ty] := type.mset.inner |] = return ty
innerTypeOf doc p = err ErrFatal $ vcat [ "innerTypeOf", pretty p, doc ]

