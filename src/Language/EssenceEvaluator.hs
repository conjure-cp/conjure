{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Language.EssenceEvaluator ( runEvaluateExpr, quanDomain ) where

import Control.Applicative ( Applicative )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader ( MonadReader, ask )
import Control.Monad.RWS ( RWST, evalRWST )
import Control.Monad.Writer ( MonadWriter, tell )
import Data.Generics.Uniplate.Direct ( transform, transformBi, rewriteBiM )
import Data.List ( genericLength, genericIndex, isSuffixOf, sort, intersect, union )
import Data.List.Split( splitOn )
import Data.Maybe ( fromMaybe )

import Language.Essence
import Language.EssencePrinters ( prExpr )
import Language.EssenceTypes ( runTypeOf )
import PrintUtils ( render )


quanDomain :: Expr -> Expr
quanDomain x@(DomainIntegerFromTo {}) = x
quanDomain x@(DomainIntegerList   {}) = x
quanDomain   (DomainSet    {element}) = element
quanDomain   (DomainMSet   {element}) = element
quanDomain _ = Identifier "<undefined>"


runEvaluateExpr :: (Applicative m, MonadIO m) => [Binding] -> Expr -> m (Expr,[Log])
runEvaluateExpr topLevels x = do
    (y,logs) <- liftIO $ evalRWST (comp x) topLevels ()
    -- liftIO $ mapM_ (\ l -> putStrLn ("evaluator: " ++ l)) logs
    -- liftIO $ mapM_ putStrLn logs
    return (y,logs)
    where
        comp :: Expr -> RWST [Binding] [Log] () IO Expr
        comp = rewriteBiM combined

        withLog ::
            ( MonadReader [Binding] m
            , MonadWriter [Log] m
            ) => String
              -> (Expr -> m (Maybe Expr))
              -> Expr
              -> m (Maybe Expr)
        withLog msg f i = do
            mr <- f i
            case mr of
                Nothing -> return Nothing
                Just r  -> do
                    tell [msg ++ ": " ++ render prExpr i ++ " ~~> " ++ render prExpr r]
                    return mr

        combined ::
            ( Applicative m
            , MonadReader [Binding] m
            , MonadWriter [Log] m
            , MonadIO m
            ) => Expr -> m (Maybe Expr)
        combined i = do
            j <- withLog "Evaluator " evaluateExpr i
            case j of
                Nothing -> withLog "Normaliser" normaliseExpr i
                Just _  -> return j


conjunct :: [Expr] -> Expr
conjunct [] = ValueBoolean True
conjunct [x] = x
conjunct (x:xs) = GenericNode And [x,conjunct xs]

tauOfDomain :: Expr -> Maybe Expr
tauOfDomain (DomainSet {element=e}) = Just e
tauOfDomain _ = Nothing


evaluateExpr ::
    ( Applicative m
    , MonadIO m
    , MonadReader [Binding] m
    ) => Expr -> m (Maybe Expr)

-- set ops for int ranges

evaluateExpr p@(ExprQuantifier {quanOver=GenericNode Intersect [a@DomainIntegerFromTo{}, b@DomainIntegerFromTo{}]})
    | unifyExpr a b = rJust p {quanOver = a}
evaluateExpr (ExprQuantifier {quanName=qnName, quanOver=ValueInteger 0})
    = rJust $ GenericNode Image [Identifier "quanID", qnName]

evaluateExpr (GenericNode SubsetEq [ValueSet xs,y]) = rJust $ conjunct [ GenericNode Elem [x,y] | x <- xs ]

-- full evaluators

evaluateExpr (GenericNode Plus   [ValueInteger i,ValueInteger j])          = rJust $ ValueInteger $ i + j
evaluateExpr (GenericNode Minus  [ValueInteger i,ValueInteger j])          = rJust $ ValueInteger $ i - j
evaluateExpr (GenericNode Times  [ValueInteger i,ValueInteger j])          = rJust $ ValueInteger $ i * j
evaluateExpr (GenericNode Div    [ValueInteger i,ValueInteger j]) | j /= 0 = rJust $ ValueInteger $ div i j
evaluateExpr (GenericNode Mod    [ValueInteger i,ValueInteger j]) | j >  0 = rJust $ ValueInteger $ mod i j
evaluateExpr (GenericNode Pow    [ValueInteger i,ValueInteger j]) | j >  0 = rJust $ ValueInteger $ i ^ j

evaluateExpr (GenericNode Abs    [ValueInteger i]) = rJust $ ValueInteger $ abs i
evaluateExpr (GenericNode Negate [ValueInteger i]) = rJust $ ValueInteger $ negate i

-- evaluateExpr (GenericNode Factorial [ValueInteger i]) = rJust $ ValueInteger $ product [1..i]

evaluateExpr (GenericNode Lt  [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i <  j
evaluateExpr (GenericNode Leq [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i <= j
evaluateExpr (GenericNode Gt  [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i >  j
evaluateExpr (GenericNode Geq [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i >= j
evaluateExpr (GenericNode Neq [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i /= j
evaluateExpr (GenericNode Eq  [ValueInteger i, ValueInteger j]) = rJust $ ValueBoolean $ i == j

evaluateExpr (GenericNode Not    [ValueBoolean b]) = rJust $ ValueBoolean $ not b

evaluateExpr (GenericNode Elem [i,ValueSet  is]) | i `elem` is = rJust $ ValueBoolean True
evaluateExpr (GenericNode Elem [i,ValueMSet is]) | i `elem` is = rJust $ ValueBoolean True

evaluateExpr (GenericNode Intersect [ValueSet is,ValueSet js]) = rJust $ ValueSet $ sort $ is `intersect` js
evaluateExpr (GenericNode Union     [ValueSet is,ValueSet js]) = rJust $ ValueSet $ sort $ is `union`     js

-- partial evaluators

evaluateExpr (GenericNode Plus   [ValueInteger 0,x]) = rJust x
evaluateExpr (GenericNode Plus   [x,ValueInteger 0]) = rJust x
evaluateExpr (GenericNode Minus  [ValueInteger 0,x]) = rJust $ GenericNode Negate [x]
evaluateExpr (GenericNode Times  [ValueInteger 0,_]) = rJust $ ValueInteger 0
evaluateExpr (GenericNode Times  [_,ValueInteger 0]) = rJust $ ValueInteger 0
evaluateExpr (GenericNode Times  [ValueInteger 1,x]) = rJust x
evaluateExpr (GenericNode Times  [x,ValueInteger 1]) = rJust x
evaluateExpr (GenericNode Div    [x,ValueInteger 1]) = rJust x
evaluateExpr (GenericNode Mod    [x,y])     | x == y = rJust $ ValueInteger 0
evaluateExpr (GenericNode Pow    [_,ValueInteger 0]) = rJust $ ValueInteger 1
evaluateExpr (GenericNode Pow    [x,ValueInteger 1]) = rJust x

evaluateExpr (GenericNode Plus [x,y]) | unifyExpr x y
    = rJust $ GenericNode Times [ValueInteger 2,x]
evaluateExpr (GenericNode Plus [GenericNode Times [x,y],z]) | unifyExpr x z
    = rJust $ GenericNode Times [x,GenericNode Plus [y,ValueInteger 1]]
evaluateExpr (GenericNode Plus [GenericNode Times [y,x],z]) | unifyExpr x z
    = rJust $ GenericNode Times [x,GenericNode Plus [y,ValueInteger 1]]
evaluateExpr (GenericNode Plus [GenericNode Times [a,b],GenericNode Times [c,d]]) | unifyExpr b d
    = rJust $ GenericNode Times [GenericNode Plus [a,c],b]
evaluateExpr (GenericNode Plus [GenericNode Times [a,b],GenericNode Times [d,c]]) | unifyExpr b d
    = rJust $ GenericNode Times [GenericNode Plus [a,c],b]

evaluateExpr (GenericNode Times [x,y]) | unifyExpr x y
    = rJust $ GenericNode Pow [x,ValueInteger 2]
evaluateExpr (GenericNode Times [GenericNode Pow [x,y],z]) | x == z
    = rJust $ GenericNode Pow [x,GenericNode Plus [y,ValueInteger 1]]

evaluateExpr (GenericNode And [ValueBoolean True ,x]) = rJust x
evaluateExpr (GenericNode And [x, ValueBoolean True]) = rJust x
evaluateExpr (GenericNode And [ValueBoolean False,_]) = rJust $ ValueBoolean False
evaluateExpr (GenericNode And [_,ValueBoolean False]) = rJust $ ValueBoolean False

evaluateExpr (GenericNode Or  [ValueBoolean False,x]) = rJust x
evaluateExpr (GenericNode Or  [x,ValueBoolean False]) = rJust x
evaluateExpr (GenericNode Or  [ValueBoolean True ,_]) = rJust $ ValueBoolean True
evaluateExpr (GenericNode Or  [_,ValueBoolean True ]) = rJust $ ValueBoolean True

evaluateExpr (GenericNode Imply [ValueBoolean True ,x]) = rJust x
evaluateExpr (GenericNode Imply [ValueBoolean False,_]) = rJust $ ValueBoolean True
evaluateExpr (GenericNode Imply [_ ,ValueBoolean True]) = rJust $ ValueBoolean True
evaluateExpr (GenericNode Imply [a,b]) | unifyExpr (GenericNode Not [a]) b = rJust b -- is this too clever?

evaluateExpr (GenericNode Iff [ValueBoolean True ,x]) = rJust x
evaluateExpr (GenericNode Iff [ValueBoolean False,x]) = rJust $ GenericNode Not [x]

-- symbolic full evaluators

evaluateExpr (GenericNode Minus [a,b]) | unifyExpr a b = rJust $ ValueInteger 0
evaluateExpr (GenericNode Negate [GenericNode Negate [x]]) = rJust x
evaluateExpr (GenericNode Eq [a,b]) | unifyExpr a b = rJust $ ValueBoolean True

evaluateExpr (GenericNode Imply [a,b]) | unifyExpr a b = rJust $ ValueBoolean True
evaluateExpr (GenericNode Iff [a,b]) | unifyExpr a b = rJust $ ValueBoolean True

-- unroll set of int to int range

evaluateExpr p@(ExprQuantifier {quanOver=ValueSet xs@(x:_)}) = do
    bs <- ask
    let typeofX = runTypeOf bs x
    case typeofX of
        (Right TypeInteger, _) -> rJust p { quanOver = DomainIntegerList xs }
        _ -> rNothing

-- ValueTuple Index

evaluateExpr (GenericNode Index [ValueTuple xs,ValueInteger i])
    = rJust $ genericIndex xs i

-- some special cases

evaluateExpr (GenericNode Minus [GenericNode Plus [x,y],z])
    | y == z = rJust x
    | x == z = rJust y
evaluateExpr (GenericNode Plus [GenericNode Minus [x,y],z])
    | y == z = rJust x
    | x == z = rJust $ GenericNode Negate [y]

evaluateExpr (GenericNode Image [Identifier "domSize",domain]) = case domain of
    ValueInteger {} -> rJust $ ValueInteger 1
    DomainIntegerList xs -> rJust $ ValueInteger $ genericLength xs
    DomainIntegerFromTo (Just a) (Just b) -> rJust $ GenericNode Plus [ GenericNode Minus [b,a]
                                                                      , ValueInteger 1
                                                                      ]
    DomainTuple [] _ -> rJust $ ValueInteger 0
    DomainTuple xs _ -> do
        let xs' = [ GenericNode Image [Identifier "domSize", d] | d <- xs ]
        xs'' <- mapM evaluateExpr xs'
        rJust $ foldr1 (\ a b -> GenericNode Times [a,b] )
              $ zipWith fromMaybe xs' xs''
    DomainSet {size=Just n,element=e} -> do
        e' <- evaluateExpr $ GenericNode Image [Identifier "domSize", e]
        case e' of
            Nothing -> rNothing
            Just t  -> rJust $ GenericNode Pow [t,n]
    DomainSet {maxSize=Just n,element=e} -> do
        e' <- evaluateExpr $ GenericNode Image [Identifier "domSize", e]
        case e' of
            Nothing -> rNothing
            Just t  -> rJust $ GenericNode Pow [t,n]
    DomainSet {element=e} -> do
        e' <- evaluateExpr $ GenericNode Image [Identifier "domSize", e]
        case e' of
            Nothing -> rNothing
            Just t  -> rJust $ GenericNode Factorial [t]
    DomainMSet {size=Just n,element=e} -> do
        e' <- evaluateExpr $ GenericNode Image [Identifier "domSize", e]
        case e' of
            Nothing -> rNothing
            Just t  -> rJust $ GenericNode Pow [t,n]
    _ -> rNothing

evaluateExpr (GenericNode Image [Identifier "repr", ValueTuple [ theVal, Identifier reprName]]) = do
    let
        collectIndices :: Expr -> (Expr,[Expr])
        collectIndices (GenericNode Index [m,i]) = let (m',is) = collectIndices m in (m', i:is)
        collectIndices  x = (x,[])
    case collectIndices theVal of
        (Identifier nm, _) -> case splitOn "#" nm of
                                [_,c] | c `isSuffixOf` reprName -> rJust $ ValueBoolean True
                                _                               -> rNothing
        _ -> rNothing

-- evaluateExpr (GenericNode Image [Identifier "repr", ValueTuple [ GenericNode Index [Identifier a', _]
--                                                                , Identifier b
--                                                                ]
--                                 ])
--     = do
--         let a = head $ splitOn "#" a'
--         bs <- ask
--         case [ () | (ty,nm,DomainMatrix{}) <- bs, nm == a, ty `elem` [Find,Given] ] of
--             [] -> rNothing
--             _  -> evaluateExpr $ GenericNode Image [Identifier "repr", ValueTuple [ Identifier a'
--                                                                                   , Identifier b
--                                                                                   ]
--                                                    ]
-- 
-- evaluateExpr (GenericNode Image [Identifier "repr", ValueTuple [ Identifier a
--                                                                , Identifier b
--                                                                ]
--                                 ])
--     = case splitOn "#" a of
--         [_,c] | c `isSuffixOf` b -> rJust $ ValueBoolean True
--         _                        -> rNothing

evaluateExpr (GenericNode Image [Identifier "refn", theVal]) = do
    let
        collectIndices :: Expr -> (Expr,[Expr])
        collectIndices (GenericNode Index [m,i]) = let (m',is) = collectIndices m in (m', i:is)
        collectIndices  x = (x,[])

        fromIndices :: Expr -> [Expr] -> Expr
        fromIndices m []     = m
        fromIndices m [i]    = GenericNode Index [m,i]
        fromIndices m (i:is) = fromIndices (GenericNode Index [m,i]) is

    case collectIndices theVal of
        (Identifier nm, inds) -> case splitOn "#" nm of
            [b,c] -> rJust $ fromIndices (Identifier (b ++ "_" ++ c)) inds
            _     -> rNothing
        _ -> rNothing

-- evaluateExpr (GenericNode Image [Identifier "refn", GenericNode Index [ Identifier a
--                                                                       , ind
--                                                                       ]
--                                 ])
--     = case splitOn "#" a of
--         [b,c] -> rJust $ GenericNode Index [Identifier (b ++ "_" ++ c), ind]
--         _     -> rNothing
-- 
-- evaluateExpr (GenericNode Image [Identifier "refn", Identifier a])
--     = case splitOn "#" a of
--         [b,c] -> rJust $ Identifier $ b ++ "_" ++ c
--         _     -> rNothing

evaluateExpr (GenericNode Replace [x,old,new]) =
    let
        f i | i == old  = new
            | otherwise = i
    in rJust $ transformBi f x

evaluateExpr (GenericNode Image [Identifier "tau", theVal]) = do
    let
        collectIndices :: Expr -> (Expr,[Expr])
        collectIndices (GenericNode Index [m,i]) = let (m',is) = collectIndices m in (m', i:is)
        collectIndices  x = (x,[])

        indexDom :: Int -> Expr -> Maybe Expr
        indexDom 0 x = Just x
        indexDom i (DomainMatrix {element}) = indexDom (i-1) element
        indexDom _ _ = Nothing

    case collectIndices theVal of
        (Identifier nmParam, is) -> do
            let nm = case splitOn "#" nmParam of [x,_] -> x; _ -> nmParam
            bs <- ask
            case [ dom | (ty,nm',dom) <- bs, nm == nm', ty `elem` [Find,Given] ] of
                [dom] -> case indexDom (length is) dom of
                    Nothing -> rNothing
                    Just t  -> return (tauOfDomain t)
                _   -> rNothing
        _ -> rNothing


-- evaluateExpr (GenericNode Image [Identifier "tau", GenericNode Index [Identifier nmParam, _]]) = do
--     let nm = case splitOn "#" nmParam of [x,_] -> x; _ -> nmParam
--     bs <- ask
--     case [ e | (ty,nm',DomainMatrix{element=e}) <- bs, nm == nm', ty `elem` [Find,Given] ] of
--         [x] -> return $ tauOfDomain x
--         _   -> rNothing
-- 
-- 
-- evaluateExpr (GenericNode Image [Identifier "tau", Identifier nmParam]) = do
--     let nm = case splitOn "#" nmParam of [x,_] -> x; _ -> nmParam
--     bs <- ask
--     case [ dom | (ty,nm',dom) <- bs, nm == nm', ty `elem` [Find,Given] ] of
--         [x] -> return $ tauOfDomain x
--         _   -> rNothing



evaluateExpr (GenericNode Image [Identifier "indices", ValueTuple [Identifier nm, i]]) = do
    -- liftIO $ putStrLn "indices 1."
    bs <- ask
    case [ dom | (_,nm',dom@DomainMatrix{}) <- bs, nm == nm' ] of
        [dom] -> do
                    -- liftIO . ppPrint $ dom
                    evaluateExpr $ GenericNode Image [Identifier "indices", ValueTuple [dom, i]]
        _     -> rNothing

evaluateExpr (GenericNode Image [Identifier "indices", ValueTuple [DomainMatrix{index},ValueInteger 0]])
    = do
        -- liftIO $ putStrLn "indices 2."
        rJust index

evaluateExpr (GenericNode Image [Identifier "indices", ValueTuple [DomainMatrix{element},ValueInteger i]])
    = do
        -- liftIO $ putStrLn "indices 3."
        evaluateExpr $ GenericNode Image [Identifier "indices", ValueTuple [element,ValueInteger (i-1)]]

evaluateExpr (GenericNode Image [Identifier "indices", ValueTuple [GenericNode Index [m,_], ValueInteger i]])
    = evaluateExpr $ GenericNode Image [Identifier "indices", ValueTuple [m, ValueInteger (i+1)]]


evaluateExpr (GenericNode Image [Identifier opParam, Identifier nm]) = do
    let op = reverse $ take 6 $ reverse opParam
    if op `elem` ["glueOp", "skipOp", "quanID"]
        then do
            bs <- ask
            case [ (glueOp,skipOp,quanID)
                 | (Letting,nm',DeclQuantifier glueOp skipOp quanID) <- bs
                 , nm == nm' ] of
                     [(glueOp,skipOp,quanID)] -> case op of "glueOp" -> rJust glueOp
                                                            "skipOp" -> rJust skipOp
                                                            "quanID" -> rJust quanID
                                                            _        -> error "this should never happen."
                     _ -> do
                         let msg = "quantifier " ++ nm ++ " is not defined."
                         liftIO $ putStrLn msg
                         void $ error msg
                         rNothing
        else rNothing


evaluateExpr (GenericNode Image [DeclLambda params body, ValueTuple arguments]) =
    if length params /= length arguments
        then rNothing
        else do
            let
                lu = zip (map fst params) arguments
                f (Identifier nm) = fromMaybe (Identifier nm) (lookup nm lu)
                f x = x
            rJust $ transform f body

evaluateExpr (GenericNode Image [DeclLambda [param] body, x])
    = evaluateExpr $ GenericNode Image [DeclLambda [param] body, ValueTuple [x]]

-- let binding

evaluateExpr (Identifier nm) = do
    bs <- ask
    case [ x | (ty,nm',x) <- bs, nm == nm', ty `elem` [Letting,InRule] ] of
        [DeclQuantifier {}] -> rNothing
        [x] -> rJust x
        _   -> rNothing

evaluateExpr (GenericNode HasType [i,j]) = do
    bs <- ask
    let typeofI = runTypeOf bs i
    let typeofJ = runTypeOf bs j
    -- liftIO $ ppPrint (typeofI, typeofJ)
    case (typeofI, typeofJ) of
        ((Right it, _), (Right jt, _)) -> rJust $ ValueBoolean $ typeUnify it jt
        _ -> rNothing

evaluateExpr p@(ExprQuantifier {quanGuard=Just (ValueBoolean True)}) = rJust p {quanGuard = Nothing}

-- defined(f)

evaluateExpr (GenericNode Defined [Identifier nmP]) = do
    bs <- ask
    let nm = head $ splitOn "#" nmP
    case [ x | (ty,nm',x) <- bs, nm == nm', ty `elem` [Find,Given] ] of
        [DomainFunction {isTotal=True,functionFrom}] -> rJust functionFrom
        _   -> rNothing

-- no eval

evaluateExpr _ = rNothing


normaliseExpr :: Monad m => Expr -> m (Maybe Expr)
normaliseExpr (GenericNode op [GenericNode op2 [a,b],c])
    | op == op2
    , op `elem` associativeOps
    = let [x,y,z] = sort [a,b,c]
      in  if [x,y,z] == [a,b,c]
              then rNothing
              else rJust $ GenericNode op [GenericNode op [x,y],z]
normaliseExpr (GenericNode op [a,b])
    | op `elem` commutativeOps
    , b < a
    = rJust $ GenericNode op [b,a]
normaliseExpr _ = rNothing


-- return Nothing in a given monad
rNothing :: Monad m => m (Maybe a)
rNothing = return Nothing

-- return Just value in a given monad
rJust :: Monad m => a -> m (Maybe a)
rJust = return . Just


-- do these two expressions unify to the same thing?
unifyExpr :: Expr -> Expr -> Bool
unifyExpr (GenericNode Not [GenericNode Not [x]]) y
    = unifyExpr x y
unifyExpr (GenericNode Not [GenericNode Eq [a,b]]) (GenericNode Neq [c,d])
    = unifyExpr a c && unifyExpr b d
unifyExpr x y = x == y
