{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Language.EssenceEvaluator where

import Control.Applicative ( Applicative, (<$>), (<*) )
import Control.Monad ( (<=<), forM )
import Control.Monad.Error ( MonadError(catchError, throwError), ErrorT, runErrorT )
import Control.Monad.State ( MonadState(get), StateT, evalStateT )
import Control.Monad.Writer ( MonadWriter(tell), WriterT, runWriterT )
import Data.List ( delete, genericIndex, genericLength, nub, sort )
import Data.String ( IsString(fromString) )
import qualified Data.Map as M ( Map, fromList, lookup )
import qualified Data.Set as S ( member )

import Language.Essence
import ParsePrint
import PrintUtils
import ParsecUtils ( parseIO, eof, unsafeParse )
import GenericOps.Core



testFullEval :: String -> IO ()
testFullEval s = do
    x <- parseIO (parse <* eof) s
    -- print x
    putStrLn $ renderDoc $ pretty (x :: Expr)
    let m = M.fromList [ ( "x", mkG ( unsafeParse (parse <* eof) "2"   :: Expr ) )
                       , ( "y", mkG ( unsafeParse (parse <* eof) "3"   :: Expr ) )
                       , ( "z", mkG ( unsafeParse (parse <* eof) "x+y" :: Expr ) )
                       , ( "m", mkG ( unsafeParse (parse <* eof) "[1,2,3,4,5]" :: Expr ) )
                       ] 
    (x',logs) <- runWriterT $ flip evalStateT m $ runErrorT $ (evaluate <=< deepSimplify) x
    case x' of
        Left err  -> print err
        Right x'' -> putStrLn $ renderDoc $ pretty (x'' :: Value)
    mapM_ (putStrLn . renderDoc) logs

testEval :: String -> IO ()
testEval s = do
    x <- parseIO (parse <* eof) s
    -- print x
    putStrLn $ renderDoc $ pretty (x :: Expr)
    let m = M.fromList [ ( "x", mkG ( unsafeParse (parse <* eof) "2"   :: Expr ) )
                       , ( "y", mkG ( unsafeParse (parse <* eof) "3"   :: Expr ) )
                       , ( "z", mkG ( unsafeParse (parse <* eof) "x+y" :: Expr ) )
                       , ( "m", mkG ( unsafeParse (parse <* eof) "[1,2,3,4,5]" :: Expr ) )
                       ] 
    (x',logs) <- runWriterT $ flip evalStateT m $ runErrorT $ evaluate x
    case x' of
        Left err  -> print err
        Right x'' -> putStrLn $ renderDoc $ pretty (x'' :: Value)
    mapM_ (putStrLn . renderDoc) logs

testSimplify :: String -> IO ()
testSimplify s = do
    x <- parseIO (parse <* eof) s
    -- print x
    putStrLn $ renderDoc $ pretty (x :: Expr)
    let m = M.fromList [ ( "x", mkG ( unsafeParse (parse <* eof) "2"   :: Expr ) )
                       , ( "y", mkG ( unsafeParse (parse <* eof) "3"   :: Expr ) )
                       , ( "z", mkG ( unsafeParse (parse <* eof) "x+y" :: Expr ) )
                       , ( "m", mkG ( unsafeParse (parse <* eof) "[1,2,3,4,5]" :: Expr ) )
                       ] 
    (x',logs) <- runWriterT $ flip evalStateT m $ runErrorT $ deepSimplify x
    case x' of
        Left err  -> print err
        Right x'' -> putStrLn $ renderDoc $ pretty (x'' :: Expr)
    mapM_ (putStrLn . renderDoc) logs



type LookupMap = M.Map String GNode
type EvalArrow m a b = a -> ErrorT Doc (StateT LookupMap (WriterT [Doc] m)) b

tryEvalArrow :: (Applicative m, MonadError e m) => (a -> m b) -> a -> m (Either e b)
tryEvalArrow f x = (Right <$> f x) `catchError` (return . Left)

tryEvalArrowMaybe :: (Applicative m, MonadError e m) => (a -> m b) -> a -> m (Maybe b)
tryEvalArrowMaybe f x = (Just <$> f x) `catchError` (\ _ -> return Nothing )

evalArrowErrorDef :: (Monad m, ParsePrint a) => EvalArrow m a b
evalArrowErrorDef = evalArrowError "Cannot evaluate"

evalArrowError :: (Monad m, ParsePrint a) => Doc -> EvalArrow m a b
evalArrowError msg x = throwError $ msg <> colon <+> pretty x



--------------------------------------------------------------------------------
-- partial evaluator -----------------------------------------------------------
--------------------------------------------------------------------------------

class Simplify a where
    simplify :: (Applicative m, Monad m) => EvalArrow m a (Maybe a)

infixr 0 ~~~>
(~~~>) :: (MonadWriter [Doc] m, ParsePrint a) => a -> a -> m (Maybe a)
a ~~~> b = do
    tell [ "[ SIMPLIFY ]"
            $$ nest 4 ("~~>" <+> pretty a)
            $$ nest 4 ("~~>" <+> pretty b)
         ]
    return (Just b)

deepSimplify :: (Applicative m, Monad m) => EvalArrow m Expr Expr
deepSimplify = unliftGM $ bottomUpRewriteM f
    where
        f :: (Applicative m, Monad m) => EvalArrow m GNode (Maybe GNode)
        f g = case fromG g of
            Nothing -> return Nothing -- type not Expr
            Just x  -> do
                y <- simplifyReal x
                case y of
                    Nothing -> return Nothing -- no simplification
                    Just z  -> return $ Just $ mkG z

simplifyReal :: (Applicative m, Monad m) => EvalArrow m Expr (Maybe Expr)
simplifyReal p = do
    mres <- simplify p
    case mres of
        Nothing -> case p of
            EOp op [a,b] | S.member op commutativeOps -> do -- simplify $ EOp op [b,a]
                mres2 <- simplify (EOp op [b,a])
                case mres2 of
                    Nothing   -> return Nothing
                    Just res2 -> p ~~~> res2
            _ -> return Nothing
        Just res -> return $ Just res

instance Simplify Expr where

    simplify p@(EOp Plus  [x, V (VInt 0)]) = p ~~~> x

    simplify p@(EOp Minus [x, V (VInt 0)]) = p ~~~> x

    simplify p@(EOp Times [_, V (VInt 0)]) = p ~~~> V $ VInt 0
    simplify p@(EOp Times [x, V (VInt 1)]) = p ~~~> x

    simplify p@(EOp Div [x, V (VInt 1)]) = p ~~~> x

    simplify p@(EOp Pow   [_, V (VInt 0)]) = p ~~~> V $ VInt 1
    simplify p@(EOp Pow   [x, V (VInt 1)]) = p ~~~> x
    simplify p@(EOp Times [ EOp Pow [a ,x]
                          , EOp Pow [a',y]
                          ])
        | a == a' = p ~~~> EOp Pow [ a
                                   , EOp Plus [x,y]
                                   ]

    simplify p@(EOp Not [EOp Not [x]]) = p ~~~> x
    simplify p@(EOp Not [EOp Imply [a,b]]) = p ~~~> EOp And [a, EOp Not [b]]

    simplify p@(EOp And [V (VBool False), _]) = p ~~~> V $ VBool False
    simplify p@(EOp And [V (VBool True ), x]) = p ~~~> x

    simplify p@(EOp Or  [V (VBool False), x]) = p ~~~> x
    simplify p@(EOp Or  [V (VBool True ), _]) = p ~~~> V $ VBool True

    simplify p@(EOp Imply [V (VBool True ), x]) = p ~~~> x
    simplify p@(EOp Imply [V (VBool False), x]) = p ~~~> EOp Not [x]
    simplify p@(EOp Imply [x,y]) | x == y = p ~~~> V $ VBool True

    simplify p@(EOp Iff [V (VBool True ), x]) = p ~~~> x
    simplify p@(EOp Iff [V (VBool False), x]) = p ~~~> EOp Not [x]

    simplify p = do
        res <- tryEvalArrowMaybe evaluate p
        case res of
            Nothing         -> return Nothing
            Just r | r == p -> return Nothing
            Just r          -> return (Just r)



--------------------------------------------------------------------------------
-- full evaluator --------------------------------------------------------------
--------------------------------------------------------------------------------

class Evaluate a b where
    evaluate :: (Applicative m, Monad m) => EvalArrow m a b

infixr 0 ~~>
(~~>) :: (MonadWriter [Doc] m, ParsePrint a, ParsePrint b) => a -> b -> m b
a ~~> b = do
    tell [ "[ EVALUATE ]"
            $$ nest 4 ("~~>" <+> pretty a)
            $$ nest 4 ("~~>" <+> pretty b)
         ]
    return b

instance (Evaluate a1 a2, Evaluate b1 b2) => Evaluate (a1,b1) (a2,b2) where
    evaluate (a1,b1) = do
        a2 <- evaluate a1
        b2 <- evaluate b1
        return (a2,b2)

instance (Evaluate a1 a2, Evaluate b1 b2, Evaluate c1 c2) => Evaluate (a1,b1,c1) (a2,b2,c2) where
    evaluate (a1,b1,c1) = do
        a2 <- evaluate a1
        b2 <- evaluate b1
        c2 <- evaluate c1
        return (a2,b2,c2)

instance (Evaluate a b) => Evaluate [a] [b] where
    evaluate = mapM evaluate

instance Evaluate Expr Value where
    evaluate p@(EHole (Identifier nm)) = do
        st <- get
        case M.lookup nm st of
            Nothing -> evalArrowError "Not bound" p
            Just x  -> case fromG x of
                Just (y :: Expr) -> do z <- evaluate y; p ~~> z
                _                -> case fromG x of
                    Just y -> p ~~> y
                    _      -> evalArrowError "Type mismatch (Value)" p
    evaluate (V v) = evaluate v
    evaluate p@(EOp Plus  [a,b]) = do (i,j) <- evaluate (a,b); p ~~> VInt $ i + j
    evaluate p@(EOp Minus [a,b]) = do (i,j) <- evaluate (a,b); p ~~> VInt $ i - j
    evaluate p@(EOp Times [a,b]) = do (i,j) <- evaluate (a,b); p ~~> VInt $ i * j
    evaluate p@(EOp Div   [a,b]) = do
        (i,j) <- evaluate (a,b)
        if j == 0
            then evalArrowError "Division by zero" p
            else p ~~> VInt (div i j)
    evaluate p@(EOp Mod   [a,b]) = do
        (i,j) <- evaluate (a,b)
        if j <= 0
            then evalArrowError "Non-positive modulus" p
            else p ~~> VInt (mod i j)
    evaluate p@(EOp Pow   [a,b]) = do
        let pow :: Integer -> Integer -> Integer
            pow = (^)
        (i,j) <- evaluate (a,b)
        p ~~> VInt (pow i j)
    evaluate p@(EOp Abs       [a]) = do i <- evaluate a; p ~~> VInt $ abs i
    evaluate p@(EOp Negate    [a]) = do i <- evaluate a; p ~~> VInt $ negate i
    evaluate p@(EOp Factorial [a]) = do i <- evaluate a; p ~~> VInt $ product [1..i]

    evaluate p@(EOp Lt  [a,b]) = do (i :: Integer, j) <- evaluate (a,b); p ~~> VBool $ i <  j
    evaluate p@(EOp Leq [a,b]) = do (i :: Integer, j) <- evaluate (a,b); p ~~> VBool $ i <= j
    evaluate p@(EOp Gt  [a,b]) = do (i :: Integer, j) <- evaluate (a,b); p ~~> VBool $ i >  j
    evaluate p@(EOp Geq [a,b]) = do (i :: Integer, j) <- evaluate (a,b); p ~~> VBool $ i >= j
    evaluate p@(EOp Neq [a,b]) = do (i :: Integer, j) <- evaluate (a,b); p ~~> VBool $ i /= j   -- need to be more general than this
    evaluate p@(EOp Eq  [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VInt  x , VInt  y ) -> p ~~> VBool $ x == y
            (VSet  xs, VSet  ys) -> let xs' = sort $ nub xs
                                        ys' = sort $ nub ys
                                    in  p ~~> VBool $ length xs' == length ys' && xs' == ys'
            (VMSet xs, VMSet ys) -> p ~~> VBool $ length xs == length ys && sort xs == sort ys
            _ -> evalArrowErrorDef p

    evaluate p@(EOp Not   [a]  ) = do i     <- evaluate a    ; p ~~> VBool $ not i
    evaluate p@(EOp Or    [a,b]) = do (i,j) <- evaluate (a,b); p ~~> VBool $ i || j
    evaluate p@(EOp And   [a,b]) = do (i,j) <- evaluate (a,b); p ~~> VBool $ i && j
    evaluate p@(EOp Imply [a,b]) = do (i :: Bool,j) <- evaluate (a,b); p ~~> VBool $ i <= j
    evaluate p@(EOp Iff   [a,b]) = do (i :: Bool,j) <- evaluate (a,b); p ~~> VBool $ i == j

    evaluate p@(EOp Union [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VSet  xs, VSet  ys) -> p ~~> VSet  $ sort $ nub $ xs ++ ys
            (VMSet xs, VMSet ys) -> p ~~> VMSet $ sort       $ xs ++ ys
            _ -> evalArrowErrorDef p

    evaluate p@(EOp Intersect [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VSet  xs, VSet  ys) -> p ~~> VSet  $ sort $ nub $ msetIntersect xs ys
            (VMSet xs, VMSet ys) -> p ~~> VMSet $ sort $       msetIntersect xs ys
            _ -> evalArrowErrorDef p
            where
                msetIntersect :: Eq a => [a] -> [a] -> [a]
                msetIntersect [] _ = []
                -- msetIntersect _  [] = []
                msetIntersect (x:xs) ys | x `elem` ys = x : msetIntersect xs (delete x ys)
                                        | otherwise   =     msetIntersect xs (delete x ys)

    evaluate p@(EOp Subset   [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VSet  xs, VSet  ys) -> let xs' = sort $ nub xs
                                        ys' = sort $ nub ys
                                    in  p ~~> VBool $ length xs' < length ys' && all (`elem` ys) xs
            (VMSet xs, VMSet ys) -> p ~~> VBool $ msetSubset xs ys
            _ -> evalArrowErrorDef p
            where
                msetSubset :: Eq a => [a] -> [a] -> Bool
                msetSubset [] [] = False
                msetSubset [] _  = True
                msetSubset (x:xs) ys | x `elem` ys = msetSubset xs (delete x ys)
                                     | otherwise   = False
    evaluate p@(EOp SubsetEq [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VSet  xs, VSet  ys) -> p ~~> VBool $ all (`elem` ys) xs
            (VMSet xs, VMSet ys) -> p ~~> VBool $ msetSubsetEq xs ys
            _ -> evalArrowErrorDef p
            where
                msetSubsetEq :: Eq a => [a] -> [a] -> Bool
                msetSubsetEq [] _ = True
                msetSubsetEq (x:xs) ys | x `elem` ys = msetSubsetEq xs (delete x ys)
                                       | otherwise   = False
    evaluate p@(EOp Supset   [a,b]) = do r <- evaluate (EOp Subset   [b,a]); p ~~> r
    evaluate p@(EOp SupsetEq [a,b]) = do r <- evaluate (EOp SubsetEq [b,a]); p ~~> r

    evaluate p@(EOp Card [a]) = do
        i <- evaluate a
        case i of
            VSet  xs -> p ~~> VInt $ genericLength $ nub xs
            VMSet xs -> p ~~> VInt $ genericLength       xs
            _        -> evalArrowErrorDef p

    evaluate p@(EOp Elem [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (v, VSet  vs) -> p ~~> VBool $ elem v vs
            (v, VMSet vs) -> p ~~> VBool $ elem v vs
            _ -> evalArrowErrorDef p

    evaluate p@(EOp op [a]) | op `elem` [Min,Max] = do
        let operator = case op of Min -> minimum; Max -> maximum; _ -> error "while evaluating Min or Max"
        i <- evaluate a
        case i of
            VSet  xs -> do ys <- evaluate xs; p ~~> operator ys
            VMSet xs -> do ys <- evaluate xs; p ~~> operator ys
            _        -> evalArrowErrorDef p

    evaluate p@(EOp ToSet [a]) = do
        i <- evaluate a
        case i of
            VMSet xs     -> p ~~> VSet $ sort $ nub xs
            VFunction xs -> p ~~> VSet $ sort $ nub xs
            VRelation xs -> p ~~> VSet $ sort $ nub xs
            _ -> evalArrowErrorDef p

    evaluate p@(EOp ToMSet [a]) = do
        i <- evaluate a
        case i of
            VSet xs      -> p ~~> VMSet $ sort xs
            VFunction xs -> p ~~> VMSet $ sort xs
            VRelation xs -> p ~~> VMSet $ sort xs
            _ -> evalArrowErrorDef p

    evaluate p@(EOp ToRel [a]) = do
        i <- evaluate a
        case i of
            VFunction xs -> p ~~> VRelation $ sort xs
            _ -> evalArrowErrorDef p

    evaluate p@(EOp Defined [a]) = do
        i <- evaluate a
        case i of
            VFunction xs -> do
                ys <- forM xs $ \ t -> case t of V (VTuple (tt:_)) -> return tt
                                                 _ -> evalArrowErrorDef t
                p ~~> VSet $ sort $ nub ys
            _ -> evalArrowErrorDef p

    evaluate p@(EOp Range [a]) = do
        i <- evaluate a
        case i of
            VFunction xs -> do
                ys <- forM xs $ \ t -> case t of V (VTuple (_:tt:_)) -> return tt
                                                 _ -> evalArrowErrorDef t
                p ~~> VSet $ sort $ nub ys
            _ -> evalArrowErrorDef p

    evaluate p@(EOp Image [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VFunction xs, y) -> do
                ys <- forM xs $ \ t -> case t of V (VTuple (V t1:V t2:_)) -> return (t1,t2)
                                                 _ -> evalArrowErrorDef t
                case [ t2 | (t1,t2) <- ys, y == t1 ] of
                    [z] -> p ~~> z
                    []  -> evalArrowError "'Not found' in function application" p
                    _   -> evalArrowError "'Multiple values' in function application" p
            _ -> evalArrowErrorDef p
    evaluate p@(EOp PreImage [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VFunction xs, y) -> do
                ys <- forM xs $ \ t -> case t of V (VTuple (V t1:V t2:_)) -> return (t1,t2)
                                                 _ -> evalArrowErrorDef t
                p ~~> VSet [ V t1 | (t1,t2) <- ys, y == t2 ]
            _ -> evalArrowErrorDef p
    evaluate p@(EOp Inverse [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VFunction xs, VFunction ys) -> do
                let
                    flipL (V (VTuple [x,y])) = return $ V $ VTuple [y,x]
                    flipL pL = evalArrowErrorDef pL
                ys' <- mapM flipL ys
                p ~~> VBool $ xs == ys'
            _ -> evalArrowErrorDef p

    -- a and b are elements of partition c
    -- Together evaluates to true, iff they are in the same part.
    evaluate p@(EOp Together [a,b,c]) = do
        (i,j,k) <- evaluate (a,b,c)
        case k of
            VPartition ks -> do
                let
                    bothInSet x y (V (VSet zs)) | V x `elem` zs && V y `elem` zs = True
                    bothInSet _ _ _ = False
                p ~~> VBool $ or [ bothInSet i j oneSet | oneSet <- ks ]
            _ -> evalArrowErrorDef p

    -- similar to Together. see above.
    evaluate p@(EOp Apart [a,b,c]) = do
        (i,j,k) <- evaluate (a,b,c)
        case k of
            VPartition ks -> do
                let
                    inSet x (V (VSet zs)) | V x `elem` zs = True
                    inSet _ _ = False

                    bothInSet x y (V (VSet zs)) | V x `elem` zs && V y `elem` zs = True
                    bothInSet _ _ _ = False
                p ~~> VBool $ and [       or [ inSet       i oneSet | oneSet <- ks ]
                                  ,       or [ inSet       j oneSet | oneSet <- ks ]
                                  , not $ or [ bothInSet i j oneSet | oneSet <- ks ]
                                  ]
            _ -> evalArrowErrorDef p

    -- evaluates to a set, the part which contains a in it. b is the partition.
    evaluate p@(EOp Party [a,b]) = do
        (i,j) <- evaluate (a,b)
        case j of
            VPartition js -> case [ VSet ks | V (VSet ks) <- js, V i `elem` ks ] of
                [z] -> p ~~> z
                [ ] -> evalArrowError "not present in partition" p
                _   -> evalArrowError "present in multiple parts" p
            _ -> evalArrowErrorDef p

    -- union of all parts
    evaluate p@(EOp Participants [a]) = do
        i <- evaluate a
        case i of
            VPartition js -> do
                jss <- forM js $ \ t -> case t of V (VSet ts) -> return ts
                                                  _ -> evalArrowErrorDef t
                p ~~> VSet $ nub $ sort $ concat jss
            _ -> evalArrowErrorDef p

    -- set of all parts
    evaluate p@(EOp Parts [a]) = do
        i <- evaluate a
        case i of
            VPartition js -> p ~~> VSet js
            _ -> evalArrowErrorDef p

    -- number of occurrences of b in a. a is a mset.
    evaluate p@(EOp Freq [a,b]) = do
        (i,j) <- evaluate (a,b)
        case i of
            VMSet ks -> p ~~> VInt $ sum [ 1 | k <- ks, V j == k ]
            _ -> evalArrowErrorDef p

    -- similar to Freq, however b is a matrix of values this time.
    evaluate p@(EOp Hist [a,b]) = do
        (i,j) <- evaluate (a,b)
        case (i,j) of
            (VMSet is, VMatrix js) -> p ~~> VMatrix [ V $ VInt $ sum [ 1 | k <- is, j' == k ]
                                                    | j' <- js
                                                    ]
            _ -> evalArrowErrorDef p

    evaluate p@(EOp Index [a,b]) = do
        i <- evaluate a
        j <- tryEvalArrowMaybe evaluate b
        case (j,b) of
            (Just jj,_) -> do
                (is,k) <- case (i,jj) of
                            (VTuple  is, VInt k) -> return (is,k)
                            (VMatrix is, VInt k) -> return (is,k)
                            _ -> evalArrowErrorDef p
                if k >= 0 && k < genericLength is
                    then case genericIndex is k of
                            V v -> p ~~> v
                            _   -> evalArrowErrorDef p
                    else evalArrowError "index out of bounds" p
            (Nothing,D jj) -> case (i,jj) of
                                (VMatrix is, DInt RAll) -> p ~~> VMatrix is
                                (VMatrix is, DInt (RFromTo Nothing   Nothing)) -> p ~~> VMatrix is
                                (VMatrix is, DInt (RFromTo (Just lb) Nothing)) -> do
                                    lbInt :: Int <- evaluate lb
                                    p ~~> VMatrix $ drop lbInt is
                                (VMatrix is, DInt (RFromTo Nothing (Just ub))) -> do
                                    ubInt :: Int <- evaluate ub
                                    p ~~> VMatrix $ take (ubInt + 1) is
                                (VMatrix is, DInt (RFromTo (Just lb) (Just ub))) -> do
                                    lbInt :: Int <- evaluate lb
                                    ubInt :: Int <- evaluate ub
                                    p ~~> VMatrix $ take (ubInt + 1 - lbInt) $ drop lbInt is
                                _ -> evalArrowErrorDef p
            _ -> evalArrowErrorDef p

    evaluate p@(EOp HasType _) = evalArrowError "not implemented" p -- do
        -- i <- typeOf a
        -- j <- typeOf b
        -- return $ VBool $ i `typeUnify` j

    evaluate p@(EOp HasDomain _) = evalArrowError "not implemented" p -- do
        -- i <- domainOf a
        -- j <- domainOf b
        -- return $ VBool $ i `typeUnify` j

    evaluate p@(EOp Replace []) = evalArrowError "not implemented" p

    evaluate p@(EOp AllDiff [a]) = do
        i <- evaluate a
        case i of
            VMatrix xs -> p ~~> VBool $ xs == nub xs
            _ -> evalArrowErrorDef p

    evaluate p = evalArrowErrorDef p

instance Evaluate Expr Expr where
    evaluate p@(EHole (Identifier nm)) = do
        st <- get
        case M.lookup nm st of
            Nothing -> evalArrowError "Not bound" p
            Just x  -> case fromG x of
                Nothing -> evalArrowError "Type mismatch" p
                Just y  -> p ~~> y
    evaluate x = V <$> evaluate x

instance Evaluate Value Value where
    evaluate p@(VBool {}) = return p
    evaluate p@(VInt  {}) = return p
    evaluate (VMatrix    xs) = VMatrix    <$> evaluate xs
    evaluate (VTuple     xs) = VTuple     <$> evaluate xs
    evaluate (VSet       xs) = VSet       <$> evaluate xs
    evaluate (VMSet      xs) = VMSet      <$> evaluate xs
    evaluate (VFunction  xs) = VFunction  <$> evaluate xs
    evaluate (VRelation  xs) = VRelation  <$> evaluate xs
    evaluate (VPartition xs) = VPartition <$> evaluate xs
    evaluate p = evalArrowErrorDef p

instance Evaluate Expr Integer where
    evaluate x = do
        v :: Value   <- evaluate x
        i :: Integer <- evaluate v
        return i

instance Evaluate Value Integer where
    evaluate (VBool False) = return 0
    evaluate (VBool True ) = return 1
    evaluate (VInt i) = return i
    evaluate v        = evalArrowErrorDef v

instance Evaluate Expr Int where
    evaluate x = fromInteger <$> evaluate x

instance Evaluate Expr Bool where
    evaluate x = do
        v :: Value <- evaluate x
        b :: Bool  <- evaluate v
        return b

instance Evaluate Value Bool where
    evaluate (VBool b) = return b
    evaluate (VInt  0) = return False
    evaluate (VInt  _) = return True
    evaluate v         = evalArrowErrorDef v
