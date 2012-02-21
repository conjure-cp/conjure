{-# LANGUAGE DeriveDataTypeable  #-}

module GenericOps.Testing where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Dynamic
import Data.Generics

import GenericOps.Core
import GenericOps.GMatch

-- test :: Expr -> Expr -> (Either String (), [(String,GNode)])
-- test x y = runState (runErrorT (gmatch (mkG x) (mkG y))) []
-- test :: Expr -> Expr -> IO (Either String [(String,GNode)])
-- test :: a -> b -> IO ()
test x y = do
    t <- evalStateT (runErrorT (gmatchCall (mkG x) (mkG y))) ([],[])
    case t of
        Left msg -> putStrLn msg
        Right ts -> forM_ ts $ \ (nm,GNode _ i) -> putStrLn (nm ++ ": " ++ show i)

testBind :: [(String,GNode)] -> Expr -> IO ()
testBind tbl x = do
    t <- evalStateT (runErrorT (gbind (mkG x))) (tbl,[])
    case t of
        Left msg -> putStrLn msg
        Right (GNode _ i) -> putStrLn (show i)



instance NodeTag Int where nodeTag x = "Int{" ++ show x ++ "}"
instance Hole    Int where hole _    = NotAHole
instance GPlate  Int where gplate x  = ([], \ _ -> x)

instance NodeTag Bool where nodeTag x = "Bool{" ++ show x ++ "}"
instance Hole    Bool where hole _    = NotAHole
instance GPlate  Bool where gplate x  = ([], \ _ -> x)



xExpr :: Expr
xExpr = Plus
                (EHole "x")
                (V (I 12))

yExpr :: Expr
yExpr = Plus
                (Plus (V (I 13)) (V (I 14)))
                (V (I 13))


aExpr :: Expr
aExpr = Plus (V (I 1))
         (V (Pair (V (VHole "x"))
                  (V (I 2))
            )
         )

bExpr :: Expr
bExpr = Plus (V (I 1))
         (V (Triple (V (I 13))
                    (V (I 2))
                    (V (I 1))
            )
         )

data Expr = EHole String
    | V Value
    | Plus Expr Expr
    deriving (Eq, Show, Data, Typeable)

instance NodeTag Expr

instance Hole Expr where
    hole (EHole "_") = UnnamedHole
    hole (EHole nm ) = NamedHole nm
    hole _           = NotAHole

instance GPlate Expr where
    gplate (V v) =
        ( [mkG v]
        , \ gs -> case gs of
                    [g] -> case fromG g of
                            Just v' -> V v'
                            _       -> gplateError
                    _   -> gplateError
        )
    gplate (EHole nm) =
        ( []
        , \ _ -> EHole nm
        )
    gplate (Plus a b) =
        ( [mkG a, mkG b]
        , \ gs -> case gs of
                    [x,y] -> case (fromG x, fromG y) of
                                (Just x', Just y') -> Plus x' y'
                                _ -> gplateError
                    _ -> gplateError
        )

data Value = VHole String
    | I Int
    | B Bool
    | Pair Expr Expr
    | Triple Expr Expr Expr
    deriving (Eq, Show, Data, Typeable)

instance NodeTag Value

instance Hole Value where
    hole (VHole "_") = UnnamedHole
    hole (VHole nm ) = NamedHole nm
    hole _           = NotAHole


instance GPlate Value where
    gplate (VHole nm) =
        ( []
        , \ _ -> VHole nm
        )
    gplate (I i) =
        ( []
        , \ _ -> I i
        )
    gplate (B b) =
        ( []
        , \ _ -> B b
        )
    gplate (Pair a b) =
        ( [mkG a, mkG b]
        , \ gs -> case gs of
                    [x,y] -> case (fromG x, fromG y) of
                                (Just x', Just y') -> Pair x' y'
                                _ -> gplateError
                    _ -> gplateError
        )
    gplate (Triple a b c) =
        ( [mkG a, mkG b, mkG c]
        , \ gs -> case gs of
                    [x,y,z] -> case (fromG x, fromG y, fromG z) of
                                (Just x', Just y', Just z') -> Triple x' y' z'
                                _ -> gplateError
                    _ -> gplateError
        )

