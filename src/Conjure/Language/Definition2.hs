{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}

module Conjure.Language.Definition2 where

import Data.Proxy

class Kind a where
    data Type a
    data Domain a
    data Constant a
    typeOfDomain :: Proxy a -> Type a
    allValues :: Domain a -> [Constant a]
    numValues :: Domain a -> Int

    numValues = length . allValues


data KindBool
instance Kind KindBool where
    data Type     KindBool = TypeBool                       deriving Show
    data Domain   KindBool = DomainBool                     deriving Show
    data Constant KindBool = EFalse | ETrue                 deriving Show
    typeOfDomain Proxy = TypeBool
    allValues DomainBool = [EFalse, ETrue]
    numValues DomainBool = 2


data KindInt
instance Kind KindInt where
    data Type     KindInt = TypeInt                         deriving Show
    data Domain   KindInt = DomainInt Int Int               deriving Show
    data Constant KindInt = ConstantInt Int                 deriving Show
    typeOfDomain Proxy = TypeInt
    allValues (DomainInt l u) = map ConstantInt [l .. u]
    numValues (DomainInt l u) = u - l + 1


data Fixity = Prefix | PrefixNumArgs Int | Infix Assoc Int
    deriving (Eq, Ord, Show)
data Assoc = AssocL | AssocN | AssocR
    deriving (Eq, Ord, Show)


class OpC a x (kInps :: [*]) kOut where
    data Op a x kInps kOut
    fixity :: Proxy a -> Fixity
    typeOfOp :: Op a x kInps kOut -> Type kOut


-- | OpPlus
--
-- >>> typeOfOp (OpPlus (MkXConstant $ ConstantInt 1) (MkXConstant $ ConstantInt 3))
-- TypeInt
--
data OpPlus
instance ( ExpressionClass x KindInt
         , kInps ~ [KindInt,KindInt]
         , kOut  ~ KindInt
         ) => OpC OpPlus x kInps kOut where
    data Op OpPlus x kInps kOut = OpPlus (Expression x) (Expression x)
    fixity Proxy = Infix AssocL 600
    typeOfOp (OpPlus a b) =
        case (typeOfExpression a, typeOfExpression b) of
            (TypeInt, TypeInt) -> TypeInt


-- | OpTimes
--
-- >>> typeOfOp (OpTimes (MkXConstant $ ConstantInt 1) (MkXConstant $ ConstantInt 3))
-- TypeInt
--
data OpTimes
instance ( kInps ~ [KindInt,KindInt]
         , kOut  ~ KindInt
         , ExpressionClass x kOut
         ) => OpC OpTimes x kInps kOut where
    data Op OpTimes x kInps kOut = OpTimes (Expression x) (Expression x)
    fixity Proxy = Infix AssocL 600
    typeOfOp (OpTimes a b) =
        case (typeOfExpression a, typeOfExpression b) of
            (TypeInt, TypeInt) -> TypeInt



-- data OpTimes
-- instance OpC OpTimes x where
--     data Op OpTimes x = OpTimes (Expression x) (Expression x)
--     fixity Proxy = Infix AssocL 700
--     typeOfOp (OpTimes a b) =
--         case (typeOfExpression a, typeOfExpression b) of
--             (TypeInt, TypeInt) -> TypeInt


class ExpressionClass a k where
    data Expression a
    typeOfExpression :: Expression a -> Type k


-- | data type for injecting constants into expressions
--
-- >>> typeOfExpression (MkXConstant EFalse)
-- TypeBool
--
-- >>> typeOfExpression (MkXConstant (ConstantInt 1))
-- TypeInt
--
data ExpressionConstant k
instance (Kind k, Kind k2, k ~ k2) => ExpressionClass (ExpressionConstant k) k2 where
    data Expression (ExpressionConstant k) = MkXConstant (Constant k)
        -- deriving Show
    typeOfExpression (MkXConstant _) = typeOfDomain Proxy

-- | OpPlus is an ExpressionClass
--
-- >>> typeOfExpression (MkXOp (OpPlus (MkXConstant $ ConstantInt 1) (MkXConstant $ ConstantInt 3))) :: Type KindInt
-- TypeInt
--
-- >>> typeOfExpression (MkXOp (OpPlus (MkXConstant $ ConstantInt 1) (MkXConstant $ ConstantInt 3)))
-- TypeInt
--
data ExpressionOp op x (kInps :: [*]) kOut
instance ( OpC op x kInps kOut
         , ExpressionClass x kOut
         , kOut ~ kOut2
         ) => ExpressionClass (ExpressionOp op x kInps kOut) kOut2 where
    data Expression (ExpressionOp op x kInps kOut) = MkXOp (Op op x kInps kOut)
        -- deriving Show
    typeOfExpression (MkXOp op) = typeOfOp op





-- TODO: Domain needs to be parameterised over (Constant | Expression)
-- TODO: Expression needs to be parameterised over (Category (Constant | Parameter | Quantified | Decision))
--                                             and (Type)





