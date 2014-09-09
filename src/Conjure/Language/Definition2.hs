{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Conjure.Language.Definition2 where
    

class Kind a where
    data Type a
    data Domain a
    data Constant a
    typeOfDomain :: a -> Type a
    allValues :: Domain a -> [Constant a]
    numValues :: Domain a -> Int

    numValues = length . allValues


data KindBool
instance Kind KindBool where
    data Type     KindBool = TypeBool                       deriving Show
    data Domain   KindBool = DomainBool                     deriving Show
    data Constant KindBool = EFalse | ETrue                 deriving Show
    typeOfDomain _ = TypeBool
    allValues DomainBool = [EFalse, ETrue]
    numValues DomainBool = 2


data KindInt
instance Kind KindInt where
    data Type     KindInt = TypeInt                         deriving Show
    data Domain   KindInt = DomainInt Int Int               deriving Show
    data Constant KindInt = ConstantInt Int                 deriving Show
    typeOfDomain _ = TypeInt
    allValues (DomainInt l u) = map ConstantInt [l .. u]
    numValues (DomainInt l u) = u - l + 1


data Fixity = Prefix | PrefixNumArgs Int | Infix Assoc Int
    deriving (Eq, Ord, Show)
data Assoc = AssocL | AssocN | AssocR
    deriving (Eq, Ord, Show)


class OpC a k where
    data Operands a
    fixity :: a -> Fixity
    typeOfOp :: Operands a -> Type k


data OpPlus
instance OpC OpPlus KindInt where
    data Operands OpPlus = forall x k .
                            ( ExpressionClass x k
                            , k ~ KindInt
                            ) => OpPlusOperands (Expression x) (Expression x)
        -- deriving Show
    fixity _ = Infix AssocL 600
    typeOfOp (OpPlusOperands _ _) = TypeInt


class ExpressionClass a k where
    data Expression a
    typeOfExpression :: Expression a -> Type k


-- | data type for injecting constants into expressions
-- 
-- >>> typeOfExpression (MkExpressionConstant EFalse)
-- TypeBool
-- 
-- >>> typeOfExpression (MkExpressionConstant (ConstantInt 1))
-- TypeInt
-- 
data ExpressionConstant k
instance (Kind k, Kind k2, k ~ k2) => ExpressionClass (ExpressionConstant k) k2 where
    data Expression (ExpressionConstant k) = MkExpressionConstant (Constant k)
        -- deriving Show
    typeOfExpression (MkExpressionConstant _) = typeOfDomain undefined

-- | OpPlus is an ExpressionClass
-- 
-- >>> typeOfExpression (MkOpPlus (OpPlusOperands (MkExpressionConstant $ ConstantInt 1) (MkExpressionConstant $ ConstantInt 3)))
-- TypeInt
-- 
instance (k ~ KindInt) => ExpressionClass OpPlus k where
    data Expression OpPlus = MkOpPlus (Operands OpPlus)
        -- deriving Show
    typeOfExpression (MkOpPlus (OpPlusOperands a b)) =
        case (typeOfExpression a, typeOfExpression b) of
            (TypeInt, TypeInt) -> TypeInt






-- TODO: Domain needs to be parameterised over (Constant | Expression)
-- TODO: Expression needs to be parameterised over (Category (Constant | Parameter | Quantified | Decision))
--                                             and (Type)





