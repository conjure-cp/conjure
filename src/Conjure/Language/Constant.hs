{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Constant
    ( Constant(..)
    , normaliseConstant
    , validateConstantForDomain
    , mkUndef, isUndef
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.AbstractLiteral

import Conjure.Language.TypeOf
import Conjure.Language.AdHoc
import Conjure.Language.Pretty

-- QuickCheck
import Test.QuickCheck ( Arbitrary(..), oneof )


data Constant
    = ConstantBool Bool
    | ConstantInt Int
    | ConstantEnum Name   {- name for the enum domain -}
                   [Name] {- values in the enum domain -}
                   Name   {- the literal -}
    | ConstantAbstract (AbstractLiteral Constant)
    | DomainInConstant (Domain () Constant)
    | ConstantUndefined Text
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize Constant
instance Hashable  Constant
instance ToJSON    Constant where toJSON = genericToJSON jsonOptions
instance FromJSON  Constant where parseJSON = genericParseJSON jsonOptions

instance Arbitrary Constant where
    arbitrary = oneof
        [ ConstantBool <$> arbitrary
        , ConstantInt <$> arbitrary
        ]

instance TypeOf Constant where
    typeOf ConstantBool{}           = return TypeBool
    typeOf ConstantInt{}            = return TypeInt
    typeOf (ConstantEnum defn _ _ ) = return (TypeEnum defn)
    typeOf (ConstantAbstract x    ) = typeOf x
    typeOf (DomainInConstant dom) = typeOf dom
    typeOf (ConstantUndefined reason) = fail ("type of undefined," <+> pretty reason)

instance Pretty Constant where
    pretty (ConstantBool False) = "false"
    pretty (ConstantBool True ) = "true"
    pretty (ConstantInt  x    ) = pretty x
    pretty (ConstantEnum _ _ x) = pretty x
    pretty (ConstantAbstract x) = pretty x
    pretty (DomainInConstant d) = "`" <> pretty d <> "`"
    pretty (ConstantUndefined reason) = "undefined" <> prParens (pretty reason)

instance ExpressionLike Constant where
    fromInt = ConstantInt
    intOut (ConstantInt x) = return x
    intOut c = fail ("Expecting an integer, but found:" <+> pretty c)

    fromBool = ConstantBool
    boolOut (ConstantBool x) = return x
    boolOut ConstantUndefined{} = return False
    boolOut c = fail ("Expecting a boolean, but found:" <+> pretty c)

    fromList xs = ConstantAbstract $ AbsLitMatrix (mkDomainIntB 1 (fromInt $ length xs)) xs
    listOut (ConstantAbstract (AbsLitMatrix _ xs)) = return xs
    listOut c = fail ("Expecting a matrix literal, but found:" <+> pretty c)

instance ReferenceContainer Constant where
    fromName name = bug ("ReferenceContainer{Constant} --" <+> pretty name)

instance DomainContainer Constant (Domain ()) where
    fromDomain = DomainInConstant
    domainOut (DomainInConstant dom) = return dom
    domainOut _ = fail "domainOut{Constant}"

mkUndef :: Doc -> Constant
mkUndef = ConstantUndefined . stringToText . show

isUndef :: Constant -> Bool
isUndef ConstantUndefined{} = True
isUndef _ = False

normaliseConstant :: Constant -> Constant
normaliseConstant x@ConstantBool{} = x
normaliseConstant x@ConstantInt{}  = x
normaliseConstant x@ConstantEnum{} = x
normaliseConstant (ConstantAbstract x) = ConstantAbstract (normaliseAbsLit normaliseConstant x)
normaliseConstant (DomainInConstant d) = DomainInConstant (normaliseDomain normaliseConstant d)
normaliseConstant x@ConstantUndefined{} = x

instance Num Constant where
    ConstantInt x + ConstantInt y = ConstantInt (x+y)
    x + y = bug $ vcat [ "Num Constant (+)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt x - ConstantInt y = ConstantInt (x-y)
    x - y = bug $ vcat [ "Num Constant (-)", "x:" <+> pretty x, "y:" <+> pretty y ]
    ConstantInt x * ConstantInt y = ConstantInt (x*y)
    x * y = bug $ vcat [ "Num Constant (*)", "x:" <+> pretty x, "y:" <+> pretty y ]
    abs (ConstantInt x) = ConstantInt (abs x)
    abs x = bug $ vcat [ "Num Constant abs", "x:" <+> pretty x ]
    signum (ConstantInt x) = ConstantInt (signum x)
    signum x = bug $ vcat [ "Num Constant signum", "x:" <+> pretty x ]
    fromInteger = ConstantInt . fromInteger


-- | Assuming both the value and the domain are normalised
validateConstantForDomain :: forall m r . (MonadFail m, Pretty r) => Constant -> Domain r Constant -> m ()

validateConstantForDomain ConstantBool{} DomainBool{} = return ()

validateConstantForDomain _ (DomainInt []) = return ()              -- no restrictions

validateConstantForDomain c@(ConstantInt i) d@(DomainInt rs) =
    let
        intInRange RangeOpen                                      = True
        intInRange (RangeSingle (ConstantInt a))                  = i == a
        intInRange (RangeLowerBounded (ConstantInt a))            = i >= a
        intInRange (RangeUpperBounded (ConstantInt a))            = i <= a
        intInRange (RangeBounded (ConstantInt a) (ConstantInt b)) = i >= a && i <= b
        intInRange _                                              = False
    in  unless (any intInRange rs) (constantNotInDomain c d)

validateConstantForDomain _ (DomainEnum _ Nothing _) = return ()    -- no restrictions
validateConstantForDomain c d@(DomainEnum _ _ Nothing) = fail $ vcat [ "validateConstantForDomain: enum not handled"
                                                                     , pretty c
                                                                     , pretty d
                                                                     ]
validateConstantForDomain
    c@ConstantInt{}
    d@(DomainEnum _ (Just ranges) (Just mp)) = nested c d $ do
        let
            -- lu :: MonadFail m => Name -> m Constant
            lu nm = case lookup nm mp of Nothing -> fail $ "No value for:" <+> pretty nm
                                         Just v  -> return (ConstantInt v)

            -- lu2 :: MonadFail m => Range Name -> m (Range Constant)
            lu2 = mapM lu

        rs <- mapM lu2 ranges
        validateConstantForDomain c (DomainInt rs :: Domain r Constant)

validateConstantForDomain
    c@(ConstantAbstract (AbsLitTuple cs))
    d@(DomainTuple ds) = nested c d $ zipWithM_ validateConstantForDomain cs ds

validateConstantForDomain
    c@(ConstantAbstract (AbsLitMatrix cIndex vals))
    d@(DomainMatrix dIndex dInner) = do
        nested c d $
            mapM_ (`validateConstantForDomain` dInner) vals
        unless (cIndex == dIndex) $ fail $ vcat
            [ "The indices do not match between the value and the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            ]

validateConstantForDomain
    c@(ConstantAbstract (AbsLitSet vals))
    d@(DomainSet _ _ dInner) = nested c d $
        mapM_ (`validateConstantForDomain` dInner) vals

validateConstantForDomain
    c@(ConstantAbstract (AbsLitMSet vals))
    d@(DomainMSet _ _ dInner) = nested c d $
        mapM_ (`validateConstantForDomain` dInner) vals

validateConstantForDomain
    c@(ConstantAbstract (AbsLitFunction vals))
    d@(DomainFunction _ _ dFrom dTo) = nested c d $ do
        mapM_ (flip validateConstantForDomain dFrom . fst) vals
        mapM_ (flip validateConstantForDomain dTo   . snd) vals
        
validateConstantForDomain
    c@(ConstantAbstract (AbsLitRelation valss))
    d@(DomainRelation _ _ dInners) = nested c d $
        forM_ valss $ \ vals ->
            zipWithM_ validateConstantForDomain vals dInners

validateConstantForDomain
    c@(ConstantAbstract (AbsLitPartition valss))
    d@(DomainPartition _ _ dInner) = nested c d $
        mapM_ (`validateConstantForDomain` dInner) (concat valss)

validateConstantForDomain c d = constantNotInDomain c d


nested :: (MonadFail m, Pretty r) => Constant -> Domain r Constant -> Either Doc () -> m ()
nested _ _ Right{} = return ()
nested c d (Left err) = fail $ vcat
    [ "The value is not a member of the domain."
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty (show d)
    , "Because of:", nest 4 err
    ]

constantNotInDomain :: (MonadFail m, Pretty r) => Constant -> Domain r Constant -> m ()
constantNotInDomain c d = fail $ vcat
    [ "The value is not a member of the domain."
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty (show d)
    ]
