{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Conjure.Language.Expression.Op.Restrict where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
-- import {-# SOURCE #-} Conjure.Process.ValidateConstantForDomain ( validateConstantForDomain )

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpRestrict x = OpRestrict x {- the function -} x {- the domain -}
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRestrict x)
instance Hashable  x => Hashable  (OpRestrict x)
instance ToJSON    x => ToJSON    (OpRestrict x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRestrict x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x, Domain () x :< x) => TypeOf (OpRestrict x) where
    typeOf p@(OpRestrict f domX) = do
        dom :: Domain () x   <- project domX
        (from ,to )<- getFunctionTypes f
        from'                <- typeOfDomain dom
        if typesUnify [from, from']
            then return (TypeFunction (mostDefined [from', from]) to)
            else raiseTypeError p

instance SimplifyOp OpRestrict x where
    simplifyOp _ = na "simplifyOp{OpRestrict}"

instance Pretty x => Pretty (OpRestrict x) where
    prettyPrec _ (OpRestrict a b) = "restrict" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpRestrict x) where
    varSymBreakingDescription (OpRestrict a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpRestrict")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]


    
