{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.TildeLt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpTildeLt x = OpTildeLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTildeLt x)
instance Hashable  x => Hashable  (OpTildeLt x)
instance ToJSON    x => ToJSON    (OpTildeLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTildeLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpTildeLt x) where
    opLexeme _ = L_TildeLt

instance (TypeOf x, Pretty x) => TypeOf (OpTildeLt x) where
    typeOf p@(OpTildeLt a b) = sameToSameToBool p a b []

instance EvaluateOp OpTildeLt where
    evaluateOp (OpTildeLt x y) = return $ ConstantBool $ tilLt x y
        where
            freq :: Eq a => a -> [a] -> Int
            freq i xs = sum [ 1 | j <- xs , i == j ]

            tupleE (i,j) = ConstantAbstract $ AbsLitTuple [i,j]

            tilLt (ConstantBool a) (ConstantBool b) = a < b
            tilLt (ConstantInt Nothing a) (ConstantInt Nothing b) = a < b
            tilLt (viewConstantTuple -> Just [])
                  (viewConstantTuple -> Just []) = False
            tilLt (viewConstantTuple -> Just (a:as))
                  (viewConstantTuple -> Just (b:bs)) =
                      if tilLt a b
                          then True
                          else a == b &&
                               tilLt (ConstantAbstract $ AbsLitTuple as)
                                     (ConstantAbstract $ AbsLitTuple bs)
            tilLt (viewConstantSet -> Just as)
                  (viewConstantSet -> Just bs) =
                or [ and [ freq i as < freq i bs
                         , and [ if tilLt j i
                                     then freq j as == freq j bs
                                     else True
                               | j <- cs
                               ]
                         ]
                   | let cs = sortNub (as ++ bs)
                   , i <- cs
                   ]
            tilLt (viewConstantMSet -> Just as)
                  (viewConstantMSet -> Just bs) =
                or [ and [ freq i as < freq i bs
                         , and [ if tilLt j i
                                     then freq j as == freq j bs
                                     else True
                               | j <- cs
                               ]
                         ]
                   | let cs = as ++ bs
                   , i <- cs
                   ]
            tilLt (viewConstantFunction -> Just as')
                  (viewConstantFunction -> Just bs') =
                or [ and [ freq i as < freq i bs
                         , and [ if tilLt j i
                                     then freq j as == freq j bs
                                     else True
                               | j <- cs
                               ]
                         ]
                   | let as = map tupleE as'
                   , let bs = map tupleE bs'
                   , let cs = as ++ bs
                   , i <- cs
                   ]
            tilLt (viewConstantRelation -> Just as')
                  (viewConstantRelation -> Just bs') =
                or [ and [ freq i as < freq i bs
                         , and [ if tilLt j i
                                     then freq j as == freq j bs
                                     else True
                               | j <- cs
                               ]
                         ]
                   | let as = map (ConstantAbstract . AbsLitTuple) as'
                   , let bs = map (ConstantAbstract . AbsLitTuple) bs'
                   , let cs = as ++ bs
                   , i <- cs
                   ]
            tilLt (viewConstantPartition -> Just as')
                  (viewConstantPartition -> Just bs') =
                or [ and [ freq i as < freq i bs
                         , and [ if tilLt j i
                                     then freq j as == freq j bs
                                     else True
                               | j <- cs
                               ]
                         ]
                   | let as = map (ConstantAbstract . AbsLitSet) as'
                   , let bs = map (ConstantAbstract . AbsLitSet) bs'
                   , let cs = as ++ bs
                   , i <- cs
                   ]
            tilLt a b = a < b

instance SimplifyOp OpTildeLt x where
    simplifyOp _ = na "simplifyOp{OpTildeLt}"

instance Pretty x => Pretty (OpTildeLt x) where
    prettyPrec prec op@(OpTildeLt a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTildeLt x) where
    varSymBreakingDescription (OpTildeLt a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpTildeLt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
