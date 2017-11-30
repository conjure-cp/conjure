{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.FrameUpdate where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpFrameUpdate x
        = OpFrameUpdate
            x      -- source
            x      -- target
            [Name] -- source variables
            [Name] -- target variables
            x      -- constraint
        -- ignore this constructor, it is only here to support the TH
        | OpFrameUpdateInternal
            x      -- source
            x      -- target
            (Either [Name] x)
            (Either [Name] x)
            x      -- constraint
        | OpFrameUpdateEprime
            x      -- source
            x      -- target
            [Name] -- source indices
            [Name] -- target indices
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFrameUpdate x)
instance Hashable  x => Hashable  (OpFrameUpdate x)
instance ToJSON    x => ToJSON    (OpFrameUpdate x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFrameUpdate x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFrameUpdate x) where
    typeOf p@(OpFrameUpdate source target _ _ cons) = do
        tyOld <- typeOf source
        tyNew <- typeOf target
        tyCons <- typeOf cons
        case (typeUnify tyOld tyNew, typeUnify tyCons TypeBool) of
            (True, True) -> return TypeBool
            (False, _  ) -> raiseTypeError $ vcat [ pretty p
                                                  , "First two arguments must be of the same type."
                                                  , "Argument 1 has type:" <+> pretty tyOld
                                                  , "Argument 2 has type:" <+> pretty tyNew
                                                  ]
            (_, False  ) -> raiseTypeError $ vcat [ pretty p
                                                  , "Fourth argument must be of type bool."
                                                  , "Instead, it has the following type:" <+> pretty tyCons
                                                  ]
    typeOf p@OpFrameUpdateInternal{} = bug $ "typeOf{OpFrameUpdateInternal}" <+> pretty p
    typeOf OpFrameUpdateEprime{} = return TypeBool

instance EvaluateOp OpFrameUpdate where
    -- TODO: How do we evaluate this???
    evaluateOp op = na $ "evaluateOp{OpFrameUpdate}:" <++> pretty (show op)

instance SimplifyOp OpFrameUpdate x where
    simplifyOp _ = na "simplifyOp{OpFrameUpdate}"

instance Pretty x => Pretty (OpFrameUpdate x) where
    prettyPrec _ (OpFrameUpdate source target sourceFocus targetFocus cons) =
        "frameUpdate" <> prettyList prParens ","
            [ pretty source
            , pretty target
            , prettyList prBrackets "," sourceFocus
            , prettyList prBrackets "," targetFocus
            , pretty cons
            ]
    prettyPrec _ p@OpFrameUpdateInternal{} = bug $ "prettyPrec{OpFrameUpdateInternal}" <+> pretty (show p)
    prettyPrec _ (OpFrameUpdateEprime source target sourceFocus targetFocus) =
        "frameUpdate" <> prettyList prParens ","
            [ pretty source
            , pretty target
            , prettyList prBrackets "," sourceFocus
            , prettyList prBrackets "," targetFocus
            ]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFrameUpdate x) where
    varSymBreakingDescription (OpFrameUpdate source target sourceFocus targetFocus cons) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpFrameUpdate")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription source
            , varSymBreakingDescription target
            , toJSON sourceFocus
            , toJSON targetFocus
            , varSymBreakingDescription cons
            ])
        ]
    varSymBreakingDescription OpFrameUpdateInternal{} = bug "varSymBreakingDescription{OpFrameUpdateInternal}"
    varSymBreakingDescription (OpFrameUpdateEprime source target sourceFocus targetFocus) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpFrameUpdateEprime")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription source
            , varSymBreakingDescription target
            , toJSON sourceFocus
            , toJSON targetFocus
            ])
        ]
