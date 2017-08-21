{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.FrameUpdate where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpFrameUpdate x = OpFrameUpdate x                  -- old
                                     x                  -- new
                                     [(Name, Name)]     -- names of the focus variables
                                     x                  -- constraint
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFrameUpdate x)
instance Hashable  x => Hashable  (OpFrameUpdate x)
instance ToJSON    x => ToJSON    (OpFrameUpdate x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFrameUpdate x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpFrameUpdate x) where
    typeOf p@(OpFrameUpdate old new _names _cons) = do
        tyOld <- typeOf old
        tyNew <- typeOf new
        -- delay the type-checking of the constraint component
        -- cannot possibly type-check it without knowing the types of the focus variables
        -- tyCons <- typeOf cons
        let tyCons = TypeBool
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

instance EvaluateOp OpFrameUpdate where
    -- TODO: How do we evaluate this???
    evaluateOp op = na $ "evaluateOp{OpFrameUpdate}:" <++> pretty (show op)

instance SimplifyOp OpFrameUpdate x where
    simplifyOp _ = na "simplifyOp{OpFrameUpdate}"

instance Pretty x => Pretty (OpFrameUpdate x) where
    prettyPrec _ (OpFrameUpdate old new names cons) =
        "frameUpdate" <> prettyList prParens ","
            [ pretty old
            , pretty new
            , prettyList prBrackets "," (map (\ (a,b) -> prettyList prParens "," [a,b]) names)
            , pretty cons
            ]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFrameUpdate x) where
    varSymBreakingDescription (OpFrameUpdate old new names cons) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpFrameUpdate")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription old
            , varSymBreakingDescription new
            , toJSON names
            , varSymBreakingDescription cons
            ])
        ]
