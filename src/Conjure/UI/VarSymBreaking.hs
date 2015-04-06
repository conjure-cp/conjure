{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Conjure.UI.VarSymBreaking where

import Conjure.Prelude
import Conjure.Language.Definition
-- import Conjure.Language.Domain
import Conjure.Language.Pretty
-- import Conjure.UI.Model ( nbUses )

-- aeson
-- import Data.Aeson ( (.=), (.:) )
import qualified Data.Aeson as JSON
-- import qualified Data.Aeson.Types as JSON

-- unordered-containers
import Data.HashMap.Strict as M


data VarSymBreakingDescr = VarSymBreakingDescr
    { nodes_to_swap     :: [Text]
    , theModel        :: Model
    }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Serialize VarSymBreakingDescr
instance Hashable  VarSymBreakingDescr
instance ToJSON    VarSymBreakingDescr where toJSON = genericToJSON jsonOptions
instance FromJSON  VarSymBreakingDescr where parseJSON = genericParseJSON jsonOptions

outputVarSymBreaking :: MonadIO m => FilePath -> Model -> m ()
outputVarSymBreaking jsonPath
    = liftIO . writeFile jsonPath
    . renderNormal . transform symmetricChildren . toJSON . varSymBreaking

varSymBreaking :: Model -> VarSymBreakingDescr
varSymBreaking model = VarSymBreakingDescr toSwap model
    where
        toSwap = [ n | Reference (Name n) _ <- universeBi model ]

symmetricChildren :: JSON.Value -> JSON.Value
symmetricChildren (JSON.Object mp) =
    let
        commutative = [ "MkOpEq", "MkOpNeq"
                      , "MkOpSum", "MkOpProduct"
                      ]

        f k (JSON.Object v) = JSON.Object $
            if k `elem` commutative
                then M.insert "symmetricChildren" (JSON.Bool True) v
                else v
        f _ v = v
    in
        JSON.Object (M.mapWithKey f mp)
symmetricChildren v = v
