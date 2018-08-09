module Conjure.UI.VarSymBreaking ( outputVarSymBreaking ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.AdHoc
import Conjure.Language.Pretty

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


outputVarSymBreaking :: MonadIO m => FilePath -> Model -> m ()
outputVarSymBreaking jsonPath = liftIO . writeFile jsonPath . renderNormal . varSymBreaking

varSymBreaking :: Model -> JSON.Value
varSymBreaking model = JSON.Object $ M.fromList
    [ ("nodes_to_swap", JSON.Array $ V.fromList $ map JSON.String $ sortNub
            $  [ n | Reference (Name n) _ <- universeBi model ]
            ++ [ n | Single    (Name n)   <- universeBi model ]
            )
    , ("model", varSymBreakingDescription model)
    ]
