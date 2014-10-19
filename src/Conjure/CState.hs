{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Conjure.CState where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition


data CState = CState
    { stNbExpression :: !Int
    , stReprsSoFar :: [ ( Name                                        -- for the declaration with this name
                        , ( Int                                       -- number of occurrences so far
                          , [Domain HasRepresentation Expression]     -- distinct reprs so far
                          ) ) ]
    , stAscendants :: [Either Expression Statement]
    , stCurrInfo :: !ModelInfo
    , stAllReprs :: [(Name, Domain HasRepresentation Expression)]     -- repr lookup, including *ALL* levels
    }
    deriving Show

instance Default CState where
    def = CState def def def def def

