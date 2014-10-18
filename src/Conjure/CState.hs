{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Conjure.CState where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.DomainOf
import Conjure.Language.TypeOf
import Conjure.Language.Pretty


data CState = CState
    { stNbExpression :: !Int
    , stReprsSoFar :: [ ( Name                                        -- for the declaration with this name
                        , ( Int                                       -- number of occurrences so far
                          , [Domain HasRepresentation Expression]     -- distinct reprs so far
                          ) ) ]
    , stAscendants :: [Either Expression Statement]
    , stCurrInfo :: !ModelInfo
    , stAllReprs :: [(Name, Domain HasRepresentation Expression)]     -- repr lookup, including *ALL* levels
    , stPastInfos :: [[Decision]]                                     -- each [Decision] is a trail of decisions
    , stExhausted :: Bool
    }
    deriving Show

instance Default CState where
    def = CState 0 [] [] def [] [] False

instance TypeOf CState Expression where
    typeOf x = do
        st <- gets stAllReprs
        evalStateT (typeOf x) st

instance DomainOf CState HasRepresentation Expression Expression where
    domainOf _ (Reference _ (Just dom)) = return dom
    domainOf _ x = bug ("domainOf{Expression}:" <+> pretty x)

instance DomainOf CState () Expression Expression where
    -- domainOf _ (Reference nm Nothing) =
    domainOf _ (Reference _ (Just dom)) = return (forgetRepr dom)
    domainOf _ x = bug ("domainOf{Expression}:" <+> pretty x)
