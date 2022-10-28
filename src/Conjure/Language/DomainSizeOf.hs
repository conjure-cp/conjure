module Conjure.Language.DomainSizeOf
    ( DomainSizeOf(..)
    , enumNameToInt, enumIntToName
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Name
import Conjure.Language.Domain
import Conjure.Language.Pretty


class DomainSizeOf x res where
    domainSizeOf ::
        ( MonadFailDoc m
        , Pretty r
        , Default r
        ) => Domain r x -> m res

enumNameToInt :: [Name] -> Name -> Int
enumNameToInt nms nm = case elemIndex nm nms of
    Nothing -> bug $ vcat [ pretty nm <+> "is not a value of this enumerated type."
                          , "Values are:" <+> prettyList id "," (map pretty nms)
                          ]
    Just i  -> i + 1

enumIntToName :: [Name] -> Int -> Name
enumIntToName nms i = at nms (i-1)

