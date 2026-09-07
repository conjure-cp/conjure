{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Ordering
    ( orderingIsGlobal, globalOrderingKey, mapGlobalOrderingKey ) where

import Conjure.Prelude
import Conjure.Language

-- A conservative certificate for the complete representation tree.  Keys are
-- fixed-width integer vectors, comparable only within the same domain/layout.
-- Adding a case also changes .< used to sort elements of enclosing collections.
orderingIsGlobal :: Domain HasRepresentation x -> Bool
orderingIsGlobal DomainInt{} = True
orderingIsGlobal (DomainTuple ds) = all orderingIsGlobal ds
orderingIsGlobal (DomainSet Set_Occurrence _ DomainInt{}) = True
orderingIsGlobal (DomainMSet MSet_Occurrence _ DomainInt{}) = True
orderingIsGlobal (DomainSet Set_Explicit (SetAttr SizeAttr_Size{}) d) = orderingIsGlobal d
orderingIsGlobal _ = False

globalOrderingKey :: (MonadFailDoc m, NameGen m)
    => (Expression -> m [Expression])
    -> Expression -> Domain HasRepresentation Expression -> m Expression
globalOrderingKey = mapGlobalOrderingKey id

-- Map scalar coordinates while constructing the key, without introducing
-- expression generators over intermediate (possibly nested) matrices.
mapGlobalOrderingKey :: (MonadFailDoc m, NameGen m)
    => (Expression -> Expression)
    -> (Expression -> m [Expression])
    -> Expression -> Domain HasRepresentation Expression -> m Expression
mapGlobalOrderingKey f down inp domain = case domain of
    DomainInt{} -> let value = f inp in return [essence| [&value] |]
    DomainTuple ds -> do
        xs <- down inp
        keys <- zipWithM (mapGlobalOrderingKey f down) xs ds
        let matrix = fromList keys
        return [essence| flatten(&matrix) |]
    DomainSet Set_Occurrence _ d -> do
        [values] <- down inp
        let index = forgetRepr d
        (iPat, i) <- quantifiedVarOverDomain index
        let value = f [essence| toInt(&values[&i]) |]
        return [essence| [&value | &iPat : &index] |]
    DomainMSet MSet_Occurrence _ d -> do
        [values] <- down inp
        let index = forgetRepr d
        (iPat, i) <- quantifiedVarOverDomain index
        let value = f [essence| &values[&i] |]
        return [essence| [&value | &iPat : &index] |]
    DomainSet Set_Explicit (SetAttr (SizeAttr_Size size)) d
        | orderingIsGlobal d -> do
            [values] <- down inp
            (iPat, i) <- quantifiedVarOverDomain [essenceDomain| int(1..&size) |]
            key <- mapGlobalOrderingKey (\ k -> f [essence| -&k |]) down [essence| &values[&i] |] d
            return [essence| flatten([&key | &iPat : int(1..&size)]) |]
    _ -> na "globalOrderingKey: uncertified representation"
