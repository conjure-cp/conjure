{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Common where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH


mkSizeCons :: SizeAttr Expression -> Expression -> [Expression]
mkSizeCons sizeAttr cardinality =
    case sizeAttr of
        SizeAttrNone           -> []
        SizeAttrSize x         -> return [essence| &x =  &cardinality |]
        SizeAttrMinSize x      -> return [essence| &x <= &cardinality |]
        SizeAttrMaxSize y      -> return [essence| &cardinality <= &y |]
        SizeAttrMinMaxSize x y -> return [essence| &x <= &cardinality /\ &cardinality <= &y |]
