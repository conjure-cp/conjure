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
        SizeAttr_None           -> []
        SizeAttr_Size x         -> return [essence| &x =  &cardinality |]
        SizeAttr_MinSize x      -> return [essence| &x <= &cardinality |]
        SizeAttr_MaxSize y      -> return [essence| &cardinality <= &y |]
        SizeAttr_MinMaxSize x y -> return [essence| &x <= &cardinality /\ &cardinality <= &y |]
