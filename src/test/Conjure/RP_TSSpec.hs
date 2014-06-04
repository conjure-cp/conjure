{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.RP_TSSpec ( spec ) where

-- conjure
import Conjure.Language.Definition hiding ( Spec )
import Conjure.RefineParam ( refineSingleParam )
import Conjure.TranslateSolution ( translateSingleSolution )
import Language.E.Imports
import Language.E.Pretty

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
-- import Test.QuickCheck ( property )


allTheWay
    :: Text
    -> Domain Constant
    -> Constant
    -> Tree Representation
    -> Spec
allTheWay name domain constant representation =
    it (show $ "allTheWay" <+> pretty domain <+> pretty constant <+> pretty representation) $
        let
            result = do
                low <- refineSingleParam representation (name, domain, constant)
                translateSingleSolution name domain representation [ (n,c) | (n,_,c) <- low ]
        in
            result `shouldBe` Right (name, constant)


spec :: Spec
spec = describe "RefineParam & TranslateSolution" $ do

    allTheWay
        "x"
        (DomainInt [RangeBounded (ConstantInt 1) (ConstantInt 9)])
        (ConstantInt 3)
        (Node NoRepresentation [])


