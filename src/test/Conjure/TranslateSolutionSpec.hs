{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.TranslateSolutionSpec where

-- conjure
import Conjure.Language.Definition hiding ( Spec )
import Conjure.TranslateSolution

-- base
-- import Control.Monad ( forM_ )
-- import Data.Monoid ( mappend )

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )

-- QuickCheck
-- import Test.QuickCheck ( property )

-- text
-- import Data.Text ( pack )


spec :: Spec
spec = describe "translating solutions" $ do

    it "int constants stay as is" $ do
        translateSingleSolution
            (DomainInt [])
            (Node NoRepresentation []) 
            [("x", DomainInt [], ConstantInt 1)] `shouldBe`
            Right ("x", DomainInt [], ConstantInt 1)

    it "tuples" $ do
        translateSingleSolution
            (DomainTuple [DomainInt [], DomainBool])
            (Node NoRepresentation [ Node NoRepresentation []
                                   , Node NoRepresentation []
                                   ]) 
            [ ("x_1", DomainInt [], ConstantInt 1)
            , ("x_2", DomainBool, ConstantBool False)
            ] `shouldBe`
            Right ("x"
                  , DomainTuple [DomainInt [], DomainBool]
                  , ConstantTuple [ConstantInt 1, ConstantBool False]
                  )

