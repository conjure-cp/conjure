{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.RefineParamSpec where

-- conjure
import Language.E.Definition hiding ( Spec )
import Conjure.RefineParam

-- containers
import Data.Tree ( Tree(..) )

-- hspec
import Test.Hspec ( Spec, describe, it, shouldBe )


spec :: Spec
spec = describe "int parameter" $ do

    it "int parameters stay as is" $ do
        refineSingleParam
            (Node NoRepresentation [])
            ( "x"
            , DomainInt []
            , ConstantInt 1
            ) `shouldBe`
            Right [ ( "x"
                    , DomainInt []
                    , ConstantInt 1
                    ) ]

