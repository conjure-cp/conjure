{-# LANGUAGE OverloadedStrings #-}

module Language.E.Pipeline.Groom where

import Language.E
import Language.E.Pipeline.IntroduceRegions ( removeRegions )
import Language.E.Pipeline.RemoveDuplicateCons ( removeDuplicateCons )
import Language.E.Pipeline.SavileRowCompat ( savilerowCompat )


groomSpec :: MonadConjure m => Spec -> m Spec
groomSpec =  recordSpec "entering groomSpec"
        >=> removeRegions           >=> recordSpec "removeRegions"
        >=> removeDuplicateCons     >=> recordSpec "removeDuplicateCons"
        >=> savilerowCompat         >=> recordSpec "savilerowCompat"

