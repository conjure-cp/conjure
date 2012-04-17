{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#define DATADIR "datafiles/"

module Constants ( figlet
                 , FreshName, getFreshName, newRuleVar, isFreshName
                 , mkFreshNames, mkPrettyFreshNames
                 , trace, traceM, TraceEnum(..)
                 ) where

import Control.DeepSeq ( deepseq )
import Control.Monad.Error ( Error(..) )
import Control.Monad.State ( MonadState )
import Data.ByteString.Char8 ( unpack)
import Data.FileEmbed ( embedFile )
import Data.List ( (\\), isPrefixOf )
import qualified Debug.Trace as D

import Has
import PrintUtils ( Doc, text )



{-# INLINE trace #-}
trace :: TraceEnum -> String -> a -> a
trace Debug s = D.trace ("[Debug] " ++ s)
-- trace Parsing s = D.trace ("[Parsing] " ++ s)
trace _       _ = id

{-# INLINE traceM #-}
traceM :: Monad m => TraceEnum -> String -> m ()
traceM e s = trace e s $ return ()

data TraceEnum = Parsing | PatternMatching | TypeChecking | Debug



figlet :: String
figlet = unpack $(embedFile (DATADIR ++ "conjure.figlet"))


newRuleVar :: String -> String
newRuleVar = (ruleVarPrefix ++)

ruleVarPrefix :: String
ruleVarPrefix = "__INRULE_"

newtype FreshName = FreshName String

mkFreshNames :: [String] -> [FreshName]
mkFreshNames used = used `deepseq` map FreshName (freshNames \\ used)
    where freshNames = [ "__" ++ show i | i <- [ (1 :: Integer) .. ] ]

mkPrettyFreshNames :: [String] -> [FreshName]
mkPrettyFreshNames used = used `deepseq` map FreshName (freshNames \\ used)
    where freshNames = words "i j k l" ++ [ "q" ++ show i | i <- [ (1 :: Integer) .. ] ]

getFreshName :: (MonadState st m, Has st [FreshName]) => m String
getFreshName = do
    (FreshName n:ns) <- getM
    putM ns
    return n

isFreshName :: String -> Bool
isFreshName s = not (ruleVarPrefix `isPrefixOf` s) && "__" `isPrefixOf` s


instance Error Doc where
    strMsg = text
