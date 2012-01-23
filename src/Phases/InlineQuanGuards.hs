{-# LANGUAGE FlexibleContexts #-}

module Phases.InlineQuanGuards where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Generics.Uniplate.Direct ( transformBiM )

import Language.Essence


inlineQuanGuards :: (Applicative m, MonadIO m) => Spec -> m Spec
inlineQuanGuards spec = do
    constraints'    <- transformBiM f $ constraints    spec
    objective'      <- transformBiM f $ objective      spec
    topLevelWheres' <- transformBiM f $ topLevelWheres spec
    return spec { constraints    = constraints'
                , objective      = objective'
                , topLevelWheres = topLevelWheres'
                }
    where
        -- f :: Expr -> m Expr
        f p@(ExprQuantifier {quanName=Identifier qnName, quanGuard=Just g, quanBody=qnBody})
            = case [ glueOp
                   | (Letting,nm,DeclQuantifier _ glueOp _) <- topLevelBindings spec
                   , nm == qnName
                   ] of
                [l] -> do
                    -- liftIO $ putStrLn $ "inlineQuanGuards: " ++ qnName
                    return p { quanGuard = Nothing
                             , quanBody  = GenericNode Image [l,ValueTuple [g,qnBody]]
                             }
                _   -> error $ "Unknown quantifier: " ++ qnName
        f x = return x
