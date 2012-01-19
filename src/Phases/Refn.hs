{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phases.Refn where

import Control.Applicative
import Control.Monad ( forM_, zipWithM_ )
import Control.Monad.Error ( MonadError, runErrorT, throwError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.State ( MonadState, get, modify, runStateT )
import Control.Monad.Writer ( MonadWriter )
import Control.Monad.RWS ( evalRWST )
import Data.Data ( dataTypeName, dataTypeOf, toConstr )
import Data.Either ( lefts, rights )
import Data.Generics ( Data )
import Data.Generics.Uniplate.Direct ( children )
import Data.Maybe ( fromMaybe )
import Data.Typeable ( cast )

import Language.Essence
import Language.EssenceParsers ( pRuleRefn, pExpr )
import Language.EssencePrinters ( prExpr )
import ParsecUtils
import Phases.ReprRefnCommon
import PrintUtils
import Utils


test :: IO ()
test = do
    r1 <- parseIO (pRuleRefn "Set.Eq.rule") =<< readFile "../rules/refn/Set.Eq.rule"
    -- x  <- parseIO (pExpr <* eof) =<< getLine
    x  <- parseIO (pExpr <* eof) =<< return "12=13"
    ppPrint x
    xs <- applyRefnsToExpr [r1] x
    mapM_ (putStrLn . render prExpr) xs


applyRefnToSpec ::
    ( Applicative m
    , MonadIO m
    ) => [[RuleRefn]] -> Spec -> m [Spec]
applyRefnToSpec _refns spec = do
    return [spec]


applyRefnsToExpr ::
    ( Functor m
    , MonadIO m
    ) => [RuleRefn]
      -> Expr
      -> m [Expr]
applyRefnsToExpr rules x = do
    results :: [Either ErrMsg [Expr]]
        <- mapM (`runApplyRefnToExpr` x) rules
    forM_ (lefts results) $ (liftIO . putStrLn)
    return $ concat $ rights results

runApplyRefnToExpr ::
    ( Functor m
    , MonadIO m
    ) => RuleRefn
      -> Expr
      -> m (Either ErrMsg [Expr])
runApplyRefnToExpr r x = fst <$> evalRWST (applyRefnToExpr r x) () []

applyRefnToExpr ::
    ( MonadState [Binding] m
    , MonadWriter [Log] m
    , MonadIO m
    ) => RuleRefn -> Expr -> m (Either ErrMsg [Expr])
applyRefnToExpr rule x = runErrorT $ do
    matchPattern (refnPattern rule) x
    forM_ (refnBindings rule) $ \ (_,nm,x) -> addBinding InRule nm x
    mapM_ checkWheres $ refnWheres rule
    mapM instantiateNames (refnTemplates rule)


matchPattern ::
    ( MonadError ErrMsg m
    , MonadState [Binding] m
    , MonadWriter [Log] m
    , MonadIO m
    ) => Expr -- the pattern
      -> Expr -- domain to match
      -> m ()
matchPattern (Identifier "_") _ = return ()
matchPattern (Identifier nm ) x = addBinding InRule nm x
matchPattern p x =
    let
        ctrP = exprTag p
        ctrX = exprTag x
    in
        if ctrP == ctrX
            then zipWithM_ matchPattern (children p) (children x)
            else do
                let msg = "Constructor names do not match: " ++ ctrP ++ " ~ " ++ ctrX
                throwError msg

addBinding :: MonadState [Binding] m => BindingEnum -> String -> Expr -> m ()
addBinding e nm x = modify ((e,nm,x) :)


----------------------------------------
-- helper functions here ---------------
----------------------------------------

typeName :: Data a => a -> String
typeName = dataTypeName . dataTypeOf

constrName :: Data a => a -> String
constrName x = typeName x ++ "#" ++ show (toConstr x)

asTypeOf :: (Data a, Data to) => a -> to -> to
asTypeOf x to = flip fromMaybe (cast x)
              $ error $ "asTypeOf: cannot cast " ++ typeName x ++ " to " ++ typeName to
