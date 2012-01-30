{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phases.Refn ( callRefn ) where

import Control.Applicative
import Control.Monad ( (<=<), forM, forM_, void, zipWithM_ )
import Control.Monad.Error ( MonadError, runErrorT, throwError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.List ( ListT, runListT )
import Control.Monad.RWS ( evalRWST )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter, tell, WriterT, runWriterT )
import Data.Either ( rights )
import Data.Generics.Uniplate.Direct ( children, transformBiM )
import Data.Monoid ( Any(..) )

import Language.Essence
import Language.EssencePrinters ( prExpr )
import MonadList ( MonadList, option )
import Phases.Eval ( evalSpec )
import Phases.InlineQuanGuards ( inlineQuanGuards )
import Phases.QuanRename ( quanRenameSt, quanRenameIO )
import Phases.RemoveUnusedDecls ( removeUnusedDecls )
import Phases.ReprRefnCommon ( ErrMsg, checkWheres, instantiateNames )
import Phases.TopLevelAnds ( topLevelAnds )
import Phases.TupleExplode ( tupleExplode )
import PrintUtils ( render )
import Utils ( ppShow )
import Has


-- test :: IO ()
-- test = do
--     r1 <- parseIO (pRuleRefn "Set.Eq.rule") =<< readFile "../rules/refn/Set.Eq.rule"
--     r2 <- parseIO (pRuleRefn "MSet.Eq.rule") =<< readFile "../rules/refn/MSet.Eq.rule"
--     x  <- parseIO (pExpr <* eof) =<< getLine
--     -- x  <- parseIO (pExpr <* eof) =<< return "12=13"
--     ppPrint x
--     xs <- runListT $ applyRefnsToExpr [] [r1,r2] x
--     mapM_ (putStrLn . render prExpr) xs

callRefn :: [RuleRefn] -> Spec -> IO [Spec]
callRefn refns spec = mapM ( evalSpec
                         <=< inlineQuanGuards
                         <=< quanRenameSt
                         <=< removeUnusedDecls
                         <=< tupleExplode
                         <=< topLevelAnds
                         )
                         =<< applyRefnsToSpec refns spec

applyRefnsToSpec :: forall m .
    ( Applicative m
    , MonadIO m
    ) => [RuleRefn] -> Spec -> m [Spec]
applyRefnsToSpec refns spec = do
    let
        f :: Expr -> WriterT Any (ListT m) Expr
        f = applyRefnsToExpr (topLevelBindings spec) refns
    results :: [(Spec,Any)]
        <- (runListT . runWriterT) (transformBiM f spec)
    t :: [[Spec]]
        <- forM results $ \ (sp,a) -> do
            sp' <- evalSpec sp
            if getAny a
                then applyRefnsToSpec refns sp'
                else return [sp']
    return $ concat t


applyRefnsToExpr ::
    ( Applicative m
    , MonadIO m
    , MonadList m
    , MonadWriter Any m
    ) => [Binding]
      -> [RuleRefn]
      -> Expr
      -> m Expr
applyRefnsToExpr bs rules x = do
    candidates :: [Either ErrMsg [Expr]]
        <- mapM (\ r -> runApplyRefnToExpr bs r x ) rules
    -- forM_ (lefts candidates) $ (liftIO . putStrLn)
    let results = rights candidates
    case results of [] -> tell (Any False) >> option [x]
                    _  -> tell (Any True ) >> option (concat results)

runApplyRefnToExpr ::
    ( Applicative m
    , MonadIO m
    ) => [Binding]
      -> RuleRefn
      -> Expr
      -> m (Either ErrMsg [Expr])
runApplyRefnToExpr bs r x = fst <$> evalRWST (applyRefnToExpr r x) () bs


applyRefnToExpr ::
    ( Applicative m
    , MonadState st m
    , MonadWriter [Log] m
    , MonadIO m
    , Has st ([Binding])
    ) => RuleRefn -> Expr -> m (Either ErrMsg [Expr])
applyRefnToExpr rule x = do
    -- liftIO $ putStrLn $ padRight ' ' 60 (refnFilename rule) ++ render prExpr x
    result <- runErrorT $ do
        matchPattern (refnPattern rule) x
        forM_ (refnBindings rule) $ \ (_,nm,t) -> addBinding InRule nm t
        mapM_ (checkWheres <=< instantiateNames) $ refnWheres rule
        mapM instantiateNames (refnTemplates rule)
    let
        -- pr = liftIO . putStrLn
        pr = void . return
    -- pr $ "applying " ++ refnFilename rule ++ "\n" ++ ppShow x
    case result of
        Left msg -> do
            -- pr $ "[NOAPPLY] " ++ msg ++ "\n"
            return ()
        Right xs -> do
            pr $ "[APPLIED] " ++ refnFilename rule
            pr $ "\t" ++ render prExpr x
            -- mapM_ (pr . ("\t"++) . render prExpr) xs
            return ()
    case result of
        Left msg -> return $ Left msg
        Right xs -> do
            xs' <- liftIO $ quanRenameIO xs
            mapM_ (pr . ("\t"++) . render prExpr) xs'
            return $ Right xs'


matchPattern ::
    ( Applicative m
    , MonadError ErrMsg m
    , MonadState st m
    , MonadWriter [Log] m
    , MonadIO m
    , Has st [Binding]
    ) => Expr -- the pattern
      -> Expr -- domain to match
      -> m ()
matchPattern (Identifier "_") _ = return ()
matchPattern (Identifier nm ) x = addBinding InRule nm x
matchPattern p@(ExprQuantifier {quanGuard=Just _ })
             x@(ExprQuantifier {quanGuard=Nothing}) = matchPattern p x { quanGuard = Just (ValueBoolean True) }
matchPattern p x = do
    -- liftIO $ putStrLn $ "matchPattern: " ++ render prExpr x
    let
        ctrP = exprTag p
        ctrX = exprTag x
    if ctrP == ctrX
        then do
            let
                chP = children p
                nmP = length chP
                chX = children x
                nmX = length chX
            if nmP == nmX
                then zipWithM_ matchPattern chP chX
                else throwError $ "Different number of children: " ++ ctrP ++ "/" ++ show nmP
                                                          ++ " ~ " ++ ctrX ++ "/" ++ show nmX
        else do
            let msg = "Constructor names do not match: " ++ ctrP ++ " ~ " ++ ctrX
            throwError msg
    -- st <- get
    -- liftIO $ ppPrint st

addBinding ::
    ( Applicative m
    , MonadState st m
    , Has st [Binding]
    ) => BindingEnum -> String -> Expr -> m ()
addBinding e nm x = modifyM $ ((e,nm,x) :)

