{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phases.Refn ( callRefn ) where

import Control.Applicative
import Control.Monad ( (<=<), forM, zipWithM_ )
import Control.Monad.Error ( MonadError, runErrorT, throwError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.List ( ListT, runListT )
import Control.Monad.RWS ( evalRWST )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter, tell, WriterT, runWriterT )
import Data.Either ( rights )
import Data.Generics.Uniplate.Direct ( children, descendM, descendBiM)
import Data.Monoid ( Any(..) )

import Has
import Language.Essence
import Language.EssenceEvaluator ( quanDomain )
import MonadList ( MonadList, option )
import Phases.AuxRename ( auxRename )
import Phases.BubbleUp ( bubbleUp )
import Phases.Eval ( evalSpec )
import Phases.InlineQuanGuards ( inlineQuanGuards )
import Phases.QuanRename ( quanRenameSt, quanRenameIO )
import Phases.RemoveUnusedDecls ( removeUnusedDecls )
import Phases.ReprRefnCommon
import Phases.TopLevelAnds ( topLevelAnds )
import Phases.TupleExplode ( tupleExplode )

-- import Language.EssencePrinters ( prExpr )
-- import PrintUtils ( render )


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
                         <=< auxRename
                         <=< quanRenameSt
                         <=< removeUnusedDecls
                         <=< tupleExplode
                         <=< topLevelAnds
                         <=< bubbleUp
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
        <- (runListT . runWriterT) (descendBiM f spec)
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
applyRefnsToExpr bs rules x@(ExprQuantifier{quanVar=Identifier nm, quanOver}) = do
    let bs' = (Quantified,nm,quanDomain quanOver) : bs
    x' <- descendM (applyRefnsToExpr bs' rules) x

    candidates :: [Either ErrMsg [Expr]]
        <- mapM (\ r -> runApplyRefnToExpr bs r x' ) rules
    -- forM_ (lefts candidates) $ (liftIO . putStrLn)
    let results = rights candidates
    case results of [] -> tell (Any False) >> option [x']
                    _  -> tell (Any True ) >> option (concat results)
applyRefnsToExpr bs rules x = do
    x' <- descendM (applyRefnsToExpr bs rules) x
    candidates :: [Either ErrMsg [Expr]]
        <- mapM (\ r -> runApplyRefnToExpr bs r x' ) rules
    -- forM_ (lefts candidates) $ (liftIO . putStrLn)
    let results = rights candidates
    case results of [] -> tell (Any False) >> option [x']
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
    , Has st [Binding]
    ) => RuleRefn -> Expr -> m (Either ErrMsg [Expr])
applyRefnToExpr rule x = do
    -- liftIO $ putStrLn $ padRight ' ' 60 (refnFilename rule) ++ render prExpr x
    result <- runErrorT $ do
        matchExprPattern (refnPattern rule) x
        introduceLocalBindings $ refnBindings rule
        mapM_ (checkWheres <=< instantiateNames) $ refnWheres rule
        mapM instantiateNames' (refnTemplates rule)

    -- liftIO . putStrLn $ "applying " ++ refnFilename rule ++ "\n" ++ ppShow x

    case result of
        Left msg -> do
            -- liftIO $ putStrLn $ "[NOAPPLY] " ++ msg ++ "\n"
            return $ Left msg
        Right xs -> do
            xs' <- liftIO $ quanRenameIO xs
            -- liftIO $ do
            --     putStrLn $ "[APPLIED] " ++ refnFilename rule
            --     putStrLn $ "\t" ++ render prExpr x
            --     -- mapM_ (putStrLn . ("\t"++) . render prExpr) xs
            --     mapM_ (putStrLn . ("\t"++) . render prExpr) xs'
            return $ Right xs'


matchExprPattern ::
    ( Applicative m
    , MonadError ErrMsg m
    , MonadState st m
    , MonadWriter [Log] m
    , MonadIO m
    , Has st [Binding]
    ) => Expr -- the pattern
      -> Expr -- domain to match
      -> m ()
matchExprPattern (Identifier "_") _ = return ()
matchExprPattern (Identifier nm ) x = addBinding InRule nm x
matchExprPattern p@(ExprQuantifier {quanGuard=Just _ })
             x@(ExprQuantifier {quanGuard=Nothing}) = matchExprPattern p x { quanGuard = Just (ValueBoolean True) }
matchExprPattern p x = do
    -- liftIO $ putStrLn $ "matchExprPattern: " ++ render prExpr x
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
                then zipWithM_ matchExprPattern chP chX
                else throwError $ "Different number of children: " ++ ctrP ++ "/" ++ show nmP
                                                          ++ " ~ " ++ ctrX ++ "/" ++ show nmX
        else do
            let msg = "Constructor names do not match: " ++ ctrP ++ " ~ " ++ ctrX
            throwError msg
    -- st <- get
    -- liftIO $ ppPrint st
