{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phases.Repr ( callRepr ) where

import Control.Applicative
import Control.Arrow ( (&&&), second )
import Control.Monad ( (<=<), forM, replicateM )
import Control.Monad.Error ( MonadError, throwError, runErrorT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.RWS ( MonadWriter, tell, MonadState, get, put, evalRWST )
import Data.Either ( rights )
import Data.Generics.Uniplate.Direct ( transform, transformBi, transformBiM, universeBi )
import Data.List ( group, nub, sort )
import Data.Maybe ( catMaybes, maybeToList, isNothing, fromMaybe )
import qualified Data.Map as M

import Language.Essence
import Language.EssencePrinters ( prExpr, prBinding )
import MonadInterleave ( MonadInterleave )
import Phases.Eval ( evalSpec )
import Phases.QuanRename ( quanRenameSt )
import Phases.ReprRefnCommon
import PrintUtils
import UniqueSupply ( nextUniqueInt )
import Utils ( allPairs, runRWSET )


-- test :: IO ()
-- test = do
--     s <- parseIO pSpec =<< readFile "../testdata/has-sets.essence"
--     r1 <- parseIO (pRuleRepr "r1") =<< readFile "../rules/repr/Set.Explicit.repr"
--     r2 <- parseIO (pRuleRepr "r2") =<< readFile "../rules/repr/Set.ExplicitVarSize.repr"
--     r3 <- parseIO (pRuleRepr "r3") =<< readFile "../rules/repr/Set.Occurrence.repr"
--     putStrLn $ render prSpec s
--     print =<< appl s [r1,r2,r3]
-- 
-- appl :: Spec -> [RuleRepr] -> IO (Either ErrMsg [Spec])
-- appl s rs = runErrorT $ applyToSpec rs s



cross :: [a] -> Int -> [[a]]
cross xs n = replicateM n xs


callRepr :: [RuleRepr] -> Spec -> IO [Spec]
callRepr reprs spec = do
    temp <- runErrorT (applyToSpec reprs spec)
    case temp of
        Left err -> error err
        Right ss -> return ss


applyToSpec ::
    ( Applicative m
    , MonadIO m
    , MonadInterleave m
    , MonadError ErrMsg m
    ) => [RuleRepr] -> Spec -> m [Spec]
applyToSpec reprs spec = do
    let
        -- all top level bindings
        bindings :: [Binding]
        bindings  = topLevelBindings spec

        matrNeedsRepr :: Expr -> (Bool, Maybe Representation)
        matrNeedsRepr (DomainMatrix {element}) = matrNeedsRepr element
        matrNeedsRepr x = (needsRepresentation x, representation x)

        -- those bindings which need a representation
        bindingsNeedsRepr :: [Binding]
        bindingsNeedsRepr = [ b
                            | b@(ty,_,x) <- bindings
                            , ty `elem` [Find, Given]
                            , let (boolean,m) = matrNeedsRepr x
                            , boolean
                            , isNothing m
                            ]

        -- number of occurrences of those bindings in "bindingsNeedsRepr" in the rest of the spec
        counts :: [(String, Int)]
        counts = map (head &&& length)
               . group
               . sort
               $ [ nm1
                 | Identifier nm1 <- universeBi spec { topLevelBindings = [] }
                 , (_,nm2,_)      <- bindingsNeedsRepr
                 , nm1 == nm2
                 ]

        -- rest of bindings
        -- bindingsRest :: [Binding]
        -- bindingsRest = bindings \\ bindingsNeedsRepr

    if null bindingsNeedsRepr
        then return []
        else do
            liftIO $ do
                putStrLn "Choosing representations for the following: "
                mapM_ (putStrLn . render (prBinding [])) bindingsNeedsRepr

            -- apply repr rules to every binding that's in "bindingsNeedsRepr". might fail.
            applied' :: [(Binding, (Either ErrMsg [ReprResult], [Log]))]
                <- zip bindingsNeedsRepr <$> sequence [ liftIO (applyAllToDom (topLevelBindings spec) d reprs)
                                                      | (_,_,d) <- bindingsNeedsRepr
                                                      ]

            -- calls error if repr application fails for any binding.
            applied :: [(Binding, [ReprResult])]
                <- forM applied' $ \ t -> case t of (_, (Left err, _logs)) -> {- liftIO (ppPrint logs) >> -} throwError err
                                                    (b, (Right r , _logs)) -> return (b,r)

            let
                foo :: [(a,[b])] -> [[(a,b)]]
                foo [] = [[]]
                foo ((x,ys):qs) = concat [ [ (x,y) : ws | y <- ys ] |  ws <- foo qs ]

                -- a lookup table contains a list of repr rule application results for each identifier in the model.
                -- there are as many lookup tables as the number of alternative models.
                lookupTables :: [ M.Map String [( Binding           -- the old binding
                                                , ReprName          -- the new representation name
                                                , Domain            -- the new representation domain
                                                , Structural        -- the structural constraint to be posted
                                                )]
                                ]
                lookupTables = map M.fromList $ foo [ (nm, cross [ (b,x,y,z) | (x,y,z) <- rrs ] cnt)
                                                    | (b@(_,nm,_), rrs) <- applied
                                                    , let cnt = fromMaybe 0 (lookup nm counts)
                                                    , cnt > 0
                                                    ]

            -- liftIO $ ppPrint lookupTables
            -- return []

            results <- forM lookupTables $ \ table -> applyConfigToSpec table spec
            mapM (quanRenameSt <=< evalSpec) results


applyConfigToSpec ::
    ( MonadIO m
    , MonadError ErrMsg m
    ) => M.Map String [(Binding,ReprName,Domain,Structural)]
      -> Spec
      -> m Spec
applyConfigToSpec mp sp = do
    let
        forChannels :: [(String,String)]
        forChannels = concat [ allPairs [ nm ++ "#" ++ r | r <- rs ]
                             | ( nm, ls ) <- M.toList mp
                             , let rs = nub [ a | (_,a,_,_) <- ls ]
                             , length rs > 1
                             ]
        channels :: [Expr]
        channels = [ GenericNode Eq [Identifier i, Identifier j]
                   | (i,j) <- forChannels
                   ]
    (resultSp,(md,bs,ss)) <- evalRWST (transformBiM f sp) () mp
    return resultSp { topLevelBindings = nub $ topLevelBindings resultSp ++ bs
                    , constraints      = nub $ constraints resultSp ++ channels ++ catMaybes ss
                    , metadata         = nub $ metadata resultSp ++ md
                    }
    where
        f :: ( MonadIO m
             , MonadError ErrMsg m
             , MonadState (M.Map String [(Binding,ReprName,Domain,Structural)]) m
             , MonadWriter ([Metadata],[Binding],[Structural]) m
             ) => Expr -> m Expr
        f p@(Identifier nm) = do
            luMap <- get
            case M.lookup nm luMap of
                Nothing -> return p
                Just [] -> throwError $ "applyConfigToSpec: empty list in state for " ++ nm
                Just ((oldB@(bEnum,_,_),rn,rd,rs):rest) -> do
                    put (M.insert nm rest luMap)
                    let newNmHash = nm ++ "#" ++ rn
                    let newNmUnderscore = nm ++ "_" ++ rn
                    let
                        -- replace "refn" with newNm
                        rs' = transformBi (\ t -> case t of Identifier "refn" -> Identifier newNmUnderscore; _ -> t ) rs
                    tell ( [ Represented oldB (bEnum,newNmUnderscore,rd) ]
                         , [ (bEnum,newNmUnderscore,rd)                  ]
                         , [ rs'                                         ]
                         )
                    return $ Identifier newNmHash
        f p = return p


applyAllToDom :: [Binding] -> Domain -> [RuleRepr] -> IO (Either ErrMsg [ReprResult], [Log])
applyAllToDom binds dom reprs = do
    -- results :: [Either ErrMsg (String, Domain, Structural)]
    -- logs    :: [Log]
    (results, logs) <- second concat . unzip <$> sequence [ runApplyToDom binds dom r | r <- reprs ]

    return $ case rights results of
        [] -> (Left ("No repr rule matches this domain: " ++ render prExpr dom), logs)
        rs -> (Right rs, logs)


runApplyToDom :: [Binding] -> Domain -> RuleRepr -> IO (Either ErrMsg (String, Domain, Structural), [Log])
runApplyToDom binds dom repr = runRWSET () binds $ applyToDom dom repr


applyToDom ::
    ( Applicative m
    , MonadError ErrMsg m
    , MonadWriter [Log] m
    , MonadState [Binding] m
    , MonadIO m
    ) => Domain
      -> RuleRepr
      -> m (String, Domain, Structural)
applyToDom (DomainMatrix ind dom) repr = do
    (nm,newdom,str) <- applyToDom dom repr
    qnVar <- (\ i -> "UQ_" ++ show i ) <$> liftIO nextUniqueInt
    let str' = case str of Nothing -> Nothing
                           Just jstr -> Just $ ExprQuantifier (Identifier "forall") (Identifier qnVar) ind Nothing
                                             $ transform (\ t -> case t of Identifier "refn" -> GenericNode Index [Identifier "refn", Identifier qnVar]; _ -> t ) jstr
    return (nm, DomainMatrix ind newdom, str')
applyToDom dom repr = do
    results <- mapM (applyCaseToDom dom) cases
    result :: Maybe Structural <- firstRight results
    case result of
        Nothing -> throwError "Cannot apply repr rule to domain."
        Just r  -> do
            b <- instantiateNames $ reprTemplate repr
            c <- case r of Nothing -> return Nothing; Just i -> do i' <- instantiateNames i; return (Just i')
            return (reprName repr, b, c)
    where
        cases :: [RuleReprCase]
        cases = flip map (reprCases repr) $ \ c -> c { reprCaseStructural = case maybeToList (reprCaseStructural c) ++ maybeToList (reprPrologueStructural repr) of
                                                                                [] -> Nothing
                                                                                t  -> Just (conjunct t)
                                                     , reprCaseWheres     = reprCaseWheres   c ++ reprPrologueWheres   repr
                                                     , reprCaseBindings   = reprCaseBindings c ++ reprPrologueBindings repr
                                                     }


applyCaseToDom :: forall m .
    ( Applicative m
    , MonadWriter [Log] m
    , MonadState [Binding] m
    , MonadIO m
    ) => Domain -> RuleReprCase -> m (Either ErrMsg Structural)
applyCaseToDom dom reprcase = do
    -- liftIO $ do
    --     putStrLn $ "applyCaseToDom"
    --     putStrLn $ "\t" ++ render prExpr dom
    --     putStrLn $ "\t" ++ render prExpr (reprCasePattern reprcase)
    --     putStrLn $ ""
    -- st <- get; liftIO $ print $ map snd3 st
    res <- runErrorT $ do
        matchDomainPattern (reprCasePattern reprcase) dom
        introduceLocalBindings $ reprCaseBindings reprcase
        mapM_ checkWheres $ reprCaseWheres reprcase
        return $ reprCaseStructural reprcase
    -- liftIO $ do
    --     case res of
    --         Left msg -> do
    --             putStrLn $ "Repr no match reason: "
    --             putStrLn $ msg
    --         Right _  -> return ()
    return res


------------------------------------------------------------
-- helper functions ----------------------------------------
------------------------------------------------------------

conjunct :: [Expr] -> Expr
conjunct [] = ValueBoolean True
conjunct [x] = x
conjunct (x:xs) = GenericNode And [x,conjunct xs]

firstRight :: MonadWriter [Log] m => [Either Log r] -> m (Maybe r)
firstRight []            = return Nothing
firstRight (Right r :_ ) = return (Just r)
firstRight (Left err:es) = tell [err] >> firstRight es
