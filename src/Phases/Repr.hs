{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phases.Repr ( callRepr ) where

import Control.Applicative
import Control.Arrow ( second )
import Control.Monad ( (<=<), forM, forM_, zipWithM_ )
import Control.Monad.Error ( MonadError, throwError, runErrorT )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.RWS ( MonadWriter, tell, MonadState, get, put, modify, evalRWST )
import Data.Either ( rights )
import Data.Generics.Uniplate.Direct ( transformBi, transformBiM, universeBi )
import Data.List ( group, nub, sort )
import Data.Maybe ( catMaybes, maybeToList )
import qualified Data.Map as M

import Language.Essence
import Language.EssencePrinters ( prExpr )
import MonadInterleave ( MonadInterleave )
import Phases.Eval ( evalSpec )
import Phases.QuanRename ( quanRenameSt )
import Phases.ReprRefnCommon
import PrintUtils
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


type Domain = Expr
type Structural = Maybe Expr
type ReprName = String
type ReprResult = (ReprName, Domain, Structural)


cross :: [a] -> Int -> [[a]]
cross xs n = sequence $ replicate n xs


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

        -- those bindings which need a representation
        bindingsNeedsRepr :: [Binding]
        bindingsNeedsRepr = [ b
                            | b@(ty,_,x) <- bindings
                            , ty `elem` [Find, Given]
                            , needsRepresentation x
                            , representation x == Nothing
                            ]
                         ++ [ b
                            | b@(ty,_,DomainMatrix {element=x}) <- bindings
                            , ty `elem` [Find, Given]
                            , needsRepresentation x
                            , representation x == Nothing
                            ]

        -- number of occurrences of those bindings in "bindingsNeedsRepr" in the rest of the spec
        counts :: [(String, Int)]
        counts = map (\ xs -> (head xs, length xs) )
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

    -- apply repr rules to every binding that's in "bindingsNeedsRepr". might fail.
    applied' :: [(Binding, (Either ErrMsg [ReprResult], [Log]))]
        <- zip bindingsNeedsRepr <$> sequence [ liftIO (applyAllToDom d reprs) | (_,_,d) <- bindingsNeedsRepr ]

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
                                            , let cnt = case lookup nm counts of
                                                            Nothing -> 0 -- error ("in applyToSpec.lookupTables: " ++ nm)
                                                            Just c  -> c
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
        forChannels = concat $ [ allPairs [ nm ++ "#" ++ r | r <- rs ]
                               | ( nm, ls ) <- M.toList mp
                               , let rs = nub [ a | (_,a,_,_) <- ls ]
                               , length rs > 1
                               ]
        channels :: [Expr]
        channels = [ GenericNode Eq [Identifier i, Identifier j]
                   | (i,j) <- forChannels
                   ]
    (resultSp,(md,bs,ss)) <- evalRWST (transformBiM f sp) () mp
    return resultSp { topLevelBindings = nub $ bs ++ topLevelBindings resultSp
                    , constraints      = nub $ catMaybes ss ++ constraints resultSp ++ channels
                    , metadata         = nub $ md ++ metadata resultSp
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


applyAllToDom :: Domain -> [RuleRepr] -> IO (Either ErrMsg [ReprResult], [Log])
applyAllToDom dom reprs = do
    -- results :: [Either ErrMsg (String, Domain, Structural)]
    -- logs    :: [Log]
    (results, logs) <- second concat . unzip <$> sequence [ runApplyToDom dom r | r <- reprs ]
    
    return $ case rights results of
        [] -> (Left ("No repr rule matches this domain: " ++ render prExpr dom), logs)
        rs -> (Right rs, logs)


runApplyToDom :: Domain -> RuleRepr -> IO (Either ErrMsg (String, Domain, Structural), [Log])
runApplyToDom dom repr = runRWSET () [] $ applyToDom dom repr


applyToDom ::
    ( Applicative m
    , MonadError ErrMsg m
    , MonadWriter [Log] m
    , MonadState [Binding] m
    , MonadIO m
    ) => Domain
      -> RuleRepr
      -> m (String, Domain, Structural)
applyToDom dom repr = do
    results <- mapM (applyCaseToDom dom) cases
    result :: Maybe Structural <- firstRight results
    case result of
        Nothing -> throwError "Cannot apply repr rule to domain."
        Just r  -> do
            b <- instantiateNames $ reprTemplate repr
            c <- instantiateNames r
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
applyCaseToDom dom reprcase = runErrorT $ do
    matchPattern (reprCasePattern reprcase) dom
    forM_ (reprCaseBindings reprcase) $ \ (_,nm,x) -> addBinding InRule nm x
    mapM_ checkWheres $ reprCaseWheres reprcase
    return $ reprCaseStructural reprcase


matchPattern ::
    ( MonadError ErrMsg m
    , MonadState [Binding] m
    ) => Domain -- the pattern
      -> Domain -- domain to match
      -> m ()

matchPattern (Identifier "_") _ = return ()

matchPattern (Identifier nm) dom = addBinding InRule nm dom

matchPattern DomainBoolean
             DomainBoolean
             = return ()

matchPattern (DomainIntegerFromTo Nothing Nothing)
             (DomainIntegerFromTo Nothing Nothing)
             = return ()
matchPattern (DomainIntegerFromTo Nothing (Just t1))
             (DomainIntegerFromTo Nothing (Just t2))
             = matchPattern t1 t2
matchPattern (DomainIntegerFromTo (Just f1) Nothing)
             (DomainIntegerFromTo (Just f2) Nothing)
             = matchPattern f1 f2
matchPattern (DomainIntegerFromTo (Just f1) (Just t1))
             (DomainIntegerFromTo (Just f2) (Just t2))
             = matchPattern f1 f2 >> matchPattern t1 t2

matchPattern p@(DomainIntegerList xs)
             v@(DomainIntegerList ys)
             = if length xs == length ys
                 then zipWithM_ matchPattern xs ys
                 else matchPatternError p v

matchPattern (DomainUnnamed s1 _)
             (DomainUnnamed s2 _)
             = matchPattern s1 s2

matchPattern (DomainMatrix i1 e1)
             (DomainMatrix i2 e2)
             = matchPattern i1 i2 >> matchPattern e1 e2

matchPattern p@(DomainTuple xs _)
             v@(DomainTuple ys _)
             = if length xs == length ys
                 then zipWithM_ matchPattern xs ys
                 else matchPatternError p v

matchPattern (DomainSet s1 mns1 mxs1 dontcare e1 _)
             (DomainSet s2 mns2 mxs2 _        e2 _)
             = matchIfJust dontcare s1 s2 >>
               matchIfJust dontcare mns1 mns2 >>
               matchIfJust dontcare mxs1 mxs2 >>
               matchPattern e1 e2

-- TODO: add rest of the domains here!

matchPattern pattern value = matchPatternError pattern value


matchPatternError ::
    ( MonadError ErrMsg m
    , MonadState [Binding] m
    ) => Domain -> Domain -> m ()
matchPatternError pattern value = do
    pattern' <- maybe (throwError ("cannot render: " ++ show pattern)) return $ prExpr pattern
    value'   <- maybe (throwError ("cannot render: " ++ show value  )) return $ prExpr value
    throwError . renderDoc $ text "matchPattern:" <+> pattern'
                                               <+> text "~~"
                                               <+> value'


matchIfJust ::
    ( MonadError [Char] m
    , MonadState [Binding] m
    ) => Bool
      -> Maybe Domain
      -> Maybe Domain
      -> m ()
matchIfJust _     Nothing  Nothing   = return ()
matchIfJust _     (Just i) (Just j)  = matchPattern i j
matchIfJust True  Nothing  _         = return ()
matchIfJust False Nothing  (Just {}) = throwError "missing attribute in pattern."
matchIfJust _     (Just {}) _        = throwError "extra attribute in pattern."


addBinding :: MonadState [Binding] m => BindingEnum -> String -> Expr -> m ()
addBinding e nm x = modify ((e,nm,x) :)


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
