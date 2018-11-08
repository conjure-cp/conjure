module Conjure.Process.FiniteGivens
    ( finiteGivens
    , finiteGivensParam
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Instantiate ( instantiateExpression, instantiateDomain )
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Language.Type
import Conjure.Process.Enumerate ( EnumerateDomain )


-- | givens should have finite domains. except ints.
--   this transformation introduces extra given ints to make them finite.
--   the values for the extra givens will be computed during translate-solution
finiteGivens
    :: ( MonadFail m
       , MonadLog m
       , NameGen m
       , MonadUserError m
       , EnumerateDomain m
       )
    => Model
    -> m Model
finiteGivens m = flip evalStateT 1 $ do
    statements <- forM (mStatements m) $ \ st ->
        case st of
            Declaration (FindOrGiven Given name domain) -> do
                (domain', extras, _) <- mkFinite domain
                return $ [ Declaration $ FindOrGiven Given e (DomainInt NoTag []) | e <- extras ]
                      ++ [ Declaration $ FindOrGiven Given name domain'                   ]
            _ -> return [st]
    namegenst <- exportNameGenState
    return m { mStatements = concat statements
             , mInfo = (mInfo m) { miNameGenState = namegenst
                                 , miNbExtraGivens = maybe 0 (\ n -> n - 1 ) (lookup "fin" namegenst)
                                 }
             }


finiteGivensParam
    :: (MonadFail m, MonadUserError m, MonadLog m, NameGen m, EnumerateDomain m)
    => Model                                -- eprime
    -> Model                                -- essence-param
    -> m (Model, [Name])                    -- essence-param
finiteGivensParam eprimeModel essenceParam = flip evalStateT 1 $ do
    let essenceGivenNames = eprimeModel |> mInfo |> miGivens
    let essenceGivens     = eprimeModel |> mInfo |> miOriginalDomains
    let essenceLettings   = extractLettings essenceParam
                         ++ eprimeModel |> mInfo |> miLettings
    let nbExtraGivens     = eprimeModel |> mInfo |> miNbExtraGivens
    let expectedExtras    = [ MachineName "fin" extraGiven []
                            | extraGiven <- [1..nbExtraGivens]
                            ]
    extras <- fmap concat $ forM essenceGivenNames $ \ name -> do
        logDebugVerbose $ "finiteGivensParam name" <+> pretty name
        case (lookup name essenceGivens, lookup name essenceLettings) of
            (Nothing, _) -> bug $ "Not found:" <+> pretty name
            (_, Nothing) -> return []
            (Just domain', Just expr) -> do
                logDebugVerbose $ "finiteGivensParam domain' " <+> pretty domain'
                domain  <- fmap Constant <$> instantiateDomain essenceLettings domain'
                logDebugVerbose $ "finiteGivensParam domain  " <+> pretty domain
                logDebugVerbose $ "finiteGivensParam expr    " <+> pretty expr
                constant <- instantiateExpression essenceLettings expr
                logDebugVerbose $ "finiteGivensParam constant" <+> pretty constant
                (_, _, f) <- mkFinite domain
                outs <- f constant
                logDebugVerbose $ "finiteGivensParam outs    " <+> vcat (map pretty outs)
                return outs
    logDebugVerbose $ "finiteGivensParam extras  " <+> vcat (map (pretty . show) extras)
    return
        ( essenceParam
            { mStatements = [ Declaration (Letting name (Constant value))
                            | name <- expectedExtras
                            -- we are storing the number of "extra givens" in the model info.
                            -- also, defaulting their values to 0 if they do not come out of
                            -- the usual finiteGivens process.
                            -- the idea is: if they don't come out from that,
                            -- they must be a part of an emply collection, hence 0.
                            , let value = fromMaybe 0 (lookup name extras)
                            ]
                         ++ mStatements essenceParam
            }
        , expectedExtras
        )


-- | given a domain, add it additional attributes to make it _smaller_
--   for example, this means adding a size attribute at the outer-most level
--   and adding a maxSize attribute at the inner levels.
mkFinite
    :: ( MonadState Int m
       , MonadFail m
       , NameGen m
       , MonadLog m
       , MonadUserError m
       , EnumerateDomain m
       )
    => Domain () Expression
    -> m ( Domain () Expression                 -- "finite" domain
         , [Name]                               -- extra givens
         , Constant -> m [(Name, Constant)]     -- value calculator for the extra givens
                                                -- input is a list of values for the domain
         )
mkFinite d@DomainTuple{}     = mkFiniteOutermost d
mkFinite d@DomainRecord{}    = mkFiniteOutermost d
mkFinite d@DomainVariant{}   = mkFiniteOutermost d
mkFinite d@DomainMatrix{}    = mkFiniteOutermost d
mkFinite d@DomainSet{}       = mkFiniteOutermost d
mkFinite d@DomainMSet{}      = mkFiniteOutermost d
mkFinite d@DomainSequence{}  = mkFiniteOutermost d
mkFinite d@DomainFunction{}  = mkFiniteOutermost d
mkFinite d@DomainRelation{}  = mkFiniteOutermost d
mkFinite d@DomainPartition{} = mkFiniteOutermost d
mkFinite d = return (d, [], const (return []))


mkFiniteOutermost
    :: ( MonadState Int m
       , MonadFail m
       , NameGen m
       , MonadLog m
       , MonadUserError m
       , EnumerateDomain m
       )
    => Domain () Expression
    -> m ( Domain () Expression
         , [Name]
         , Constant -> m [(Name, Constant)]
         )
mkFiniteOutermost (DomainTuple inners) = do
    mids <- mapM mkFiniteInner inners
    return
        ( DomainTuple (map fst3 mids)
        , concatMap snd3 mids
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainTuple" <+> pretty constant
                xs <- failToUserError $ viewConstantTuple constant
                let innerFs = map thd3 mids
                innerValues <- sequence [ innerF [x] | (innerF, x) <- zip innerFs xs ]
                return (concat innerValues)
        )
mkFiniteOutermost (DomainRecord inners) = do
    mids <- mapM (mkFiniteInner . snd) inners
    return
        ( DomainRecord (zip (map fst inners) (map fst3 mids))
        , concatMap snd3 mids
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainRecord" <+> pretty constant
                xs' <- failToUserError $ viewConstantRecord constant
                let
                    xs :: [Constant]
                    xs = map snd xs'
                let innerFs = map thd3 mids
                innerValues <- sequence [ innerF [x] | (innerF, x) <- zip innerFs xs ]
                return (concat innerValues)
        )
mkFiniteOutermost (DomainVariant inners) = do
    mids <- mapM (mkFiniteInner . snd) inners
    return
        ( DomainVariant (zip (map fst inners) (map fst3 mids))
        , concatMap snd3 mids
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainVariant" <+> pretty constant
                xs' <- failToUserError $ viewConstantVariant constant
                xs :: [Constant] <- sequence
                    [ case xs' of
                        (_, nm', c') | nm == nm' -> return c'
                        _ -> instantiateDomain [] d >>= zeroVal
                    | (nm, d) <- inners ]
                let innerFs = map thd3 mids
                innerValues <- sequence [ innerF [x] | (innerF, x) <- zip innerFs xs ]
                return (concat innerValues)
        )
mkFiniteOutermost (DomainMatrix index inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainMatrix index inner'
        , innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainMatrix" <+> pretty constant
                (_, matr) <- failToUserError $ viewConstantMatrix constant
                innerValues <- innerF matr
                return innerValues
        )
mkFiniteOutermost (DomainSet () attr@(SetAttr SizeAttr_Size{}) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () attr inner'
        , innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainSet" <+> pretty constant
                set <- failToUserError $ viewConstantSet constant
                innerValues <- innerF set
                return innerValues
        )
mkFiniteOutermost (DomainSet () _ inner) = do
    s <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () (SetAttr (SizeAttr_Size (fromName s))) inner'
        , s:innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainSet" <+> pretty constant
                set <- failToUserError $ viewConstantSet constant
                let setSize = genericLength set
                innerValues <- innerF set
                return $ innerValues ++ [(s, ConstantInt NoTag setSize)]
        )
mkFiniteOutermost (DomainMSet () attr@(MSetAttr SizeAttr_Size{} _) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainMSet () attr inner'
        , innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainMSet" <+> pretty constant
                set <- failToUserError $ viewConstantMSet constant
                innerValues <- innerF set
                return innerValues
        )
mkFiniteOutermost (DomainMSet () (MSetAttr _ occurAttr) inner) = do
    s <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainMSet () (MSetAttr (SizeAttr_Size (fromName s)) occurAttr) inner'
        , s:innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainMSet" <+> pretty constant
                set <- failToUserError $ viewConstantMSet constant
                let setSize = genericLength set
                innerValues <- innerF set
                return $ innerValues ++ [(s, ConstantInt NoTag setSize)]
        )
mkFiniteOutermost (DomainSequence () attr@(SequenceAttr SizeAttr_Size{} _) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSequence () attr inner'
        , innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainSequence" <+> pretty constant
                set <- failToUserError $ viewConstantSequence constant
                innerValues <- innerF set
                return innerValues
        )
mkFiniteOutermost (DomainSequence () (SequenceAttr _ jectivityAttr) inner) = do
    s <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSequence () (SequenceAttr (SizeAttr_Size (fromName s)) jectivityAttr) inner'
        , s:innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainSequence" <+> pretty constant
                set <- failToUserError $ viewConstantSequence constant
                let setSize = genericLength set
                innerValues <- innerF set
                return $ innerValues ++ [(s, ConstantInt NoTag setSize)]
        )
mkFiniteOutermost (DomainFunction () attr@(FunctionAttr SizeAttr_Size{} _ _) innerFr innerTo) = do
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction () attr innerFr' innerTo'
        , innerFrExtras ++ innerToExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainFunction" <+> pretty constant
                function <- failToUserError $ viewConstantFunction constant
                innerFrValues <- innerFrF (map fst function)
                innerToValues <- innerToF (map snd function)
                return $ innerFrValues ++ innerToValues
        )
mkFiniteOutermost (DomainFunction () (FunctionAttr _ partialityAttr jectivityAttr) innerFr innerTo) = do
    s <- nextName "fin"
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction ()
                (FunctionAttr (SizeAttr_Size (fromName s)) partialityAttr jectivityAttr)
                innerFr' innerTo'
        , s : innerFrExtras ++ innerToExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainFunction" <+> pretty constant
                function <- failToUserError $ viewConstantFunction constant
                let functionSize = genericLength function
                innerFrValues <- innerFrF (map fst function)
                innerToValues <- innerToF (map snd function)
                return $ innerFrValues ++ innerToValues ++ [(s, ConstantInt NoTag functionSize)]
        )
mkFiniteOutermost (DomainRelation () attr@(RelationAttr SizeAttr_Size{} _) inners) = do
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation () attr inners'
        , concat innersExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainRelation" <+> pretty constant
                relation <- failToUserError $ viewConstantRelation constant
                innersValues <- zipWithM ($) innersF (transpose relation)
                return (concat innersValues)
        )
mkFiniteOutermost (DomainRelation () (RelationAttr _ binRelAttr) inners) = do
    s <- nextName "fin"
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation ()
                (RelationAttr (SizeAttr_Size (fromName s)) binRelAttr)
                inners'
        , s : concat innersExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainRelation" <+> pretty constant
                relation <- failToUserError $ viewConstantRelation constant
                let relationSize = genericLength relation
                innersValues <- zipWithM ($) innersF (transpose relation)
                return $ concat innersValues ++ [(s, ConstantInt NoTag relationSize)]
        )
mkFiniteOutermost (DomainPartition () attr@(PartitionAttr SizeAttr_Size{} SizeAttr_Size{} _) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainPartition () attr inner'
        , innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainPartition" <+> pretty constant
                parts <- failToUserError $ viewConstantPartition constant
                innerValues <- mapM innerF parts
                return (concat innerValues)
        )
mkFiniteOutermost (DomainPartition () (PartitionAttr _ _ isRegularAttr) inner) = do
    numPartsFin  <- nextName "fin"
    partsSizeFin <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainPartition ()
            (PartitionAttr (SizeAttr_Size (fromName numPartsFin))
                           (SizeAttr_MaxSize (fromName partsSizeFin))
                           isRegularAttr)
            inner'
        , numPartsFin:partsSizeFin:innerExtras
        , \ constant -> do
                logDebug $ "mkFiniteOutermost DomainPartition" <+> pretty constant
                parts <- failToUserError $ viewConstantPartition constant
                let numPartsVal = genericLength parts
                let partsSizeVal = maximum0 $ map genericLength parts
                innerValues <- mapM innerF parts
                return $ concat innerValues ++ [ (numPartsFin, ConstantInt NoTag numPartsVal)
                                               , (partsSizeFin, ConstantInt NoTag partsSizeVal)
                                               ]
        )
mkFiniteOutermost d = return (d, [], const (return []))


mkFiniteInner
    :: ( MonadState Int m
       , MonadFail m
       , NameGen m
       , MonadLog m
       , MonadUserError m
       , EnumerateDomain m
       )
    => Domain () Expression
    -> m ( Domain () Expression
         , [Name]
         , [Constant] -> m [(Name, Constant)]
         )
mkFiniteInner (DomainInt t []) = do
    fr <- nextName "fin"
    to <- nextName "fin"
    return
        ( DomainInt t [RangeBounded (fromName fr) (fromName to)]
        , [fr, to]
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainInt" <+> vcat (map pretty constants)
                ints <- failToUserError $ mapM viewConstantInt constants
                return [ (fr, ConstantInt t (minimum ints))
                       , (to, ConstantInt t (maximum0 ints))
                       ]
        )
mkFiniteInner (DomainInt t [RangeLowerBounded low]) = do
    new <- nextName "fin"
    return
        ( DomainInt t [RangeBounded low (fromName new)]
        , [new]
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainInt" <+> vcat (map pretty constants)
                ints <- failToUserError $ mapM viewConstantInt constants
                return [ (new, ConstantInt t (maximum0 ints)) ]
        )
mkFiniteInner (DomainInt t [RangeUpperBounded upp]) = do
    new <- nextName "fin"
    return
        ( DomainInt t [RangeBounded (fromName new) upp]
        , [new]
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainInt" <+> vcat (map pretty constants)
                ints <- failToUserError $ mapM viewConstantInt constants
                return [ (new, ConstantInt t (minimum ints)) ]
        )
mkFiniteInner (DomainTuple inners) = do
    mids <- mapM mkFiniteInner inners
    return
        ( DomainTuple (map fst3 mids)
        , concatMap snd3 mids
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainTuple" <+> vcat (map pretty constants)
                xss <- failToUserError $ mapM viewConstantTuple constants
                let innerFs = map thd3 mids
                innerValues <- sequence [ innerF xs | (innerF, xs) <- zip innerFs (transpose xss) ]
                return (concat innerValues)
        )
mkFiniteInner (DomainRecord inners) = do
    mids <- mapM (mkFiniteInner . snd) inners
    return
        ( DomainRecord (zip (map fst inners) (map fst3 mids))
        , concatMap snd3 mids
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainRecord" <+> vcat (map pretty constants)
                xss' :: [[(Name, Constant)]] <- failToUserError $ mapM viewConstantRecord constants
                let
                    xss :: [[Constant]]
                    xss = map (map snd) xss'
                let innerFs = map thd3 mids
                innerValues <- sequence [ innerF xs | (innerF, xs) <- zip innerFs (transpose xss) ]
                return (concat innerValues)
        )
mkFiniteInner (DomainVariant inners) = do
    mids <- mapM (mkFiniteInner . snd) inners
    return
        ( DomainVariant (zip (map fst inners) (map fst3 mids))
        , concatMap snd3 mids
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainVariant" <+> vcat (map pretty constants)
                xss' <- failToUserError $ mapM viewConstantVariant constants
                xss :: [[Constant]]
                    <- sequence
                            [ sequence
                                [ case xs' of
                                    (_, nm', c') | nm == nm' -> return c'
                                    _ -> instantiateDomain [] d >>= zeroVal
                                | (nm, d) <- inners ]
                            | xs' <- xss' ]
                let innerFs = map thd3 mids
                innerValues <- sequence [ innerF xs | (innerF, xs) <- zip innerFs (transpose xss) ]
                return (concat innerValues)
        )
mkFiniteInner (DomainMatrix index inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainMatrix index inner'
        , innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainMatrix" <+> vcat (map pretty constants)
                xss <- failToUserError $ mapM viewConstantMatrix constants
                innerF (concatMap snd xss)
        )
mkFiniteInner (DomainSet () attr@(SetAttr SizeAttr_Size{}) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () attr inner'
        , innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainSet" <+> vcat (map pretty constants)
                sets <- failToUserError $ mapM viewConstantSet constants
                innerF (concat sets)
        )
mkFiniteInner (DomainSet () _ inner) = do
    s <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () (SetAttr (SizeAttr_MaxSize (fromName s))) inner'
        , s:innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainSet" <+> vcat (map pretty constants)
                sets <- failToUserError $ mapM viewConstantSet constants
                let setMaxSize = maximum0 $ map genericLength sets
                innerValues <- innerF (concat sets)
                return $ innerValues ++ [(s, ConstantInt NoTag setMaxSize)]
        )
mkFiniteInner (DomainMSet () attr@(MSetAttr SizeAttr_Size{} _) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainMSet () attr inner'
        , innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainMSet" <+> vcat (map pretty constants)
                sets <- failToUserError $ mapM viewConstantMSet constants
                innerF (concat sets)
        )
mkFiniteInner (DomainMSet () (MSetAttr _ occurAttr) inner) = do
    s <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainMSet () (MSetAttr (SizeAttr_MaxSize (fromName s)) occurAttr) inner'
        , s:innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainMSet" <+> vcat (map pretty constants)
                sets <- failToUserError $ mapM viewConstantMSet constants
                let setMaxSize = maximum0 $ map genericLength sets
                innerValues <- innerF (concat sets)
                return $ innerValues ++ [(s, ConstantInt NoTag setMaxSize)]
        )
mkFiniteInner (DomainSequence () attr@(SequenceAttr SizeAttr_Size{} _) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSequence () attr inner'
        , innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainSequence" <+> vcat (map pretty constants)
                seqs <- failToUserError $ mapM viewConstantSequence constants
                innerF (concat seqs)
        )
mkFiniteInner (DomainSequence () (SequenceAttr _ jectivityAttr) inner) = do
    s <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSequence () (SequenceAttr (SizeAttr_MaxSize (fromName s)) jectivityAttr) inner'
        , s:innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainSequence" <+> vcat (map pretty constants)
                seqs <- failToUserError $ mapM viewConstantSequence constants
                let seqMaxSize = maximum0 $ map genericLength seqs
                innerValues <- innerF (concat seqs)
                return $ innerValues ++ [(s, ConstantInt NoTag seqMaxSize)]
        )
mkFiniteInner (DomainFunction () attr@(FunctionAttr SizeAttr_Size{} _ _) innerFr innerTo) = do
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction () attr innerFr' innerTo'
        , innerFrExtras ++ innerToExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainFunction" <+> vcat (map pretty constants)
                functions <- failToUserError $ mapM viewConstantFunction constants
                innerFrValues <- innerFrF (map fst (concat functions))
                innerToValues <- innerToF (map snd (concat functions))
                return $ innerFrValues ++ innerToValues
        )
mkFiniteInner (DomainFunction () (FunctionAttr _ partialityAttr jectivityAttr) innerFr innerTo) = do
    s <- nextName "fin"
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction ()
                (FunctionAttr (SizeAttr_MaxSize (fromName s)) partialityAttr jectivityAttr)
                innerFr' innerTo'
        , s : innerFrExtras ++ innerToExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainFunction" <+> vcat (map pretty constants)
                functions <- failToUserError $ mapM viewConstantFunction constants
                let functionMaxSize = maximum0 $ map genericLength functions
                innerFrValues <- innerFrF (map fst (concat functions))
                innerToValues <- innerToF (map snd (concat functions))
                return $ innerFrValues ++ innerToValues ++ [(s, ConstantInt NoTag functionMaxSize)]
        )
mkFiniteInner (DomainRelation () attr@(RelationAttr SizeAttr_Size{} _) inners) = do
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation () attr inners'
        , concat innersExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainRelation" <+> vcat (map pretty constants)
                relations <- failToUserError $ mapM viewConstantRelation constants
                innersValues <- zipWithM ($) innersF (transpose $ concat relations)
                return $ concat innersValues
        )
mkFiniteInner (DomainRelation () (RelationAttr _ binRelAttr) inners) = do
    s <- nextName "fin"
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation ()
                (RelationAttr (SizeAttr_MaxSize (fromName s)) binRelAttr)
                inners'
        , s : concat innersExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainRelation" <+> vcat (map pretty constants)
                relations <- failToUserError $ mapM viewConstantRelation constants
                let relationMaxSize = maximum0 $ map genericLength relations
                innersValues <- zipWithM ($) innersF (transpose $ concat relations)
                return $ concat innersValues ++ [(s, ConstantInt NoTag relationMaxSize)]
        )
mkFiniteInner (DomainPartition () attr@(PartitionAttr SizeAttr_Size{} SizeAttr_Size{} _) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainPartition () attr inner'
        , innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainPartition" <+> vcat (map pretty constants)
                parts <- failToUserError $ mapM viewConstantPartition constants
                innersValues <- mapM innerF (concat parts)
                return $ concat innersValues
        )
mkFiniteInner (DomainPartition () (PartitionAttr _ _ isRegularAttr) inner) = do
    numPartsFin  <- nextName "fin"
    partsSizeFin <- nextName "fin"
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainPartition ()
            (PartitionAttr (SizeAttr_MaxSize (fromName numPartsFin))
                           (SizeAttr_MaxSize (fromName partsSizeFin))
                           isRegularAttr)
            inner'
        , numPartsFin:partsSizeFin:innerExtras
        , \ constants -> do
                logDebug $ "mkFiniteInner DomainPartition" <+> vcat (map pretty constants)
                parts <- failToUserError $ mapM viewConstantPartition constants
                let numPartsVal = maximum0 $ map genericLength parts
                let partsSizeVal = maximum0 $ map genericLength parts
                innerValues <- mapM innerF (concat parts)
                return $ concat innerValues ++ [ (numPartsFin, ConstantInt NoTag numPartsVal)
                                               , (partsSizeFin, ConstantInt NoTag partsSizeVal)
                                               ]
        )
mkFiniteInner d = return (d, [], const (return []))


-- specialised the type for maximum0, to avoid possible bugs
-- this function is always intended to be used with Integers
maximum0 :: [Integer] -> Integer
maximum0 xs = maximum (0:xs)
