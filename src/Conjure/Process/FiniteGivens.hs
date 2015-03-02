{-# LANGUAGE BangPatterns #-}

module Conjure.Process.FiniteGivens
    ( finiteGivens
    , finiteGivensParam
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Instantiate ( instantiateExpression )


-- | givens should have finite domains. except ints.
--   this transformation introduces extra given ints to make them finite.
--   the values for the extra givens will be computed during translate-solution
finiteGivens
    :: (MonadFail m, MonadLog m)
    => Model
    -> m Model
finiteGivens m = flip evalStateT 1 $ do
    statements <- forM (mStatements m) $ \ st ->
        case st of
            Declaration (FindOrGiven Given name domain) -> do
                (domain', extras, _) <- mkFinite domain
                return $ [ Declaration $ FindOrGiven Given e (DomainInt []) | e <- extras ]
                      ++ [ Declaration $ FindOrGiven Given name domain'                   ]
            _ -> return [st]
    return m { mStatements = concat statements }


finiteGivensParam
    :: (MonadFail m, MonadLog m)
    => Model                                -- eprime
    -> Model                                -- essence-param
    -> m (Model, [Name])                    -- essence-param
finiteGivensParam eprimeModel essenceParam = flip evalStateT 1 $ do
    let essenceGivenNames = eprimeModel |> mInfo |> miGivens
    let essenceGivens     = eprimeModel |> mInfo |> miOriginalDomains
    let essenceLettings   = extractLettings essenceParam
    extras <- forM essenceGivenNames $ \ name -> do
        logDebugVerbose $ "finiteGivensParam name" <+> pretty name
        case (lookup name essenceGivens, lookup name essenceLettings) of
            (Nothing, _) -> bug $ "Not found:" <+> pretty name
            (_, Nothing) -> return []
            (Just domain, Just expr) -> do
                logDebugVerbose $ "finiteGivensParam domain  " <+> pretty domain
                logDebugVerbose $ "finiteGivensParam expr    " <+> pretty expr
                constant <- instantiateExpression [] expr
                logDebugVerbose $ "finiteGivensParam constant" <+> pretty constant
                (_, _, f) <- mkFinite domain
                outs <- f [constant]
                logDebugVerbose $ "finiteGivensParam outs    " <+> vcat (map pretty outs)
                return outs
    return
        ( essenceParam
            { mStatements = [ Declaration (Letting n (Constant c)) | (n,c) <- concat extras ]
                         ++ mStatements essenceParam
            }
        , map fst (concat extras)
        )


-- | given a domain, add it additional attributes to make it _smaller_
--   for example, this means adding a size attribute at the outer-most level
--   and adding a maxSize attribute at the inner levels.
mkFinite
    :: (MonadState Int m, MonadFail m)
    => Domain () Expression
    -> m ( Domain () Expression                 -- "finite" domain
         , [Name]                               -- extra givens
         , [Constant] -> m [(Name, Constant)]   -- value calculator for the extra givens
                                                -- input is a list of values for the domain
         )
mkFinite d@DomainSet{} = mkFiniteOutermost d
mkFinite d@DomainFunction{} = mkFiniteOutermost d
mkFinite d@DomainRelation{} = mkFiniteOutermost d
mkFinite d = return (d, [], const (return []))


mkFiniteOutermost
    :: (MonadState Int m, MonadFail m)
    => Domain () Expression
    -> m ( Domain () Expression
         , [Name]
         , [Constant] -> m [(Name, Constant)]
         )
mkFiniteOutermost (DomainSet () attr@(SetAttr SizeAttr_Size{}) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () attr inner'
        , innerExtras
        , \ constants -> case constants of
                [constant] -> do
                    set <- viewConstantSet constant
                    innerValues <- innerF set
                    return innerValues
                _ -> fail "mkFiniteOutermost: multiple values"
        )
mkFiniteOutermost (DomainSet () _ inner) = do
    s <- nextName
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () (SetAttr (SizeAttr_Size (fromName s))) inner'
        , s:innerExtras
        , \ constants -> case constants of
                [constant] -> do
                    set <- viewConstantSet constant
                    let setSize = genericLength $ nub set
                    innerValues <- innerF set
                    return $ innerValues ++ [(s, ConstantInt setSize)]
                _ -> fail "mkFiniteOutermost: multiple values"
        )
mkFiniteOutermost (DomainFunction () attr@(FunctionAttr SizeAttr_Size{} _ _) innerFr innerTo) = do
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction () attr innerFr' innerTo'
        , innerFrExtras ++ innerToExtras
        , \ constants -> case constants of
                [constant] -> do
                    function <- viewConstantFunction constant
                    innerFrValues <- innerFrF (map fst function)
                    innerToValues <- innerToF (map snd function)
                    return $ innerFrValues ++ innerToValues
                _ -> fail "mkFiniteOutermost: multiple values"
        )
mkFiniteOutermost (DomainFunction () (FunctionAttr _ partialityAttr jectivityAttr) innerFr innerTo) = do
    s <- nextName
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction ()
                (FunctionAttr (SizeAttr_Size (fromName s)) partialityAttr jectivityAttr)
                innerFr' innerTo'
        , s : innerFrExtras ++ innerToExtras
        , \ constants -> case constants of
                [constant] -> do
                    function <- viewConstantFunction constant
                    let functionSize = genericLength $ nub function
                    innerFrValues <- innerFrF (map fst function)
                    innerToValues <- innerToF (map snd function)
                    return $ innerFrValues ++ innerToValues ++ [(s, ConstantInt functionSize)]
                _ -> fail "mkFiniteOutermost: multiple values"
        )
mkFiniteOutermost (DomainRelation () attr@(RelationAttr SizeAttr_Size{} _) inners) = do
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation () attr inners'
        , concat innersExtras
        , \ constants -> case constants of
                [constant] -> do
                    relation <- viewConstantRelation constant
                    innersValues <- zipWithM ($) innersF (transpose relation)
                    return (concat innersValues)
                _ -> fail "mkFiniteOutermost: multiple values"
        )
mkFiniteOutermost (DomainRelation () (RelationAttr _ binRelAttr) inners) = do
    s <- nextName
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation ()
                (RelationAttr (SizeAttr_Size (fromName s)) binRelAttr)
                inners'
        , s : concat innersExtras
        , \ constants -> case constants of
                [constant] -> do
                    relation <- viewConstantRelation constant
                    let relationSize = genericLength $ nub relation
                    innersValues <- zipWithM ($) innersF (transpose relation)
                    return $ concat innersValues ++ [(s, ConstantInt relationSize)]
                _ -> fail "mkFiniteOutermost: multiple values"
        )
mkFiniteOutermost d = return (d, [], const (return []))


mkFiniteInner
    :: (MonadState Int m, MonadFail m)
    => Domain () Expression
    -> m ( Domain () Expression
         , [Name]
         , [Constant] -> m [(Name, Constant)]
         )
mkFiniteInner (DomainInt []) = do
    fr <- nextName
    to <- nextName
    return
        ( DomainInt [RangeBounded (fromName fr) (fromName to)]
        , [fr, to]
        , \ constants -> do
                ints <- mapM viewConstantInt constants
                return [ (fr, ConstantInt (minimum ints))
                       , (to, ConstantInt (maximum ints))
                       ]
        )
mkFiniteInner (DomainSet () attr@(SetAttr SizeAttr_Size{}) inner) = do
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () attr inner'
        , innerExtras
        , \ constants -> do
                sets <- mapM viewConstantSet constants
                innerF (concat sets)
        )
mkFiniteInner (DomainSet () _ inner) = do
    s <- nextName
    (inner', innerExtras, innerF) <- mkFiniteInner inner
    return
        ( DomainSet () (SetAttr (SizeAttr_MaxSize (fromName s))) inner'
        , s:innerExtras
        , \ constants -> do
                sets <- mapM viewConstantSet constants
                let setMaxSize = maximum $ map (genericLength . nub) sets
                innerValues <- innerF (concat sets)
                return $ innerValues ++ [(s, ConstantInt setMaxSize)]
        )
mkFiniteInner (DomainFunction () attr@(FunctionAttr SizeAttr_Size{} _ _) innerFr innerTo) = do
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction () attr innerFr' innerTo'
        , innerFrExtras ++ innerToExtras
        , \ constants -> do
                functions <- mapM viewConstantFunction constants
                innerFrValues <- innerFrF (map fst $ concat functions)
                innerToValues <- innerToF (map snd $ concat functions)
                return $ innerFrValues ++ innerToValues
        )
mkFiniteInner (DomainFunction () (FunctionAttr _ partialityAttr jectivityAttr) innerFr innerTo) = do
    s <- nextName
    (innerFr', innerFrExtras, innerFrF) <- mkFiniteInner innerFr
    (innerTo', innerToExtras, innerToF) <- mkFiniteInner innerTo
    return
        ( DomainFunction ()
                (FunctionAttr (SizeAttr_MaxSize (fromName s)) partialityAttr jectivityAttr)
                innerFr' innerTo'
        , s : innerFrExtras ++ innerToExtras
        , \ constants -> do
                functions <- mapM viewConstantFunction constants
                let functionMaxSize = maximum $ map (genericLength . nub) functions
                innerFrValues <- innerFrF (map fst $ concat functions)
                innerToValues <- innerToF (map snd $ concat functions)
                return $ innerFrValues ++ innerToValues ++ [(s, ConstantInt functionMaxSize)]
        )
mkFiniteInner (DomainRelation () attr@(RelationAttr SizeAttr_Size{} _) inners) = do
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation () attr inners'
        , concat innersExtras
        , \ constants -> do
                relations <- mapM viewConstantRelation constants
                innersValues <- zipWithM ($) innersF (transpose $ concat relations)
                return $ concat innersValues
        )
mkFiniteInner (DomainRelation () (RelationAttr _ binRelAttr) inners) = do
    s <- nextName
    (inners', innersExtras, innersF) <- unzip3 <$> mapM mkFiniteInner inners
    return
        ( DomainRelation ()
                (RelationAttr (SizeAttr_MaxSize (fromName s)) binRelAttr)
                inners'
        , s : concat innersExtras
        , \ constants -> do
                relations <- mapM viewConstantRelation constants
                let relationMaxSize = maximum $ map (genericLength . nub) relations
                innersValues <- zipWithM ($) innersF (transpose $ concat relations)
                return $ concat innersValues ++ [(s, ConstantInt relationMaxSize)]
        )
mkFiniteInner d = return (d, [], const (return []))


nextName :: MonadState Int m => m Name
nextName = do
    !i <- gets id
    modify (+1)
    return (Name ("p" `mappend` stringToText (show i)))

viewConstantInt :: MonadFail m => Constant -> m Integer
viewConstantInt (ConstantInt i) = return i
viewConstantInt constant = fail ("Expecting an integer, but got:" <+> pretty constant)

viewConstantSet :: MonadFail m => Constant -> m [Constant]
viewConstantSet (ConstantAbstract (AbsLitSet xs)) = return xs
viewConstantSet constant = fail ("Expecting a set, but got:" <+> pretty constant)

viewConstantFunction :: MonadFail m => Constant -> m [(Constant,Constant)]
viewConstantFunction (ConstantAbstract (AbsLitFunction xs)) = return xs
viewConstantFunction constant = fail ("Expecting a function, but got:" <+> pretty constant)

viewConstantRelation :: MonadFail m => Constant -> m [[Constant]]
viewConstantRelation (ConstantAbstract (AbsLitRelation xs)) = return xs
viewConstantRelation constant = fail ("Expecting a relation, but got:" <+> pretty constant)
