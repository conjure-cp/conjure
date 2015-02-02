{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

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


mkFinite
    :: (MonadState Int m, MonadFail m)
    => Domain () Expression
    -> m ( Domain () Expression                 -- "finite" domain
         , [Name]                               -- extra givens
         , [Constant] -> m [(Name, Constant)]   -- value calculator for the extra givens
                                                -- input is a list of values for the domain
         )
mkFinite d@DomainSet{} = helper d
mkFinite d@DomainFunction{} = helper d
mkFinite d@DomainRelation{} = helper d
mkFinite d = return (d, [], const (return []))


helper
    :: (MonadState Int m, MonadFail m)
    => Domain () Expression
    -> m ( Domain () Expression
         , [Name]
         , [Constant] -> m [(Name, Constant)]
         )
helper (DomainInt []) = do
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
helper (DomainSet () attr@(SetAttr SizeAttr_Size{}) inner) = do
    (inner', innerExtras, innerF) <- helper inner
    return
        ( DomainSet () attr inner'
        , innerExtras
        , \ constants -> do
                sets <- mapM viewConstantSet constants
                innerF (concat sets)
        )
helper (DomainSet () _ inner) = do
    s <- nextName
    (inner', innerExtras, innerF) <- helper inner
    return
        ( DomainSet () (SetAttr (SizeAttr_Size (fromName s))) inner'
        , s:innerExtras
        , \ constants -> do
                sets <- mapM viewConstantSet constants
                let setSizes = map (length . nub) sets
                setSize <- allEqual setSizes
                innerValues <- innerF (concat sets)
                return $ innerValues ++ [(s, ConstantInt setSize)]
        )
helper (DomainFunction () attr@(FunctionAttr SizeAttr_Size{} _ _) innerFr innerTo) = do
    (innerFr', innerFrExtras, innerFrF) <- helper innerFr
    (innerTo', innerToExtras, innerToF) <- helper innerTo
    return
        ( DomainFunction () attr innerFr' innerTo'
        , innerFrExtras ++ innerToExtras
        , \ constants -> do
                functions <- mapM viewConstantFunction constants
                innerFrValues <- innerFrF (map fst $ concat functions)
                innerToValues <- innerToF (map snd $ concat functions)
                return $ innerFrValues ++ innerToValues
        )
helper (DomainFunction () (FunctionAttr _ partialityAttr jectivityAttr) innerFr innerTo) = do
    s <- nextName
    (innerFr', innerFrExtras, innerFrF) <- helper innerFr
    (innerTo', innerToExtras, innerToF) <- helper innerTo
    return
        ( DomainFunction ()
                (FunctionAttr (SizeAttr_Size (fromName s)) partialityAttr jectivityAttr)
                innerFr' innerTo'
        , s : innerFrExtras ++ innerToExtras
        , \ constants -> do
                functions <- mapM viewConstantFunction constants
                let functionSizes = map (length . nub) functions
                functionSize <- allEqual functionSizes
                innerFrValues <- innerFrF (map fst $ concat functions)
                innerToValues <- innerToF (map snd $ concat functions)
                return $ innerFrValues ++ innerToValues ++ [(s, ConstantInt functionSize)]
        )
helper (DomainRelation () attr@(RelationAttr SizeAttr_Size{} _) inners) = do
    (inners', innersExtras, innersF) <- unzip3 <$> mapM helper inners
    return
        ( DomainRelation () attr inners'
        , concat innersExtras
        , \ constants -> do
                relations <- mapM viewConstantRelation constants
                innersValues <- zipWithM ($) innersF (transpose $ concat relations)
                return $ concat innersValues
        )
helper (DomainRelation () (RelationAttr _ binRelAttr) inners) = do
    s <- nextName
    (inners', innersExtras, innersF) <- unzip3 <$> mapM helper inners
    return
        ( DomainRelation ()
                (RelationAttr (SizeAttr_Size (fromName s)) binRelAttr)
                inners'
        , s : concat innersExtras
        , \ constants -> do
                relations <- mapM viewConstantRelation constants
                let relationSizes = map (length . nub) relations
                relationSize <- allEqual relationSizes
                innersValues <- zipWithM ($) innersF (transpose $ concat relations)
                return $ concat innersValues ++ [(s, ConstantInt relationSize)]
        )
helper d = return (d, [], const (return []))


nextName :: MonadState Int m => m Name
nextName = do
    !i <- gets id
    modify (+1)
    return (Name ("p" `mappend` stringToText (show i)))

viewConstantInt :: MonadFail m => Constant -> m Int
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

allEqual :: (Eq a, Pretty a, MonadFail m) => [a] -> m a
allEqual (x:xs) | all (x==) xs = return x
allEqual [] = fail "allEqual, false: no value."
allEqual xs = fail $ "allEqual, false:" <+> prettyList id "," xs
