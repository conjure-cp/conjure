{-# LANGUAGE ViewPatterns #-}

module Conjure.Process.ValidateConstantForDomain ( validateConstantForDomain ) where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.Instantiate ( instantiateExpression )
import Conjure.Language.NameGen ( NameGen )
import Conjure.Process.AttributeAsConstraints ( mkAttributeToConstraint )
import Conjure.Process.Enumerate ( EnumerateDomain )

-- containers
import Data.Set as S ( size, size, toList )


-- | Assuming both the value and the domain are normalised
-- TODO: this is incomplete, which means parameter values won't be checked for every property

validateConstantForDomain ::
    forall m r .
    MonadFail m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Pretty r =>
    Eq r =>
    Name -> Constant -> Domain r Constant -> m ()

validateConstantForDomain _ ConstantBool{} DomainBool{} = return ()

validateConstantForDomain _ _ (DomainInt _ []) = return ()              -- no restrictions

validateConstantForDomain name c@(ConstantInt cTag i) d@(DomainInt dTag rs) | cTag == dTag =
    let
        intInRange RangeOpen                                          = True
        intInRange (RangeSingle (ConstantInt _ a))                    = i == a
        intInRange (RangeLowerBounded (ConstantInt _ a))              = i >= a
        intInRange (RangeUpperBounded (ConstantInt _ a))              = i <= a
        intInRange (RangeBounded (ConstantInt _ a) (ConstantInt _ b)) = i >= a && i <= b
        intInRange _                                                   = False
    in  unless (any intInRange rs) (constantNotInDomain name c d)

validateConstantForDomain _ (ConstantInt _ i) (DomainUnnamed _ (ConstantInt _ a)) | i >= 1 && i <= a = return ()

validateConstantForDomain _ _ (DomainEnum _ Nothing _) = return ()    -- no restrictions
validateConstantForDomain name c d@(DomainEnum _ _ Nothing) =
    fail $ vcat [ "validateConstantForDomain: enum not handled"
                , pretty name
                , pretty c
                , pretty d
                ]
validateConstantForDomain name
    c@(ConstantInt cTag _)
    d@(DomainEnum _ (Just ranges) (Just mp)) = nested c d $ do
        let
            -- lu :: MonadFail m => Name -> m Constant
            lu (ConstantEnum _ _ nm) =
                case lookup nm mp of
                    Nothing -> fail $ "No value for:" <+> pretty nm
                    Just v  -> return (ConstantInt cTag v)
            lu (ConstantInt t v) = return (ConstantInt t v)
            lu x = fail $ "validateConstantForDomain.lu" <+> pretty x

            -- lu2 :: MonadFail m => Range Name -> m (Range Constant)
            lu2 = mapM lu

        rs <- mapM lu2 ranges
        validateConstantForDomain name c (DomainInt cTag rs :: Domain r Constant)

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitTuple cs))
    d@(DomainTuple ds) = nested c d $ zipWithM_ (validateConstantForDomain name) cs ds

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitRecord (sortOn fst -> cs)))
    d@(DomainRecord (sortOn fst -> ds))
        | map fst cs == map fst ds
            = nested c d $ zipWithM_ (validateConstantForDomain name) (map snd cs) (map snd ds)
        | otherwise
            = constantNotInDomain name c d

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitVariant _ n c'))
    d@(DomainVariant ds)
        | Just d' <- lookup n ds
            = nested c d $ validateConstantForDomain name c' d'
        | otherwise
            = constantNotInDomain name c d

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitMatrix cIndex vals))
    d@(DomainMatrix dIndex dInner) = do
        nested c d $
            mapM_ (\ val -> validateConstantForDomain name val dInner ) vals
        let
            isEmptyIntDomain (DomainInt _ []) = True
            isEmptyIntDomain _ = False
        unless (cIndex == dIndex || isEmptyIntDomain cIndex) $ fail $ vcat
            [ "The indices do not match between the value and the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            ]

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitSet vals))
    d@(DomainSet _ (SetAttr sizeAttr) dInner) = do
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt _ s) -> s == genericLength vals
                SizeAttr_MinSize (ConstantInt _ s) -> s <= genericLength vals
                SizeAttr_MaxSize (ConstantInt _ s) -> genericLength vals <= s
                SizeAttr_MinMaxSize (ConstantInt _ smin) (ConstantInt _ smax) ->
                    smin <= genericLength vals && genericLength vals <= smax
                _ -> False
        unless cardinalityOK $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        nested c d $ mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitMSet vals))
    d@(DomainMSet _ (MSetAttr sizeAttr occurAttr) dInner) = do
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt _ s) -> s == genericLength vals
                SizeAttr_MinSize (ConstantInt _ s) -> s <= genericLength vals
                SizeAttr_MaxSize (ConstantInt _ s) -> genericLength vals <= s
                SizeAttr_MinMaxSize (ConstantInt _ smin) (ConstantInt _ smax) ->
                    smin <= genericLength vals && genericLength vals <= smax
                _ -> False
        unless cardinalityOK $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        let occurOK = case occurAttr of
                OccurAttr_None -> True
                OccurAttr_MinOccur (ConstantInt _ s) -> and [ s <= occ | (_, occ) <- histogram vals ]
                OccurAttr_MaxOccur (ConstantInt _ s) -> and [ occ <= s | (_, occ) <- histogram vals ]
                OccurAttr_MinMaxOccur (ConstantInt _ smin) (ConstantInt _ smax) ->
                    and [ smin <= occ && occ <= smax | (_, occ) <- histogram vals ]
                _ -> False
        unless occurOK $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty occurAttr
            ]
        nested c d $ mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitFunction vals))
    d@(DomainFunction _ _ dFrom dTo) = nested c d $ do
        mapM_ (\ val -> validateConstantForDomain name (fst val) dFrom) vals
        mapM_ (\ val -> validateConstantForDomain name (snd val) dTo  ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitSequence vals))
    d@(DomainSequence _ _ dInner) = nested c d $
        mapM_ (\ val -> validateConstantForDomain name val dInner ) vals

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitRelation valss))
    d@(DomainRelation _ (RelationAttr sizeAttr (BinaryRelationAttrs binRelAttrs)) dInners) = do
        let numValss = genericLength valss
        let cardinalityOK = case sizeAttr of
                SizeAttr_None -> True
                SizeAttr_Size (ConstantInt _ s) -> s == numValss
                SizeAttr_MinSize (ConstantInt _ s) -> s <= numValss
                SizeAttr_MaxSize (ConstantInt _ s) -> numValss <= s
                SizeAttr_MinMaxSize (ConstantInt _ smin) (ConstantInt _ smax) ->
                    smin <= numValss && numValss <= smax
                _ -> False
        unless cardinalityOK $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Domain attributes are not satisfied."
            , "Specifically:" <+> pretty sizeAttr
            ]
        when (S.size binRelAttrs > 0 && length dInners /= 2) $ fail $ vcat
            [ "The value is not a member of the domain."
            , "Value :" <+> pretty c
            , "Domain:" <+> pretty d
            , "Reason: Binary relation attributes cannot be used for this domain."
            , "Specifically:" <+> prettyList id "," (S.toList binRelAttrs)
            ]
        forM_ (S.toList binRelAttrs) $ \ a -> do
            constraint <- mkAttributeToConstraint (fmap Constant d) (binRelToAttrName a) Nothing (Constant c)
            evaluated <- instantiateExpression [] constraint
            case evaluated of
                ConstantBool True -> return ()
                ConstantBool False -> fail $ vcat
                    [ "The value is not a member of the domain."
                    , "Value :" <+> pretty c
                    , "Domain:" <+> pretty d
                    , "Reason: Domain attributes are not satisfied."
                    , "Specifically:" <+> pretty a
                    ]
                evaluatedC -> fail $ vcat
                    [ "The value is not a member of the domain."
                    , "Value :" <+> pretty c
                    , "Domain:" <+> pretty d
                    , "Reason: Domain attributes are not satisfied."
                    , "Specifically:" <+> pretty a
                    , "Evaluted to:" <+> pretty evaluatedC
                    ]
        nested c d $ forM_ valss $ \ vals ->
            zipWithM_ (validateConstantForDomain name) vals dInners

validateConstantForDomain name
    c@(ConstantAbstract (AbsLitPartition valss))
    d@(DomainPartition _ _ dInner) = nested c d $
        mapM_ (\ val -> validateConstantForDomain name val dInner ) (concat valss)

validateConstantForDomain name c@(TypedConstant c' _) d = nested c d $ validateConstantForDomain name c' d

validateConstantForDomain name c d = constantNotInDomain name c d


nested ::
    MonadFail m =>
    Pretty c =>
    Pretty d =>
    c -> d -> ExceptT m () -> m ()
nested c d inner = do
    mres <- runExceptT inner
    case mres of
        Right () -> return ()
        Left err ->
            fail $ vcat
                [ "The value is not a member of the domain."
                , "Value :" <+> pretty c
                , "Domain:" <+> pretty d
                , "Reason:"
                , nest 4 err
                ]


constantNotInDomain :: (MonadFail m, Pretty r) => Name -> Constant -> Domain r Constant -> m ()
constantNotInDomain n c d = fail $ vcat
    [ "The value is not a member of the domain."
    , "Name  :" <+> pretty n
    , "Value :" <+> pretty c
    , "Domain:" <+> pretty d
    ]


