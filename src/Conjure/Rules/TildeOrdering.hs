{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.TildeOrdering where

import Conjure.Rules.Import


-- Occurrence coordinates are already in the global element order.  Unlike
-- symmetryOrdering for sets, use positive membership bits: absence < presence.
-- Equal index domains are essential; equal vector lengths alone are not enough.
rule_Occurrence :: Rule
rule_Occurrence = "tildeOrd-occurrence" `namedRule` theRule where
    theRule p = do
        (x, y, mk) <- case p of
            [essence| &x ~< &y |]  -> return (x, y, \ a b -> [essence| &a <lex &b |])
            [essence| &x ~<= &y |] -> return (x, y, \ a b -> [essence| &a <=lex &b |])
            _ -> na "rule_Occurrence"
        rx <- representationOf x
        ry <- representationOf y
        unless (rx == ry) $ na "rule_Occurrence: different representations"
        case rx of
            Set_Occurrence    -> return ()
            MSet_Occurrence   -> return ()
            Relation_AsMatrix -> return ()
            _ -> na "rule_Occurrence: not an occurrence representation"
        [mx] <- downX1 x
        [my] <- downX1 y
        ix <- indexDomainsOf mx
        iy <- indexDomainsOf my
        unless (not (null ix) && ix == iy) $ na "rule_Occurrence: different index domains"
        indexTypes <- mapM typeOfDomain ix
        unless (all typeCanIndexMatrix indexTypes) $ na "rule_Occurrence: non-primitive indices"
        return
            ( "Global order via lexicographic occurrence comparison"
            , return $ mk (make opFlatten mx) (make opFlatten my)
            )


-- Sorted explicit lists encode the first differing frequency by the first
-- differing value, with the value order reversed.  An exhausted list comes
-- before a live entry.  Restrict this to integer elements: .< reverses Boolean
-- order, and structured values need not agree with their global order either.
rule_Explicit :: Rule
rule_Explicit = "tildeOrd-explicit" `namedRule` theRule where
    theRule p = do
        (x, y, mk) <- case p of
            [essence| &x ~< &y |]  -> return (x, y, \ a b -> [essence| &a <lex &b |])
            [essence| &x ~<= &y |] -> return (x, y, \ a b -> [essence| &a <=lex &b |])
            _ -> na "rule_Explicit"
        rx <- representationOf x
        ry <- representationOf y
        unless (rx == ry) $ na "rule_Explicit: different representations"
        unless (rx `elem` [Set_Explicit, Set_ExplicitVarSizeWithDummy,
                          Set_ExplicitVarSizeWithFlags, Set_ExplicitVarSizeWithMarker,
                          MSet_ExplicitWithRepetition, MSet_ExplicitWithFlags]) $
            na "rule_Explicit: unsupported representation"
        xs <- downX1 x
        ys <- downX1 y
        vx : _ <- return $ reverse xs
        vy : _ <- return $ reverse ys
        DomainMatrix ix dx <- domainOf vx
        DomainMatrix iy dy <- domainOf vy
        unless (ix == iy) $ na "rule_Explicit: different capacities"
        tx <- typeOfDomain dx
        ty <- typeOfDomain dy
        unless (tx == ty && case tx of TypeInt{} -> True; _ -> False) $
            na "rule_Explicit: non-integer elements"
        -- Dummy values must denote the same sentinel on both sides.
        when (rx == Set_ExplicitVarSizeWithDummy && dx /= dy) $
            na "rule_Explicit: different dummy domains"
        return
            ( "Global order via lexicographic explicit-list comparison"
            , if rx `elem` [Set_Explicit, Set_ExplicitVarSizeWithDummy]
                then return $ mk vy vx
                else do
                    let key refs = do
                            [flags, values] <- return refs
                            (iPat, i) <- quantifiedVar
                            let active = case rx of
                                    Set_ExplicitVarSizeWithFlags -> [essence| &flags[&i] |]
                                    MSet_ExplicitWithFlags -> [essence| &flags[&i] > 0 |]
                                    _ -> [essence| &i <= &flags |]
                            let value = [essence| &values[&i] |]
                            -- Inactive values are don't-cares; normalise them so
                            -- equality is independent of their chosen padding.
                            let entry = if rx == MSet_ExplicitWithFlags
                                    then [essence| [toInt(&active), -&value * toInt(&active), &flags[&i]] |]
                                    else [essence| [toInt(&active), -&value * toInt(&active)] |]
                            return [essence| flatten([&entry | &iPat : &ix]) |]
                    kx <- key xs
                    ky <- key ys
                    return $ mk kx ky
            )


rule_BoolInt :: Rule
rule_BoolInt = "tildeOrd-bool-int" `namedRule` theRule where
    theRule [essence| &x ~< &y |] = do
        tyx <- typeOf x
        tyy <- typeOf y
        case mostDefined [tyx, tyy] of
            TypeBool  -> return ()
            TypeInt _ -> return ()
            _ -> na "rule_BoolInt"
        return
            ( "~< to <"
            , return [essence| &x < &y |]
            )
    theRule [essence| &x ~<= &y |] = do
        tyx <- typeOf x
        tyy <- typeOf y
        case mostDefined [tyx, tyy] of
            TypeBool  -> return ()
            TypeInt _ -> return ()
            _ -> na "rule_BoolInt"
        return
            ( "~<= to <="
            , return [essence| &x <= &y |]
            )
    theRule _ = na "rule_BoolInt"


rule_MSet :: Rule
rule_MSet = "tildeLt-mset" `namedRule` theRule where
    theRule [essence| &x ~< &y |] = do
        tyX <- typeOf x
        tyY <- typeOf y
        case mostDefined [tyX, tyY] of
            TypeMSet{} -> return ()
            _ -> na "rule_MSet"
        return
            ( "mset ~<"
            , do
                (iPat, i) <- quantifiedVar
                (jPat, j) <- quantifiedVar
                let z = [essence| &x union &y |]
                -- there exists an i, where freq_x is smaller than freq_y
                -- and all j's (s.t. j<i), freq_x = freq_y
                -- i.e. all those that are smaller than the ith occur equal nb times
                return [essence|
                    exists &iPat in &z .
                        freq(&x, &i) < freq(&y, &i) /\
                        (forAll &jPat in &z , &j ~< &i . freq(&x, &j) = freq(&y, &j))
                               |]
            )
    theRule _ = na "rule_MSet"


rule_ViaMSet :: Rule
rule_ViaMSet = "tildeLt-via-mset" `namedRule` theRule where
    theRule [essence| &x ~< &y |] = do
        tyX <- typeOf x
        tyY <- typeOf y
        f   <- case mostDefined [tyX, tyY] of
            TypeSet{}       -> return $ \ i ->
                case match opToSetWithFlag i of
                    -- if i is a toSet, that doesn't contain any duplicates anyway, stip the toSet
                    Just (True, j) -> [essence| toMSet(&j) |]
                    _              -> [essence| toMSet(&i) |]
            TypeFunction{}  -> return $ \ i -> [essence| toMSet(&i) |]
            TypeRelation{}  -> return $ \ i -> [essence| toMSet(&i) |]
            TypePartition{} -> return $ \ i -> [essence| toMSet(parts(&i)) |]
            _               -> na "rule_ViaMSet"
        let fx = f x
        let fy = f y
        return
            ( "set, function, relation, partition ~<"
            , return [essence| &fx ~< &fy |]
            )
    theRule _ = na "rule_ViaMSet"


rule_TildeLeq :: Rule
rule_TildeLeq = "tildeLeq" `namedRule` theRule where
    theRule [essence| &x ~<= &y |] = do
        tyX <- typeOf x
        tyY <- typeOf y
        case mostDefined [tyX, tyY] of
            TypeSet{}       -> return ()
            TypeMSet{}      -> return ()
            TypeFunction{}  -> return ()
            TypeRelation{}  -> return ()
            TypePartition{} -> return ()
            _               -> na "rule_TildeLeq"
        return
            ( "~<= to ~<"
            , return [essence| or([ &x = &y
                                  , &x ~< &y
                                  ])
                             |]
            )
    theRule _ = na "rule_TildeLeq"
