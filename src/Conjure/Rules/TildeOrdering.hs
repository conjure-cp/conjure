{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.TildeOrdering where

import Conjure.Rules.Import


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
                (kPat, k) <- quantifiedVar
                -- A first differing value with freq_x < freq_y must occur in y.
                -- Check earlier values in x and y separately: duplicates do not
                -- affect universal quantification, so no multiset union is needed.
                return [essence|
                    exists &iPat in &y .
                        freq(&x, &i) < freq(&y, &i) /\
                        (forAll &jPat in &x , &j ~< &i . freq(&x, &j) = freq(&y, &j)) /\
                        (forAll &kPat in &y , &k ~< &i . freq(&x, &k) = freq(&y, &k))
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
