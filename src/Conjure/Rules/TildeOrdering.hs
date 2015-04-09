{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.TildeOrdering where

import Conjure.Rules.Import


rule_BoolInt :: Rule
rule_BoolInt = "tildeOrd-bool-int" `namedRule` theRule where
    theRule [essence| &x ~< &y |] = do
        tyx <- typeOf x
        tyy <- typeOf y
        case mostDefined [tyx, tyy] of
            TypeBool -> return ()
            TypeInt  -> return ()
            _ -> na "rule_BoolInt"
        return
            ( "~< to <"
            , return [essence| &x < &y |]
            )
    theRule [essence| &x ~<= &y |] = do
        tyx <- typeOf x
        tyy <- typeOf y
        case mostDefined [tyx, tyy] of
            TypeBool -> return ()
            TypeInt  -> return ()
            _ -> na "rule_BoolInt"
        return
            ( "~<= to <="
            , return [essence| &x <= &y |]
            )
    theRule _ = na "rule_BoolInt"


rule_MSet :: Rule
rule_MSet = "tildeOrd-mset" `namedRule` theRule where
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
                -- and all j's (s.t >i), freq_x = freq_y
                return [essence|
                    exists &iPat in &z .
                        freq(&x, &i) < freq(&y, &i) /\
                        (forAll &jPat in &z , &j > &i . freq(&x, &j) = freq(&y, &j))
                               |]
            )
    theRule [essence| &x ~<= &y |] = do
        tyx <- typeOf x
        tyy <- typeOf y
        case mostDefined [tyx, tyy] of
            TypeMSet{} -> return ()
            _ -> na "rule_MSet"
        return
            ( "mset ~<="
            , return [essence| or([ &x = &y
                                  , &x ~< &y
                                  ])
                             |]
            )
    theRule _ = na "rule_MSet"


