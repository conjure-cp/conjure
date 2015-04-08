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
        tyx <- typeOf x
        tyy <- typeOf y
        case mostDefined [tyx, tyy] of
            TypeMSet{} -> return ()
            _ -> na "rule_MSet"
        return
            ( "~< to <"
            , return [essence| or([ |&x| = 0 /\ |&y| > 0
                                  , max(&x) < max(&y)
                                  , max(&x) = max(&y) /\ ((&x - mset(max(&x))) ~< (&y - mset(max(&y))))
                                  ])
                             |]
            )
    theRule [essence| &x ~<= &y |] = do
        tyx <- typeOf x
        tyy <- typeOf y
        case mostDefined [tyx, tyy] of
            TypeMSet{} -> return ()
            _ -> na "rule_MSet"
        return
            ( "~< to <"
            , return [essence| or([ &x = &y
                                  , &x ~< &y
                                  ])
                             |]
            )
    theRule _ = na "rule_MSet"


