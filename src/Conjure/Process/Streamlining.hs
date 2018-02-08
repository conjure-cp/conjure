{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Streamlining where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.TypeOf ( typeOf )
import Conjure.Compute.DomainOf ( domainOf )


streamlining :: (MonadFail m, MonadLog m, MonadUserError m, NameGen m) => Model -> m ()
streamlining model =
    forM_ (mStatements model) $ \ statement ->
        case statement of
            Declaration (FindOrGiven Find nm domain) -> do
                let ref = Reference nm (Just (DeclNoRepr Find nm domain NoRegion))
                streamliners <- streamlinersForSingleVariable ref
                traceM $ show $ vcat [ "Streamliners for --" <+> pretty statement
                                     , vcat [ nest 4 (pretty s) | s <- streamliners ]
                                     ]
            _ -> return ()



type Streamliner = [Expression]

type StreamlinerGen m = Expression -> m Streamliner


-- given a reference to a top level variable, produce a list of all applicable streamliners
streamlinersForSingleVariable :: (MonadFail m, NameGen m) => StreamlinerGen m
streamlinersForSingleVariable x = concatMapM ($ x)
    [ intOdd
    , intEven
    , setAll streamlinersForSingleVariable
    ]


-- given an integer expression (which can be a reference to a decision variable),
-- generate a constraint forcing it to be odd
intOdd :: MonadFail m => StreamlinerGen m
intOdd x = do
    ty <- typeOf x
    if typeUnify ty TypeInt
        then return $ return [essence| &x % 2 = 1 |]
        else return []


intEven :: MonadFail m => StreamlinerGen m
intEven x = do
    ty <- typeOf x
    if typeUnify ty TypeInt
        then return $ return [essence| &x % 2 = 0 |]
        else return []


setAll :: (MonadFail m, NameGen m) => StreamlinerGen m -> StreamlinerGen m
setAll innerStreamliner x = do
    -- traceM $ show $ "setAll" <+> pretty x
    dom <- domainOf x
    -- traceM $ show $ "setAll dom" <+> pretty dom
    case dom of
        DomainSet _ _ innerDom -> do
            nm <- nextName "q"
            -- traceM $ show $ "setAll nm" <+> pretty nm
            let pat = Single nm
                ref = Reference nm (Just (DeclNoRepr Find nm innerDom NoRegion))
            innerConstraints <- innerStreamliner ref
            -- traceM $ show $ "maybeInnerConstraint" <+> vcat (map pretty maybeInnerConstraint)
            forM innerConstraints $ \ innerConstraint -> 
                    return [essence| forAll &pat in &x . &innerConstraint |]
        _ -> return []

