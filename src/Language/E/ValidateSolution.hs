{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.ValidateSolution ( validateSolution ) where

import qualified Data.HashMap.Strict as M

import Language.E
import Language.E.Pipeline.InlineLettings
import Language.E.Pipeline.AtMostOneSuchThat


type Essence  = Spec
type Param    = Maybe Spec
type Solution = Spec

validateSolution :: Essence -> Param -> Solution -> IO ()
validateSolution essence param solution = do
    let (mresult, _logs) = runCompESingle "validating solution" helper
    case mresult of
        Left  x     -> error $ renderPretty x
        Right False -> error "Not a valid solution"
        Right True  -> return ()

    where
        Spec language essenceStmt = essence

        inliner bindersMap (Spec v s) = Spec v (f s)
            where
                f [xMatch| [Prim (S x)] := reference |]
                    | Just y <- M.lookup x bindersMap
                    = f y
                f (Tagged t xs)
                    = Tagged t (map f xs)
                f x = x

        helper = do
            case param of
                Nothing -> return ()
                Just (Spec _ s) -> mapM_ introduceStuff (statementAsList s)
            -- bindersDoc >>= mkLog "binders 1"

            case solution of
                Spec _ s        -> mapM_ introduceStuff (statementAsList s)
            -- bindersDoc >>= mkLog "binders 2"

            let declsStripped = Spec language $ listAsStatement
                    [ i
                    | i <- statementAsList essenceStmt
                    , case i of
                        [xMatch| _ := topLevel.declaration|] -> False
                        _ -> True
                    ]
            lettingsInlined <- inlineLettings declsStripped
            fullyInlined    <- do bs <- gets binders
                                  let bsMap = M.fromList [ (nm, val) | Binder nm val <- bs ]
                                  return $ inliner bsMap lettingsInlined
            Spec _ s <- fmap atMostOneSuchThat $ simplifySpec fullyInlined
            -- mkLog "debug stripped"           (pretty declsStripped)
            -- mkLog "debug lettingsInlined"    (pretty lettingsInlined)
            -- mkLog "debug fullyInlined"       (pretty fullyInlined)
            -- mkLog "debug simplified"         (pretty simplified)
            case statementAsList s of
                [ [xMatch| [Prim (B b)] := topLevel.suchThat.value.literal |] ] -> return b
                _ -> error $ renderPretty $ vcat [ "Cannot fully evaluate."
                                                 , pretty s
                                                 , prettyAsTree s
                                                 , prettyAsPaths s
                                                 ]

