{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.ValidateSolution ( validateSolution ) where

import qualified Data.HashMap.Strict as M

import Language.E
import Language.E.Pipeline.InlineLettings
import Language.E.Pipeline.ExplodeStructuralVars
import Language.E.Pipeline.AtMostOneSuchThat


type Essence  = Spec
type Param    = Maybe Spec
type Solution = Spec

validateSolution :: Essence -> Param -> Solution -> IO ()
validateSolution essence@(Spec language _) param solution = do
    let (mresult, _logs) = runCompESingle "validating solution" helper
    printLogs _logs
    case mresult of
        Left  x     -> error $ renderPretty x
        Right False -> error "Not a valid solution."
        Right True  -> return ()

    where
        getDecls (Spec _ x) =
            [ (nm, dom)
            | [xMatch| [Prim (S nm)] := topLevel.declaration.find.name.reference
                     | [dom]         := topLevel.declaration.find.domain
                     |] <- statementAsList x
            ] ++
            [ (nm, dom)
            | [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                     | [dom]         := topLevel.declaration.given.domain
                     |] <- statementAsList x
            ]

        inliner typesMap bindingsMap (Spec v s) = Spec v (f s)
            where
                f x@[xMatch| [Prim (S nm)] := reference |]
                    = case (M.lookup nm typesMap, M.lookup nm bindingsMap) of
                        (Just theType, Just binding) ->
                            f [xMake| typed.left  := [binding]
                                    | typed.right := [theType]
                                    |]
                        (_, Just _) -> error $ show $ "Cannot determine the type of:" <+> pretty nm
                        _           -> x
                f x@[xMatch| [_] := structural.single.reference |] = x
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

            lettingsInlined <- inlineLettings essence
            -- mkLog "debug lettingsInlined"    (pretty lettingsInlined)

            let declsStripped = Spec language $ listAsStatement
                    [ i
                    | let Spec _ stmt = lettingsInlined
                    , i <- statementAsList stmt
                    , case i of
                        [xMatch| _ := topLevel.declaration |] -> False
                        _ -> True
                    ]
            -- mkLog "debug stripped"           (pretty declsStripped)


            fullyInlined    <- do bs <- gets binders
                                  typesMap <- fmap M.fromList
                                        $ forM (getDecls lettingsInlined)
                                        $ \ (nm, dom) -> do
                                            itsType <- typeOf dom
                                            return (nm, itsType)
                                  let bindingsMap = M.fromList [ (nm, val) | Binder nm val <- bs ]
                                  return $ inliner typesMap bindingsMap declsStripped
            -- mkLog "debug fullyInlined"       (pretty fullyInlined)

            Spec _ s <- fullyEvaluate fullyInlined
            -- simplified@(Spec _ s) <- fullyEvaluate fullyInlined
            -- mkLog "debug simplified"         (pretty simplified)

            let checks = map isPartOfValidSolution (statementAsList s)
            if all isJust checks
                then return (and $ catMaybes checks)
                else error $ renderPretty $ vcat [ "Cannot fully evaluate."
                                                 , pretty s
                                                 , prettyAsTree s
                                                 , prettyAsPaths s
                                                 ]


isPartOfValidSolution :: E -> Maybe Bool
isPartOfValidSolution [xMatch| [Prim (B b)] := topLevel.suchThat.value.literal |] = Just b
isPartOfValidSolution [xMatch| [Prim (B b)] := topLevel.where   .value.literal |] = Just b
isPartOfValidSolution [xMatch| _ := topLevel.objective |] = Just True
isPartOfValidSolution _ = Nothing

fullyEvaluate :: MonadConjure m => Spec -> m Spec
fullyEvaluate
    = return
    >=> recordSpec >=> explodeStructuralVars
    >=> recordSpec >=> fullySimplifySpec
    >=> recordSpec >=> return . atMostOneSuchThat


