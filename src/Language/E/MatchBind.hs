{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.MatchBind where

import Stuff.Generic
import Stuff.MetaVariable
import Stuff.FunkyT

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Pretty
import Language.E.Parser ( lexAndParseIO, parseExpr, inCompleteFile )

import Data.Set as S ( fromList, toList )
import qualified Data.Text as T


sort :: Ord a => [a] -> [a]
sort = S.toList . S.fromList

patternMatch :: Monad m => E -> E -> CompE m Bool
patternMatch pattern actual = do
    flag <- core pattern actual
    if flag
        then do
            mkLog "patternMatch" $ vcat ["(successful)", pretty pattern, pretty actual]
            return ()
        else do
            mkLog "patternMatch" $ vcat ["(failed)"    , pretty pattern, pretty actual]
            return ()
    return flag
    where
        core [xMatch| [] := type.unknown |] y = do
            mkLog "patternMatch.core" $ vcat ["type.unknown", "~~", pretty y]
            return True
        core (Prim x) (Prim y) =
            if x == y
                then do
                    mkLog "patternMatch.core" $ vcat [ "same literal"
                                                     , pretty x
                                                     , "~~"
                                                     , pretty y
                                                     ]
                    return True
                else do
                    mkLog "patternMatch.core" $ vcat [ "literals not equal"
                                                     , pretty x
                                                     , "~~"
                                                     , pretty y
                                                     ]
                    return False
        core p _ | unnamedMV p =
            return True
        core p x | Just nm <- namedMV p = do
            addBinder ('&':nm) x
            return True
        core [xMatch| xs := attrCollection |]
             [xMatch| ys := attrCollection |]
            = do
                let dontcare    = [xMake| attribute.dontCare := [] |]
                let xs'       = filter (/= dontcare) xs
                let hasDontcare = xs /= xs'
                if not hasDontcare
                    then do
                        let xsOrdered = sort xs'
                        let ysOrdered = sort ys
                        core [xMake| nowOrdered := xsOrdered |]
                             [xMake| nowOrdered := ysOrdered |]
                    else do
                        let foo _ [] = return False
                            foo j (i:is) = do
                                b <- core j i
                                if b then return True else foo j is
                        bs <- forM xs' $ \ x -> foo x ys
                        return $ and bs
        core _x@(Tagged xTag xArgs) _y@(Tagged yTag yArgs) =
            case (xTag == yTag, length xArgs == length yArgs) of
                (True, True) -> do
                    -- mkLog "patternMatch.core" $ "same tree label & equal number of subtrees. great" <+> pretty xTag
                    bs <- zipWithM core xArgs yArgs
                    if and bs
                        then
                            -- mkLog "patternMatch.core" $ vcat [ "all subtrees matched fine"
                            --                                  , pretty _x
                            --                                  , "~~"
                            --                                  , pretty _y
                            --                                  ]
                            return True
                        else do
                            mkLog "patternMatch.core" $ vcat [ "some subtrees didn't match"
                                                             , pretty _x
                                                             , "~~"
                                                             , pretty _y
                                                             ]
                            return False
                (False, _) -> do
                    mkLog "patternMatch.core" $ vcat [ "different tree labels"
                                                     , pretty xTag
                                                     , "~~"
                                                     , pretty yTag
                                                     ]
                    return False
                (_, False) -> do
                    mkLog "patternMatch.core" $ vcat [ "different number of subtrees"
                                                     , pretty _x
                                                     , "~~"
                                                     , pretty _y
                                                     ]
                    return False
        core _ _ =
            -- mkLog "patternMatch.core" $ vcat [ "this just fails"
            --                                  , pretty x
            --                                  , "~~"
            --                                  , pretty y
            --                                  ]
            return False


-- if this returns nothing, that means there is some unbound reference.
patternBind :: (Functor m, Monad m) => E -> MaybeT (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) E
patternBind x | Just nm <- namedMV x = do
    res <- lookupBinder ('&':nm)
    patternBind res
patternBind (Tagged xTag xArgs) = Tagged xTag <$> mapM patternBind xArgs
patternBind x = return x


test_Match :: String -> String -> IO ()
test_Match patternText actualText = do
    pattern <- lexAndParseIO (inCompleteFile parseExpr) (T.pack patternText)
    actual  <- lexAndParseIO (inCompleteFile parseExpr) (T.pack actualText)
    void $ runCompE $ do
        flag <- patternMatch pattern actual
        bs   <- getsLocal binders
        forM_ bs $ \ (Binder nm val) -> liftIO $ do
            putStr nm
            putStr " : "
            putStrLn $ renderPretty val
        if flag
            then liftIO $ putStrLn "Matched."
            else liftIO $ putStrLn "Not matched."
