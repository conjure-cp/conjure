{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.E.MatchBind where

import Stuff.Pretty
import Stuff.Generic
import Stuff.MetaVariable
import Stuff.NamedLog
import Stuff.FunkyT
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty ()
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
            -- mkLog "patternMatch" $ vcat ["(failed)"    , pretty pattern, pretty actual]
            return ()
    return flag
    where
        core (Prim x) (Prim y) =
            if x == y
                then do
                    mkLog "patternMatch.core" $ "same literal" <+> pretty x <+> "~~" <+> pretty y
                    return True
                else do
                    mkLog "patternMatch.core" $ "literals not equal" <+> pretty x <+> "~~" <+> pretty y
                    return False
        core p _ | unnamedMV p = do
            return True
        core p x | Just nm <- namedMV p = do
            addBinder ('@':nm) x
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
                        if and bs
                            then do
                                return True
                            else do
                                return False
        core _x@(Tagged xTag xArgs) _y@(Tagged yTag yArgs) = do
            case (xTag == yTag, length xArgs == length yArgs) of
                (True, True) -> do
                    -- mkLog "patternMatch.core" $ "same tree label & equal number of subtrees. great" <+> pretty xTag
                    bs <- zipWithM core xArgs yArgs
                    if and bs
                        then do
                            mkLog "patternMatch.core" $ "all subtrees matched fine" <+> pretty _x <+> "~~" <+> pretty _y
                            return True
                        else do
                            mkLog "patternMatch.core" $ "some subtrees didn't match" <+> pretty _x <+> "~~" <+> pretty _y
                            return False
                (False, _) -> do
                    mkLog "patternMatch.core" $ "different tree labels" <+> pretty xTag <+> "~~" <+> pretty yTag
                    return False
                (_, False) -> do
                    mkLog "patternMatch.core" $ "different number of subtrees" <+> pretty xTag <+> "~~" <+> pretty yTag
                    return False
        core _x _y = do
            -- mkLog "patternMatch.core" $ "this just fails" <+> pretty x <+> "~~" <+> pretty y
            return False


-- patternBind :: (Functor m, Monad m) => E -> MaybeT (CompE m) E
patternBind x | Just nm <- namedMV x = lookupBinder ('@':nm)
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
            putStrLn $ show $ pretty val
        if flag
            then liftIO $ putStrLn "Matched."
            else liftIO $ putStrLn "Not matched."


-- mkFunction :: (Functor m, Monad m) => E -> [E] -> CompE m (E -> MaybeT (CompE m) [E])
-- mkFunction pattern templates = do
--     let patternMetaVars  = S.fromList [ r | Expr ":metavar" [R r] <- universe pattern  ]
--     let templateMetaVars = S.unions [ S.fromList [ r | Expr ":metavar" [R r] <- universe template ]
--                                     | template <- templates
--                                     ]
--     unless (templateMetaVars `S.isSubsetOf` patternMetaVars)
--         $ err undefined
--         $ singletonNested
--         $ vcat [ "Pattern meta variables:"  <+> prettyListDoc id Pr.comma (map showAST $ S.toList patternMetaVars)
--                      , "Template meta variables:" <+> prettyListDoc id Pr.comma (map showAST $ S.toList templateMetaVars)
--                      ]
--     return $ \ x -> do
--         bindersBefore <- gets binders
--         b <- lift $ patternMatch pattern x
--         if b
--             then do
--                 ys <- mapM bind templates
--                 modify $ \ st -> st { binders = bindersBefore }
--                 return ys
--             else do
--                 modify $ \ st -> st { binders = bindersBefore }
--                 mzero
-- 
-- testAsAFunc :: String -> String -> String -> IO ()
-- testAsAFunc pattern' template' x' = do
--     pattern  <- headNote "parsing pattern"  <$> lexAndParseIO (parseExpr <* eof) (T.pack pattern')
--     template <- headNote "parsing template" <$> lexAndParseIO (parseExpr <* eof) (T.pack template')
--     x        <- headNote "parsing x"        <$> lexAndParseIO (parseExpr <* eof) (T.pack x')
--     void $ runCompE $ do
--         f  <- mkFunction pattern [template]
--         my <- runMaybeT (f x)
--         case my of
--             Nothing -> do
--                 error "function returns Nothing"
--             Just ys -> forM_ ys $ \ y -> do
--                 liftIO $ print $ pretty y
--                 -- mkLog "testAsAFunc" (showAST y)
--                 -- return $ showAST y
--                 -- return $ pretty y






-- [x] <- lexAndParseIO (parseDomain <* eof) "set of int(1..9)" 
-- [p] <- lexAndParseIO (parseDomain <* eof) "set of int(@a..9)" 
-- [t] <- lexAndParseIO (parseExpr <* eof) "a*2**a" 
