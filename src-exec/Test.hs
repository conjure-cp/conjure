{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Stuff.Generic
import Stuff.Pretty
import Stuff.NamedLog
import Stuff.FunkyT
import Stuff.MonadList

import Language.E

import Text.Show.Pretty
import System.Environment
import Data.List

-- test [xMatch| attrs   := domain.set.attributes
--            |]
--     = ppShow attrs
-- test [xMatch| [inner] := domain.set.inner
--            |]
--     = ppShow inner
-- test [xMatch| attrs   := domain.set.attributes
--            | [inner] := domain.set.inner
--            |]
--     = ppShow (attrs, inner)
-- test _ = "catch all"

-- viewTaggeds [["domain", "set", "attributes"], ["domain", "set", "inner"]]


testIt :: Monad m => (FilePath, Text) -> FunkyT LocalState GlobalState CompError m Spec
testIt spectobe = do
    spec <- readSpec spectobe
    let op = fooSpec
    -- let op = fooSpec >=> fooSpec >=> (\ i -> returns [spec] )
    -- let op = fooSpec >=> fooSpec >=> (\ i -> FunkyT $ \ l g -> return ([(Right spec,l)],g) )
    op spec

-- readSpec :: (Functor m, Monad m)
--     => (FilePath, Text)
--     -> FunkyT m Spec
readSpec (fp,con) =
    case runLexerAndParser parseSpec fp con of
        Left  e -> throwError (ErrFatal, e)
        Right x -> return x

fooSpec s = do
    mkLog' "fooSpec" "ahem"
    s' <- traverseSpec Nothing foo Nothing s
    returns [s,s']
-- fooSpec (Spec v xs) = Spec v <$> mapM foo xs

foo [xMatch| [Prim (I i)] := value.literal |] = return [xMake| value.literal := [Prim $ I $ i + 1] |]
foo x = do
    -- mkLog' "foo" $ pretty x
    return x

mkLog' a b = modifyGlobal $ \ st -> st { logs = logs st ++ [NamedLog a b] } 


main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."

    spec    <- pairWithContents specFilename

    let
        (mgenerateds, globalSt) = runFunky def def $ testIt spec
        errors     = [ x | (Left  x, _ ) <- mgenerateds ]
        generateds = [ x | (Right x, _ ) <- mgenerateds ]
    print $ prettyLogs $ logs globalSt
    unless (null errors)
        $ error
        $ show
        $ prettyErrors "There were errors in at least one branch." errors

    putStrLn ""
    putStrLn "[ === Generated === ]"
    putStrLn ""

    mapM_ (print . pretty) generateds













-- labelOf :: Pretty primitive => Generic primitive -> Doc
-- labelOf (Prim   p  ) = pretty p
-- labelOf (Tagged s _) = stringToDoc s
-- 
-- addBinder' nm val = do
--     case nm of
--         '@':_ -> return ()
--         _ -> mkLog' "addBinder" $ stringToDoc nm
--     modifyLocal $ \ st -> st { binders = Binder nm val : binders st }
-- 
-- 
-- -- traverseSpec :: MonadWriter [NamedLog] m
-- --     => Maybe (Generic BuiltIn -> m (Generic BuiltIn))
-- --     -> (Generic BuiltIn -> m (Generic BuiltIn))
-- --     -> Maybe (Generic BuiltIn -> m (Generic BuiltIn))
-- --     -> Spec
-- --     -> m Spec
-- traverseSpec mpre func mpost (Spec v xs) = do
--     -- forM_ xs $ \ x -> mkLog' "debug" $ prettyAsPaths x
--     xs' <- mapM (traverse mpre func mpost) xs
--     return $ Spec v xs'
-- 
-- 
-- -- traverse :: (MonadWriter [NamedLog] m, Pretty primitive)
-- --     => Maybe (Generic primitive -> m (Generic primitive))
-- --     -> (Generic primitive -> m (Generic primitive))
-- --     -> Generic primitive
-- --     -> m (Generic primitive)
-- traverse mpre func mpost t = do
--     -- mkLog' " ==>    " $ labelOf t
--     -- printAllBound "1"
--     bindersBefore <- getsLocal binders
--     introduceStuff t
--     result <- case mpre of
--         Nothing  -> do
--             -- mkLog' "traverse" "no preorder modification"
--             afterPre mpre func mpost t
--         Just pre -> do
--             t' <- pre t
--             -- mkLog' "traverse" $ "after pre:" <+> labelOf t
--             afterPre mpre func mpost t'
--     modifyLocal $ \ st -> st { binders = bindersBefore }
--     -- printAllBound "2"
--     return result
-- 
-- 
-- -- afterPre :: (MonadWriter [NamedLog] m, Pretty primitive)
-- --     => Maybe (Generic primitive -> m (Generic primitive))
-- --     -> (Generic primitive -> m (Generic primitive))
-- --     -> Generic primitive
-- --     -> m (Generic primitive)
-- afterPre mpre func mpost t = do
--     -- mkLog' "afterPre" $ labelOf t
--     t' <- case t of
--         Tagged s xs -> do
--             xs' <- mapM (traverse mpre func mpost) xs
--             func (Tagged s xs')
--         _ -> func t
--     -- mkLog' "    ==> " $ labelOf t'
--     -- printAllBound ()
--     case mpost of 
--         Nothing -> do
--             -- mkLog' "traverse" "no postorder modification"
--             return t'
--         Just post -> do
--             t'' <- post t'
--             -- mkLog' "traverse" $ "after post:" <+> labelOf t''
--             return t''
-- 
-- 
-- introduceStuff
--     s@[xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
--              | [      _      ] := topLevel.declaration.find.domain
--              |] = do
--                  mkLog' "introduceStuff" $ "find" <+> stringToDoc name
--                  addBinder' name s
-- introduceStuff
--     s@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference
--              | [      _      ] := topLevel.declaration.given.domain
--              |] = addBinder' name s
-- introduceStuff
--       [xMatch| [Prim (S name)] := topLevel.declaration.letting.name.reference
--              | [ expression ]  := topLevel.declaration.letting.expr
--              |] = addBinder' name expression
-- introduceStuff
--     [xMatch| ls := withLocals.locals |] = do
--         mapM_ introduceStuff ls
--         mkLog' "introduceStuff" $ vcat $ map pretty ls
-- introduceStuff _ = do
--     -- mkLog' "introduceStuff" $ pretty x
--     return ()
-- 
-- 
-- printAllBound s = do
--     bs <- getsLocal binders
--     let names = [ s | Binder s _ <- bs ]
--     mkLog' ("----- ----- ----- ----- " ++ s) $ prettyList id "," names
-- 
