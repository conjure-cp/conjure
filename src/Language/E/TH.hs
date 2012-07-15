{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.E.TH where

import Stuff.Generic
import Language.E.Definition
import Language.E.Parser

-- template-haskell
import Language.Haskell.TH ( Exp(..), Pat(..), Lit(..), mkName )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

-- import qualified GHC.Types
import qualified Data.Text as T


eMatch :: QuasiQuoter
eMatch = qq {
    quotePat = \ inp -> do
        let
            -- buildP [xMatch| [Prim (S t)] := hsTerm |] = 
            --     case parsePat t of
            --         Left  e -> error e
            --         Right p -> return p
            
            buildP' (B False)
                = ConP (mkName "B")
                    [ ConP (mkName "False") [] ]
            buildP' (B True)
                = ConP (mkName "B")
                    [ ConP (mkName "True") [] ]
            buildP' (I i)
                = ConP (mkName "I")
                    [ LitP (IntegerL i) ]
            buildP' (S s)
                = ConP (mkName "S")
                    [ LitP (StringL s) ]
            -- buildP' s = error ("buildP': " ++ show s)

            buildP (Prim p) = do
                return $ ConP (mkName "Prim") [buildP' p]
            buildP (Tagged "metavar" [Prim (S s)]) = do
                return $ VarP (mkName s)
            buildP (Tagged t xs) = do
                ys <- mapM buildP xs
                return $ ConP (mkName "Tagged")
                              [ LitP (StringL t)
                              , ListP ys
                              ]
            -- buildP s = [p| s |]
            -- buildP s = error ("buildP: " ++ show s)
        case runLexerAndParser (inCompleteFile parseExpr) "" (T.pack inp) of
            Left  e -> error $ show e
            Right x -> buildP x
        -- let inps = splitOn "|" inp
        -- let each i = do
        --         let [patternS, tag] = splitOn ":=" i
        --         let stripped = strip tag
        --         -- paths <- runIO $ readFile "tags.txt"
        --         -- let flag = stripped `elem` lines paths
        --         let flag = True
        --         if flag
        --             then do
        --                 let tags = splitOn "." stripped
        --                 case parsePat patternS of
        --                     Left  e -> error e
        --                     Right p -> do
        --                         tags' <- [e| tags |]
        --                         return (tags', p)
        --             else error $ "No such tag: " ++ stripped
        -- xs <- mapM each inps
        -- let lhs = AppE (VarE  $ mkName "viewTaggeds")
        --                (ListE $ map fst xs)
        -- let rhs = ListP $ map (\ i -> ConP (mkName "Just") [i] ) $ map snd xs
        -- return (ViewP lhs rhs)
    }

eMake :: QuasiQuoter
eMake = qq {
    quoteExp = \ inp -> do
        let

            build' (B False)
                = ConE (mkName "B")
                    `AppE`
                  ConE (mkName "False")
            build' (B True)
                = ConE (mkName "B")
                    `AppE`
                  ConE (mkName "True")
            build' (I i)
                = ConE (mkName "I")
                    `AppE`
                  LitE (IntegerL i)
            build' (S s)
                = ConE (mkName "S")
                    `AppE`
                  LitE (StringL s)
            -- build' s = error ("build': " ++ show s)

            build (Prim p) = do
                return $ ConE (mkName "Prim") `AppE` build' p
            build (Tagged "metavar" [Prim (S s)]) = do
                return $ VarE (mkName s)
            build (Tagged s xs) = do
                ys <- mapM build xs
                return $ ConE (mkName "Tagged")
                            `AppE`
                         LitE (StringL s)
                            `AppE`
                         ListE ys
            -- build s = error ("build: " ++ show s)
        case runLexerAndParser (inCompleteFile parseExpr) "" (T.pack inp) of
            Left  e -> error $ show e
            Right x -> build x
    }

-- xMake :: QuasiQuoter
-- xMake = qq {
--     quoteExp = \ inp -> do
--         let inps = splitOn "|" inp
--         let
--             each :: String -> Q Exp
--             each i = do
--                 let [lhs,rhs] = splitOn ":=" i
--                 let stripped = strip lhs
--                 let tags = splitOn "." stripped
--                 case parseExp rhs of
--                     Left  e -> error  $ "Malformed expression: " ++ e
--                     Right x -> do
--                         runIO $ appendFile "tags.txt" $ stripped ++ "\n"
--                         return $ mkTaggedTH tags x
--         xs <- mapM each inps
--         case mergeTaggedTH xs of
--             [x] -> return x
--             _   -> error "These do not seem to have a commmon root."
--     }
