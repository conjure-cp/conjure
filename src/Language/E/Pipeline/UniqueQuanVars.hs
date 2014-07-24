{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.UniqueQuanVars
    ( uniqueQuanVars
    ) where

import Language.E

import Control.Monad.State ( evalState )
import qualified Data.Text as T


uniqueQuanVars :: Spec -> Spec
uniqueQuanVars (Spec v x) = Spec v x'

    where

        x' = evalState (f x) [ 0 :: Int .. ]

        f p@[xMatch| [quanVar] := quantified.quanVar |]
            | let toReplace = collectQuanVars quanVar
            , not $ and [ "q_" `T.isPrefixOf` r | r <- toReplace ]
            = do
                lu <- forM toReplace $ \ t -> do
                    q <- nextQ
                    return (t,q)
                let p' = replaceIdentifiers lu p
                f p'
        f (Tagged t xs) = Tagged t <$> mapM f xs
        f t = return t

        nextQ = do
            q <- gets head
            modify tail
            return $ stringToText $ "q_" ++ show q

        replaceIdentifiers these = transform $ \ i -> case i of
            Prim (S nm) ->
                case nm `lookup` these of
                    Nothing  -> Prim (S nm)
                    Just nm' -> Prim (S nm')
            _ -> i
