{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}

module Stuff.Generic.Definition
    ( Generic(..)
    , xMake, xMatch, viewTaggeds, viewTagged
    , prettyAsTree, prettyAsPaths
    , qq
    ) where

import Control.Arrow ( first )
import Data.List ( intersperse )
import Data.Maybe ( fromJust, isJust )
import Data.Generics ( Data, Typeable )

-- split
import Data.List.Split ( splitOn )

-- template-haskell
import Language.Haskell.TH ( Q, Exp(..), Pat(..), Lit(..), mkName )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

-- haskell-src-meta
import Language.Haskell.Meta.Parse.Careful

-- pretty printing
import Stuff.Pretty ( Pretty(..) )
import Text.PrettyPrint ( Doc, ($+$), (<+>), hcat, vcat, nest )

-- text
import qualified Data.Text as T


data Generic primitive
    = Prim primitive
    | Tagged T.Text [Generic primitive]
    deriving (Eq, Ord, Read, Show, Data, Typeable)

prettyAsTree :: Pretty primitive => Generic primitive -> Doc
prettyAsTree (Prim p) = pretty p
prettyAsTree (Tagged tag xs) = pretty tag $+$ vcat (map (nest 4 . prettyAsTree) xs)

prettyAsPaths :: Pretty primitive => Generic primitive -> Doc
prettyAsPaths = vcat . map pOne . toPaths
    where
        pOne (ts,Nothing) = hcat (map pretty $ intersperse "." ts) <+> ":= []"
        pOne (ts,Just p ) = hcat (map pretty $ intersperse "." ts) <+> ":=" <+> pretty p

        toPaths :: Generic primitive -> [([T.Text],Maybe primitive)]
        toPaths (Prim p) = [([], Just p)]
        toPaths (Tagged s []) = [([s],Nothing)]
        toPaths (Tagged s xs) = map (first (s:)) (concatMap toPaths xs)


mkTaggedTH :: [String] -> Exp -> Exp
mkTaggedTH []     _ = error "mkTaggedTH"
mkTaggedTH [t]    g = AppE (AppE (ConE (mkName "Tagged")) (LitE (StringL t))) g
mkTaggedTH (t:ts) g = AppE (AppE (ConE (mkName "Tagged")) (LitE (StringL t))) (ListE [mkTaggedTH ts g])

mergeTaggedTH :: [Exp] -> [Exp]
mergeTaggedTH []     = error "mergeTaggedTH"
mergeTaggedTH [g]    = [g]
mergeTaggedTH gs = 
    let
        extract (AppE (AppE (ConE conName) (LitE (StringL t))) (ListE xs))
            | conName == mkName "Tagged"
            = (t, xs)
        extract (AppE (AppE (ConE conName) (LitE (StringL t))) xs)
            | conName == mkName "Tagged"
            = (t, [xs])
        extract x = error $ "extract: " ++ show x
        gs'    = map extract gs
        tag    = fst $ head gs'
        merged = mergeTaggedTH (concatMap snd gs')
    in  if all (tag==) (map fst gs')
            then [AppE (AppE (ConE (mkName "Tagged")) (LitE (StringL tag))) (ListE merged)]
            else gs

strip :: String -> String
strip = filter (`notElem` " \n\t")

qq :: QuasiQuoter
qq = QuasiQuoter { quoteExp  = error "not implemented"
                 , quoteType = error "not implemented"
                 , quotePat  = error "not implemented"
                 , quoteDec  = error "not implemented"
                 }

xMatch :: QuasiQuoter
xMatch = qq {
    quotePat = \ inp -> do
        let inps = splitOn "|" inp
        let each i = do
                let [patternS, tag] = splitOn ":=" i
                let tags = splitOn "." $ strip tag
                case parsePat patternS of
                    Left  e -> error e
                    Right p -> do
                        tags' <- [e| tags |]
                        return (tags', p)
        xs <- mapM each inps
        let lhs = AppE (VarE  $ mkName "viewTaggeds")
                       (ListE $ map fst xs)
        let rhs = ListP $ map (\ (_,i) -> ConP (mkName "Just") [i] ) xs
        return (ViewP lhs rhs)
    }

xMake :: QuasiQuoter
xMake = qq {
    quoteExp = \ inp -> do
        let inps = splitOn "|" inp
        let
            each :: String -> Q Exp
            each i = do
                let [lhs,rhs] = splitOn ":=" i
                let stripped = strip lhs
                let tags = map strip $ splitOn "." stripped
                case parseExp rhs of
                    Left  e -> error  $ "Malformed expression: " ++ e
                    Right x ->
                        -- runIO $ appendFile "tags.txt" $ stripped ++ "\n"
                        return $ mkTaggedTH tags x
        xs <- mapM each inps
        case mergeTaggedTH xs of
            [x] -> return x
            _   -> error "These do not seem to have a commmon root."
    }

viewTagged :: Show primitive => [T.Text] -> Generic primitive -> Maybe [Generic primitive]
viewTagged [] g = Just [g]
viewTagged [t] (Tagged i []) | t == i = Just []
viewTagged (t:ts) (Tagged i xs) | t == i = do
    let justs = filter isJust $ map (viewTagged ts) xs
    if null justs
        then Nothing
        else return (concatMap fromJust justs)
viewTagged _ _ = Nothing

viewTaggeds :: Show primitive => [[T.Text]] -> Generic primitive -> [Maybe [Generic primitive]]
viewTaggeds as g = map (`viewTagged` g) as
