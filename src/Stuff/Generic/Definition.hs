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
import Language.Haskell.TH.Lift ( deriveLift )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )

-- haskell-src-meta
import Language.Haskell.Meta.Parse.Careful

-- pretty printing
import Stuff.Pretty ( Pretty(..) )
import Text.PrettyPrint ( Doc, ($+$), (<+>), hcat, vcat, nest )


data Generic primitive
    = Prim primitive
    | Tagged String [Generic primitive]
    deriving (Eq, Ord, Read, Show, Data, Typeable)

deriveLift ''Generic

prettyAsTree :: Pretty primitive => Generic primitive -> Doc
prettyAsTree (Prim p) = pretty p
prettyAsTree (Tagged tag xs) = pretty tag $+$ vcat (map (nest 4 . prettyAsTree) xs)

prettyAsPaths :: Pretty primitive => Generic primitive -> Doc
prettyAsPaths = vcat . map pOne . toPaths
    where
        pOne (ts,Nothing) = hcat (map pretty $ intersperse "." ts) <+> ":= []"
        pOne (ts,Just p ) = hcat (map pretty $ intersperse "." ts) <+> ":=" <+> pretty p

        toPaths :: Generic primitive -> [([String],Maybe primitive)]
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
                let stripped = strip tag
                -- paths <- runIO $ readFile "tags.txt"
                -- let flag = stripped `elem` lines paths
                let flag = True
                if flag
                    then do
                        let tags = splitOn "." stripped
                        case parsePat patternS of
                            Left  e -> error e
                            Right p -> do
                                tags' <- [e| tags |]
                                return (tags', p)
                    else error $ "No such tag: " ++ stripped
        xs <- mapM each inps
        let lhs = AppE (VarE  $ mkName "viewTaggeds")
                       (ListE $ map fst xs)
        let rhs = ListP $ map (\ i -> ConP (mkName "Just") [i] ) $ map snd xs
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
                    Right x -> do
                        -- runIO $ appendFile "tags.txt" $ stripped ++ "\n"
                        return $ mkTaggedTH tags x
        xs <- mapM each inps
        case mergeTaggedTH xs of
            [x] -> return x
            _   -> error "These do not seem to have a commmon root."
    }

viewTagged :: Show primitive => [String] -> Generic primitive -> Maybe [Generic primitive]
-- viewTagged ts g | trace (show ("viewTagged",ts,g)) False = undefined
viewTagged [] g = Just [g]
-- viewTagged (t:_ ) (Tagged i []) | t == i = Just []
-- viewTagged [] _ = Just []
-- viewTagged (t:_ ) (Tagged i []) | t == i = Nothing
viewTagged [t] (Tagged i []) | t == i = Just []
viewTagged (t:ts) (Tagged i xs) | t == i = do
    let justs = filter isJust $ map (viewTagged ts) xs
    if null justs
        then Nothing
        else return (concat $ map fromJust justs)
viewTagged _ _ = Nothing

-- viewTagged :: Show primitive => [String] -> Generic primitive -> Maybe [Generic primitive]
-- viewTagged [] g = trace "1" $ Just [g]
-- viewTagged (t:ts) (Tagged i []) | t == i = trace ("2i: " ++ show (t,t:ts) ) $ Just []
-- viewTagged (t:ts) (Tagged i xs) | t == i = trace ("2j: " ++ show (t,t:ts) ) $ do
--     let (nothings,justs) = partition isNothing
--                          $ map (\ x -> trace ("calling with " ++ show ts ++ show x) $ viewTagged ts x) xs
--     trace ("nothings: " ++ show nothings) $
--         trace ("justs:" ++ show justs) $
--             if null justs
--                 then trace "THEN" $ Nothing
--                 else trace "ELSE" $ return (concat $ map fromJust justs)
-- viewTagged ts g = trace ("3: " ++ (show ts)) Nothing

viewTaggeds :: Show primitive => [[String]] -> Generic primitive -> [Maybe [Generic primitive]]
viewTaggeds as g = map (`viewTagged` g) as
-- viewTaggeds as g = let out = map (`viewTagged` g) as in
--     trace (ppShow ("viewTaggeds",as,g,out)) out


-- mkTagged :: [String] -> Generic primitive -> Generic primitive
-- mkTagged [] g = g
-- mkTagged (t:ts) g = Tagged t [mkTagged ts g]
-- 
-- mergeTagged :: Generic primitive -> Generic primitive -> Maybe (Generic primitive)
-- mergeTagged (Tagged ta as) (Tagged tb bs) | ta == tb = do
--     inner <- mergeTaggeds (as++bs)
--     return $ Tagged ta [inner]
-- mergeTagged _ _ = Nothing
-- 
-- mergeTaggeds :: [Generic primitive] -> Maybe (Generic primitive)
-- mergeTaggeds [] = Nothing
-- mergeTaggeds (g:gs) = mergeTagged g =<< mergeTaggeds gs
