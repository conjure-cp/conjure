{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stuff.Generic.GenMatcher where

import Stuff.Generic.Definition
import Stuff.Generic.PatParser
import Stuff.Pretty
import Stuff.TryMatch

import Data.Either
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Generics ( Data )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( Lit )
import Language.Haskell.TH.Quote

import Language.Haskell.TH.Lift ( deriveLift )


qq :: QuasiQuoter
qq = QuasiQuoter { quoteExp  = error "not supported"
                 , quoteType = error "not supported"
                 , quoteDec  = error "not supported"
                 , quotePat  = error "not supported"
                 }

defaultCase :: QuasiQuoter
defaultCase = qq
    { quoteDec = \ nm ->
        return [ FunD (mkName nm)
               [ Clause
                   [WildP]
                   (NormalB (AppE (ConE 'Left) (LitE (StringL ("didn't match: " ++ nm)))))
                   []
               ]]
    }

magic :: QuasiQuoter
magic = qq
    { quotePat  = \ patternString -> do
        loc <- location
        case parsePat (showLoc loc) patternString of
            Left  e       -> error e
            Right pattern -> compilePattern pattern
    }


compilePattern :: GenericPat -> Q Pat
compilePattern (MetaVar s) = return $ VarP $ mkName s
compilePattern (NamedPat t v) = do
    v' <- compilePattern v
    return $ ConP (mkName "Named") [LitP (StringL t), v']
compilePattern (ListPat xs) = do
    xs' <- mapM compilePattern xs
    return $ ConP (mkName "List") [ListP xs']
compilePattern (ObjectPat xs) = do
    let sorted = sortBy (comparing fst) xs
    let names = map fst sorted
    xs' <- mapM (compilePattern . snd) sorted
    return $
        ConP (mkName "Object")
        [ListP $ zipWith (\ i j -> TupP [LitP (StringL i),j] ) names xs']
compilePattern p = error $ "Cannot: " ++ show p


showLoc :: Loc -> String
showLoc loc = concat [ "in file "
                     , loc_filename loc
                     , " at line "
                     , show $ fst $ loc_start loc
                     , " column "
                     , show $ snd $ loc_start loc
                     ]

    -- pattern <- parsePat patternString
    -- return $ VarP $ mkName "a" 
    -- error $ "foo: " ++ patternString
    -- input: a -> a -> a
    -- output a -> a
    -- = [| \ f -> let a = Prim 1 in let b = Prim 2 in \ x -> f x |]


test :: Int -> Q Exp -> Q Exp
test value continuation = [| value |]
    
    -- return $ LetE [ ValD (VarP (mkName "x"))
    --                      (NormalB $(value))
    --                      []
    --               ]
    --               continuation

-- genMatcher :: (Lift tag, Lift primitive, Lift proxy)
--     => Generic tag primitive proxy
--     -> Q Exp
--     -> Q Exp
-- genMatcher pattern continuation = do
--     continuation' <- continuation
--     [| \ actual -> case actual of
--                     Prim _ -> error "a pattern cannot contain a Prim"
--                     Named t v ->
--                     _      -> error "some other msg"
--      |]
--         -- $(genMatcher' pattern actual continuation') |]
-- 
-- 
-- genMatcher' :: forall tag primitive proxy . (Pretty tag, Pretty primitive, Eq tag, Lift tag, Lift primitive, Lift proxy)
--     => Generic tag primitive proxy
--     -> Generic tag primitive proxy
--     -> Exp
--     -> Q Exp
-- genMatcher' (Prim   {}) _ _      = error "a pattern cannot contain a Prim"
-- genMatcher' (Named t v) actual continuation = do
--     v'  <- inQ $ matchNamed actual t
--     genMatcher' v v' continuation
-- genMatcher' (List xs  ) actual continuation = do
--     xs' <- inQ $ matchList actual
--     let
--         loopIt :: [Generic tag primitive proxy]
--                -> [Generic tag primitive proxy]
--                -> Exp
--                -> Q Exp
--         loopIt []     []     cont = return cont
--         loopIt (i:is) (j:js) cont = do cont' <- genMatcher' i j cont ; loopIt is js cont'
--         loopIt _ _ _ = error "Different number of elements in list term."
--     loopIt xs' xs continuation
-- genMatcher' (Object _xs) _ _ = error "n. i. y."
-- genMatcher' (MetaVar s) actual continuation = do
--     actual'       <- [e|actual|]
--     return $ LetE [ ValD (VarP (mkName s))          -- Pat
--                          (NormalB actual')          -- Body
--                          []                         -- Dec
--                   ]
--                   continuation
-- 
-- 
-- inQ :: TryMatch a -> Q a
-- inQ (TryMatch (Left  e)) = error $ show e
-- inQ (TryMatch (Right x)) = return x
-- 
-- 
-- -- data Generic tag primitive proxy
-- --     = Prim primitive
-- --     | Named tag (Generic tag primitive proxy)
-- --     | List [Generic tag primitive proxy]
-- --     | Object (Map tag (Generic tag primitive proxy))
-- --     | MetaVar String
-- 
-- -- matchPrimitive :: (Pretty tag, Pretty primitive)
-- --     => Generic tag primitive proxy
-- --     -> TryMatch primitive
-- -- matchPrimitive (Prim x) = return x
-- -- matchPrimitive x        = throwError $ "Not a primitive:" <+> pretty x
-- -- 
-- -- matchNamed :: (Pretty tag, Pretty primitive, Eq tag)
-- --     => Generic tag primitive proxy
-- --     -> tag
-- --     -> TryMatch (Generic tag primitive proxy)
-- -- matchNamed (Named t v) tE | t == tE   = return v
-- --                           | otherwise = throwError $ "Different name." <++> vcat [ "Expecting:" <+> pretty tE
-- --                                                                                  , "But got:" <+> pretty t
-- --                                                                                  ]
-- -- matchNamed x _ = throwError $ "Not a named term:" <+> pretty x
-- -- 
-- -- matchList :: (Pretty tag, Pretty primitive)
-- --     => Generic tag primitive proxy
-- --     -> TryMatch [Generic tag primitive proxy]
-- -- matchList (List xs) = return xs
-- -- matchList x         = throwError $ "Not a list term:" <+> pretty x
-- -- 
-- -- matchObject :: (Pretty tag, Pretty primitive)
-- --     => Generic tag primitive proxy
-- --     -> TryMatch (Map tag (Generic tag primitive proxy))
-- -- matchObject (Object m) = return m
-- -- matchObject x          = throwError $ "Not an object term:" <+> pretty x
-- -- 
-- -- matchMetaVar :: (Pretty tag, Pretty primitive)
-- --     => Generic tag primitive proxy
-- --     -> TryMatch String
-- -- matchMetaVar (MetaVar s) = return s
-- -- matchMetaVar x           = throwError $ "Not a meta variable:" <+> pretty x
