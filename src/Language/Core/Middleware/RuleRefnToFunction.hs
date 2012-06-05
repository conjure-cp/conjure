{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Middleware.RuleRefnToFunction ( worker ) where

import Language.Core
import Language.Core.Middleware
import qualified Language.Core.Middleware.FreshNames as FreshNames ( worker )

import qualified Data.Set as S
import Data.Generics.Uniplate.Data ( universe )

worker :: (Functor m, Monad m) => Middleware (CompT m) [RuleRefn] [Middleware (CompT m) Core (Maybe [Core])]
worker fs = do
    let
        justsFirst (Just i) (Just j) = compare i j
        justsFirst Nothing (Just _)  = GT
        justsFirst (Just _) Nothing  = LT
        justsFirst Nothing  Nothing  = EQ
    gs <- mapM combineRuleRefns
            ( groupBy (\ (_,a,_) (_,b,_) -> a == b )
            $ sortBy  (\ (_,a,_) (_,b,_) -> justsFirst a b )
              fs )
    return gs
    -- return $ \ x -> return Nothing

combineRuleRefns :: (Functor m, Monad m) => Middleware (CompT m) [RuleRefn] (Middleware (CompT m) Core (Maybe [Core]))
combineRuleRefns fs = return $ \ x -> do
    gs  <- mapM single fs
    mys <- mapM ($ x) gs
    return $ case concat $ catMaybes mys of
        [] -> Nothing
        ys -> Just ys

single :: (Functor m, Monad m) => Middleware (CompT m) RuleRefn (Middleware (CompT m) Core (Maybe [Core]))
single ( ( _name
         , _
         , viewDeep [":rulerefn"]
            -> Just [ Expr ":rulerefn-pattern"   [pattern]
                    , Expr ":rulerefn-templates" templates
                    , Expr ":rulerefn-locals"    locals
                    ]
         )
       ) = do

   -- f <- mkFunction pa tes
   -- return $ \ x -> runMaybeT (f x)
    let patternMetaVars  = S.fromList [ r | Expr ":metavar" [R r] <- universe pattern  ]
    let templateMetaVars = S.unions [ S.fromList [ r | Expr ":metavar" [R r] <- universe template ]
                                    | template <- templates
                                    ]
    unless (templateMetaVars `S.isSubsetOf` patternMetaVars)
        $ err $ vcat [ "Pattern meta variables:"  <+> prettyListDoc id "," (map showAST $ S.toList patternMetaVars)
                     , "Template meta variables:" <+> prettyListDoc id "," (map showAST $ S.toList templateMetaVars)
                     ]
    return $ \ x -> runMaybeT $ do
        bindersBefore <- gets binders
        b <- lift $ match pattern x
        let
            localHandler local@( viewDeep [":toplevel",":where"] -> Just [y] ) = do
                xBool <- lift $ toBool y
                case xBool of
                    Just True  -> return ()
                    Just False -> do
                        lift $ mkLog "rule-fail" $ "where statement evaluated to false: " <++> pretty local
                        mzero
                    Nothing    -> do
                        lift $ mkLog "rule-fail" $ "where statement cannot be fully evaluated: " <++> pretty local
                        mzero
            localHandler local = err $ "not handled" <+> showAST local
        beforeRenamer <-
            if b
                then do
                    mapM_ localHandler locals
                    ys <- forM templates $ \ t -> do
                        t' <- lift $ FreshNames.worker t
                        bind t'
                    modify $ \ st -> st { binders = bindersBefore }
                    return ys
                else do
                    modify $ \ st -> st { binders = bindersBefore }
                    mzero
        return beforeRenamer
single _ = err "This should never happen. (in RuleRefnToFunction.worker)"
