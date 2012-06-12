{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Core.Middleware.RuleRefnToFunction ( worker ) where

import Language.Core
import qualified Language.Core.Middleware.FreshNames as FreshNames ( worker )

import qualified Data.Set as S
import Data.Generics.Uniplate.Data ( universe )


worker :: (Functor m, Monad m)
    => [RuleRefn]
    -> Either
        [CompError]
        [Core -> CompT m [(Text,Core)]]
    -- Middleware (CompT m) [RuleRefn] [Middleware (CompT m) Core (Maybe [Core])]
worker fs = 
    let
        justsFirst :: Ord a => Maybe a -> Maybe a -> Ordering
        justsFirst (Just i) (Just j) = compare i j
        justsFirst Nothing (Just _)  = GT
        justsFirst (Just _) Nothing  = LT
        justsFirst Nothing  Nothing  = EQ

        fsGrouped :: [[RuleRefn]]
        fsGrouped = groupBy (\ (_,a,_) (_,b,_) -> a == b )
                  $ sortBy  (\ (_,a,_) (_,b,_) -> justsFirst a b )
                    fs

        -- mresults :: (Functor m, Monad m) => [Either [CompError] (Core -> CompT m Core)]
        mresults = map combineRuleRefns fsGrouped

        -- errors :: [CompError]
        errors = concat $ lefts mresults

        -- funcs :: (Functor m, Monad m) => [Core -> CompT m Core]
        funcs = rights mresults
    in
        if null errors
            then Right funcs
            else Left  errors


combineRuleRefns :: (Functor m, Monad m)
    => [RuleRefn]
    -> Either
        [CompError]
        (Core -> CompT m [(Text,Core)])
-- Middleware (CompT m) [RuleRefn] (Middleware (CompT m) Core (Maybe [Core]))
combineRuleRefns fs =
    let
        -- mresults :: (Functor m, Monad m) => [Either CompError (Core -> CompT m (Maybe Core))]
        mresults = map single fs

        -- errors   :: [CompError]
        errors   = lefts  mresults

        -- funcs    :: (Functor m, Monad m) => [Core -> CompT m (Maybe Core)]
        funcs    = rights mresults
    in  if null errors
            then Right $ \ x -> do
                mys <- mapM ($ x) funcs
                let ys = catMaybes mys
                if null ys
                    then err ErrNoRuleApplications
                            $ Nested Nothing []
                    else return ys
            else Left errors


single :: forall m . (Functor m, Monad m)
    => RuleRefn
    -> Either
        CompError                   -- static errors in the rule
        (Core -> CompT m (Maybe (Text,Core)))      -- the rule as a function.
 -- Middleware (CompT m) RuleRefn (Middleware (CompT m) Core (Maybe [Core]))
single ( ( name
         , _
         , viewDeep [":rulerefn"]
            -> Just [ Expr ":rulerefn-pattern"   [pattern]
                    , Expr ":rulerefn-templates" templates
                    , Expr ":rulerefn-locals"    locals
                    ]
         )
       ) = do
    let
        staticCheck :: Either CompError ()
        staticCheck = do
            let metaVarsIn p = S.fromList [ r | Expr ":metavar" [R r] <- universe p ]
            let patternMetaVars   = metaVarsIn pattern
            let templateMetaVars  = S.unions [ metaVarsIn template
                                             | template <- templates ]
            let hasDomainMetaVars = S.unions [ S.unions [ metaVarsIn b
                                                        | Expr ":operator-hasdomain" [_,b] <- universe loc
                                                        ]
                                             | loc <- locals
                                             ]
            unless (templateMetaVars `S.isSubsetOf` S.unions [patternMetaVars,hasDomainMetaVars])
                $ Left ( ErrInvalidRule
                       , singletonNested
                         $ vcat [ "Pattern meta variables:"  <+> prettyListDoc id "," (map showAST $ S.toList patternMetaVars)
                                , "Template meta variables:" <+> prettyListDoc id "," (map showAST $ S.toList templateMetaVars)
                                ]
                       )
    staticCheck
    return $ \ x -> do
        bindersBefore <- gets binders
        let restoreState = modify $ \ st -> st { binders = bindersBefore }
        flagMatch <- match pattern x
        let
            localHandler :: Core -> CompT m Bool
            localHandler lokal@( viewDeep [":toplevel",":where"] -> Just [y] ) = do
                xBool <- toBool y
                case xBool of
                    Just True  -> return True
                    Just False -> do
                        mkLog "rule-fail"
                            $ "where statement evaluated to false: " <++> vcat [ pretty lokal
                                                                               , "in rule" <+> textToDoc name
                                                                               , "at expression" <+> pretty x
                                                                               ]
                        return False
                    Nothing    -> do
                        mkLog "rule-fail"
                            $ "where statement cannot be fully evaluated: " <++> vcat [ pretty lokal
                                                                                      , "in rule" <+> textToDoc name
                                                                                      , "at expression" <+> pretty x
                                                                                      ]
                        return False
            localHandler lokal = err ErrInvariant
                                    $ singletonNested
                                    $ "not handled" <+> showAST lokal
        if flagMatch
            then do
                bs        <- mapM localHandler locals
                if and bs
                    then do
                        template  <- returns templates
                        template' <- FreshNames.worker template
                        mres      <- runMaybeT $ bind template'
                        case mres of
                            Nothing  -> restoreState >> errRuleFail
                            Just res -> restoreState >> return (Just (name, res))
                    else restoreState >> errRuleFail
            else restoreState >> errRuleFail
single _ = Left (ErrInvariant, "This should never happen. (in RuleRefnToFunction.worker)")


errRuleFail :: Monad m => CompT m (Maybe a)
errRuleFail = return Nothing

