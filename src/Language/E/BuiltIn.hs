{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.BuiltIn ( builtInRepr, builtInRefn, mergeReprFunc ) where

import Language.E

type ReprFunc m =
    ( String                                -- original name of the variable
    , E                                     -- original domain
    )
    -> CompE m [ ( String                   -- rule name
                 , String                   -- name of the representation
                 , E                        -- replacement domain
                 , [E]                      -- structural constraints
                 ) ]



mergeReprFunc :: (Functor m, Monad m) => [ReprFunc m] -> ReprFunc m
mergeReprFunc [ ] =  error "mergeReprFunc []"
mergeReprFunc [f] = f
mergeReprFunc fs = \ param -> concat <$> mapM ($ param) fs

builtInRepr :: (Functor m, Monad m) => [ReprFunc m]
builtInRepr = [relationRepr]

relationRepr :: (Functor m, Monad m) => ReprFunc m
relationRepr ( _name, [xMatch| ts := domain.relation.inners |]) = do
    let t = [xMake| domain.tuple.inners := ts |]
    return [( "builtIn.relationRepr"
            , "RelationAsSet"
            , [xMake| domain.set.attributes.attrCollection := []
                    | domain.set.inner := [t]
                    |]
            , []
            )]

relationRepr ( _, [xMatch| _ := domain.function |] ) = return []
relationRepr ( name, dom ) = do
    mkLog "relationRepr" $ vcat [ pretty name
                                , prettyAsPaths dom
                                ]
    return []



type RefnFunc m =
    E                                   -- the expression
    -> CompE m (Maybe [(String, E)])    -- Nothing if rule doesn't apply
                                        -- returns a list of rewrites, fst being rulename
                                        --                           , snd being E

builtInRefn :: (Functor m, Monad m) => [RefnFunc m]
builtInRefn = [relationApply]

relationApply :: (Functor m, Monad m) => RefnFunc m
relationApply [xMatch| [actual]             := functionApply.actual
                     | [Prim (S actualRef)] := functionApply.actual.reference
                     |  args                := functionApply.args
                     |] = do
    case splitOn "#" actualRef of
        [actualName, "RelationAsSet"] -> do
            actualTy <- typeOf actual
            argsTy   <- mapM typeOf args
            case actualTy of
                [xMatch| actualInners := type.relation.inners |] | actualInners == argsTy -> do
                    let theTuple = [xMake| value.tuple.values := args |]
                    let theSet   = [xMake| reference := [Prim $ S $ actualName ++ "_RelationAsSet" ] |]
                    return $! Just [( "builtIn.relationApply"
                                    , [eMake| &theTuple in &theSet |]
                                    )]
                _ -> return Nothing
        _ -> return Nothing
relationApply _ = return Nothing






















