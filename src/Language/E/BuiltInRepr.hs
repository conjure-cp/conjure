{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.BuiltInRepr where

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

builtInRepr :: (Functor m, Monad m) => ReprFunc m
builtInRepr = mergeReprFunc [relationRepr]

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
