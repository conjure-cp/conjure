{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.AbstractDomsInQuans where

-- this transformation is called during refn, but uses repr rules
-- it handles those quantified expressions which quantify over an abstract
-- domain.

-- such as: sum s : dom , guard . body
-- where dom is sth like set (size 2) of int(1..3)

-- what it does is pretty simple:
-- apply repr rules to dom and get (newDom, structuralCons) pairs
-- rewrite the expr into: sum s : newDom , guard /\ structuralCons . body
-- where structuralCons and body might need some rewrites

import Language.E
import Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction )
import Language.E.BuiltIn ( builtInRepr, mergeReprFunc )


abstractDomsInQuans :: MonadConjure m => [RuleRepr] -> E -> m (Maybe [(Text, E)])
abstractDomsInQuans
    reprs
    [xMatch| [qnQuan]         := quantified.quantifier
           | [Prim (S qnVar)] := quantified.quanVar.structural.single.reference
           | [qnOverDom]      := quantified.quanOverDom
           | []               := quantified.quanOverOp
           | []               := quantified.quanOverExpr
           | [guard]          := quantified.guard
           | [body]           := quantified.body
           |]
    | domainNeedsRepresentation qnOverDom
    = withBindingScope' $ let mfunc = ruleReprToFunction reprs in case mfunc of
        Left es     -> err ErrFatal $ vcat $ map (prettyError "abstractDomsInQuans") es
        Right func' -> withBindingScope' $ do
            let func = mergeReprFunc (func' : builtInRepr)
            ys <- func (qnVar, qnOverDom, error "abstractDomsInQuans.decl")
            zs <- case ys of
                [] -> err ErrFatal $ "No representation rule matches domain:" <+> pretty qnOverDom
                _  -> do
                    let ysNames = flip map ys $ \ (_origDecl, _ruleName, reprName, _newDom, _cons) -> reprName
                    mkLog "representation" $ sep [ pretty qnOverDom
                                                 , "(#" <> pretty (length ys) <> ")"
                                                 , prettyList id "," ysNames
                                                 ]
                    let zs = flip map ys $ \ (_origDecl, _ruleName, reprName, newDom, cons)
                                          -> (reprName, newDom, cons)
                    return zs
            let mkOut qnVar' qnOverDom' guards body' =
                    let
                        guards' = if null guards
                                    then [ [xMake| emptyGuard := [] |] ]
                                    else guards
                    in
                        [xMake| quantified.quantifier   := [qnQuan]
                              | quantified.quanVar.structural.single.reference := [Prim (S qnVar')]
                              | quantified.quanOverDom  := [qnOverDom']
                              | quantified.quanOverOp   := []
                              | quantified.quanOverExpr := []
                              | quantified.guard        := [conjunct guards']
                              | quantified.body         := [body']
                              |]
            let outs = flip map zs $ \ (reprName, newDom, cons) ->
                    let
                        qnVar'Hash       = identifierConstruct qnVar (Just "regionS") (Just reprName)
                        qnVar'Underscore = qnVar `mappend` "_" `mappend` reprName
                        qnVarReplacer :: E -> E
                        qnVarReplacer = replace
                            [xMake| reference := [Prim (S qnVar     )] |]
                            [xMake| reference := [Prim (S qnVar'Hash)] |]
                    in
                        ( "builtIn.abstractDomsInQuans"
                        , mkOut
                            qnVar'Underscore
                            newDom
                            [ qnVarReplacer i | i <- guard : cons
                                              , i /= [xMake| emptyGuard := [] |]
                                              , i /= [eMake| true |]
                                              ]
                            ( qnVarReplacer body )
                        )
            return $ Just outs
abstractDomsInQuans _ _ = return Nothing

quanDomAndSubsetEq :: MonadConjure m => E -> m (Maybe [(Text, E)])
quanDomAndSubsetEq x = return $ case go x of
    Nothing -> Nothing
    Just y  -> Just [("builtIn.quanDomAndSubsetEq", y)]
    where
        go [eMatch| &quan &i : &dom subsetEq &blah , &guard . &body |] = Just
           [eMake|  &quan &i : &dom , &i subsetEq &blah /\ &guard . &body |]
        go _ = Nothing

