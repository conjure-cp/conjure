{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.TranslateSolution ( translateSingleSolution, translateSolution ) where

-- conjure
import Conjure.UpDown
import Language.E.Imports
import Language.E.Definition


-- | Translating a collection of low level constants to a high level constant.
--   The high level domain (with representations) is taken as an argument.
translateSingleSolution
    :: MonadError UpDownError m
    => Text
    -> Domain Representation Constant
    ->  [(Text, Constant)]
    -> m (Text, Constant)
translateSingleSolution name highDomain ctxt = do
    (_, _, _, _, highConstantsGen) <- upDown highDomain
    highConstant <- highConstantsGen name ctxt
    return (name, highConstant)

--     let lowNames = lowNamesGen name
--     lowDomains <- lowDomainsGen
--     lowConstants <- lowConstantsGen highConstant
--     return $ zip3 lowNames lowDomains lowConstants
--
--
-- -- translateSingleSolution name d@(DomainMatrix index domain) lows = do
-- --     case name `lookup` lows of
-- --         Just (ConstantMatrix _ constants) -> do
-- --             constants' <- liftM (map snd) $ sequence [ translateSingleSolution name domain ((name, c):lows) | c <- constants ]
-- --             return (name, ConstantMatrix index constants')
-- --         Nothing -> throwError $ ConstantUpError $ sep $
-- --             [ pretty "translateSingleSolution DomainMatrix 1", pretty name, pretty d ]
-- --             ++ map pretty lows
-- --         Just c -> throwError $ ConstantUpError $ sep $
-- --             [ pretty "translateSingleSolution DomainMatrix 2", pretty name, pretty d, pretty c ]
-- --             ++ map pretty lows
--
-- translateSingleSolution name domain lows = do
--     mres <- upDown domain
--     case mres of
--         Nothing ->
--             case name `lookup` lows of
--                 Nothing ->
--                     -- here "name" may be a matrix containing abstract things
--                     case domain of
--                         DomainMatrix _ inner -> do
--                             mres <- upDown domain
--                             case mres of
--                                 Nothing -> throwError (ConstantUpError "meh")
--                                 Just (genDoms,genNames,_,_,_) -> do
--                                     doms <- genDoms
--                                     let names = map ($ name) genNames
--                                     mids <- sequence [ translateSingleSolution n d lows | (n,d) <- zip names doms ]
--                                     let (midNames, midConstants) = unzip mids
--                                     endName <- namesUp midNames
--                                     endConstant <- constantsUp midConstants
--                                     return (endName, endConstant)
--
--
--                             doms <- downDomain inner
--                             when ([inner] == doms) $
--                             translateSingleSolution doms
--                         _ -> throwError $ ConstantUpError $ sep $
--                                 [ pretty "translateSingleSolution", pretty name, pretty domain ]
--                                 ++ map pretty lows
--                 Just c  -> return (name, c)
--         Just (domainDown, namesDown, namesUp, _, constantsUp) -> do
--             domains <- domainDown
--             let names = map ($ name) namesDown
--             mids <- sequence [ translateSingleSolution n d lows | (n,d) <- zip names domains ]
--             let (midNames, midConstants) = unzip mids
--             endName <- namesUp midNames
--             endConstant <- constantsUp midConstants
--             return (endName, endConstant)
--
-- -- translateSingleSolution name domain lows = do
-- --     let (lowNames, lowConstants) = unzip lows
-- --     mres <- upDown domain
-- --     case mres of
-- --         Nothing -> do
-- --             highConstant <- upConstant domain lowConstants
-- --             return (name, highConstant)
-- --         Just (_, _, nameUp, _, constantUp) -> do
-- --             highName <- nameUp lowNames
-- --             highConstant <- constantUp lowConstants
-- --             return (highName, highConstant)


translateSolution
    :: MonadError UpDownError m
    => Spec                         -- ^ Essence
    -> Spec                         -- ^ Essence'
    -> Spec                         -- ^ Essence' Solution
    -> m Spec                       -- ^ Essence Solution
translateSolution = error "translateSolution"

