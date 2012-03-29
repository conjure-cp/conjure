{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.RuleRepr where

-- import Control.Monad.IO.Class
-- import Debug.Trace
import Control.Applicative
import Control.Arrow ( (&&&) )
import Control.Monad ( (<=<), (>=>), msum, when )
import Control.Monad.Error ( MonadError, throwError, catchError )
import Control.Monad.State ( MonadState, get, modify, StateT, evalStateT, execStateT, runStateT )
import Control.Monad.Writer ( MonadWriter, tell, WriterT, runWriterT )
import Data.Either ( lefts, rights )
import Data.Foldable ( forM_ )
import Data.Function ( on )
import Data.Generics ( Data )
import Data.List ( (\\), group, sort, groupBy, sortBy )
import Data.Maybe ( catMaybes, listToMaybe, maybeToList, isNothing )
import Data.Ord ( comparing )
import Data.Traversable ( forM )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified Data.Map as M
import qualified Data.Set as S

import Constants ( freshNames, newRuleVar )
import GenericOps.Core ( NodeTag, Hole
                       , GPlate, gplate, gplateError
                       , mkG, fromGs
                       , MatchBind, runMatch, runBind, BindingsMap
                       , universe, bottomUp, bottomUpM
                       )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty, prettyListDoc )
import PrintUtils ( (<>), (<+>), text, Doc )
import qualified PrintUtils as Pr
import Utils ( allPairs, concatMapM, fst3 )
import Utils.MonadList ( MonadList, option, runListT )

import Language.Essence.Binding
import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Op
import Language.Essence.Spec
import Language.Essence.Where
import Language.Essence.Phases.BubbleUp ( bubbleUp )
import Language.Essence.Phases.CheckWhere ( checkWhere )
import Language.Essence.Phases.CleanUp ( cleanUp )
import Language.Essence.Phases.EnumIdentifiers ( enumIdentifiers )
import Language.Essence.Phases.QuanDomRefine ( quanDomRefine )
import Language.Essence.Phases.QuanRename ( quanRename )
import Language.EssenceEvaluator ( deepSimplify, runSimplify )



data RuleRepr = RuleRepr
    { reprFilename   :: String
    , reprName       :: String
    , reprTemplate   :: Domain
    , reprStructural :: Maybe Expr
    , reprLocals     :: [Either Binding Where]
    , reprCases      :: [RuleReprCase]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag RuleRepr

instance Hole RuleRepr

instance GPlate RuleRepr where
    gplate RuleRepr {..} =
        (  mkG reprTemplate
        :  map mkG (maybeToList reprStructural)
        ++ map mkG reprLocals
        ++ map mkG reprCases
        , \ xs -> let
            l1 = 1
            l2 = length (maybeToList reprStructural)
            l3 = length reprLocals
            l4 = length reprCases
            reprTemplate'   = fromGs $ take l1 xs
            reprStructural' = fromGs $ take l2 $ drop l1 xs
            reprLocals'     = fromGs $ take l3 $ drop l2 $ drop l1 xs
            reprCases'      = fromGs $ take l4 $ drop l3 $ drop l2 $ drop l1 xs
            in if l1 == length reprTemplate' &&
                  l2 == length reprStructural' &&
                  l3 == length reprLocals' &&
                  l4 == length reprCases'
                  then RuleRepr
                        reprFilename
                        reprName
                        (head reprTemplate')
                        (listToMaybe reprStructural')
                        reprLocals'
                        reprCases'
                  else gplateError "RuleRepr"
        )

instance MatchBind RuleRepr

instance ParsePrint RuleRepr where
    parse = do
        whiteSpace
        name   <- reservedOp "~~>" >> identifier
        templ  <- reservedOp "~~>" >> parse
        cons   <- optionMaybe (reservedOp "~~>" >> parse)
        locals <- parse
        cases  <- many1 parse
        eof
        return (RuleRepr "" name templ cons locals cases)
    pretty RuleRepr {..} = Pr.vcat $ [ "~~>" <+> text reprName
                                     , "~~>" <+> pretty reprTemplate ]
                                  ++ [ "~~>" <+> pretty s | Just s <- [reprStructural] ]
                                  ++ map (Pr.nest 4 . pretty) reprLocals
                                  ++ [ text "" ]
                                  ++ map pretty reprCases



data RuleReprCase = RuleReprCase
    { reprCasePattern    :: Domain
    , reprCaseStructural :: Maybe Expr
    , reprCaseLocals     :: [Either Binding Where]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag RuleReprCase

instance Hole RuleReprCase

instance GPlate RuleReprCase where
    gplate RuleReprCase {..} =
        (  mkG reprCasePattern
        :  map mkG (maybeToList reprCaseStructural)
        ++ map mkG reprCaseLocals
        , \ xs -> let
            l1 = 1
            l2 = length (maybeToList reprCaseStructural)
            l3 = length reprCaseLocals
            reprCasePattern'    = fromGs $ take l1 xs
            reprCaseStructural' = fromGs $ take l2 $ drop l1 xs
            reprCaseLocals'     = fromGs $ take l3 $ drop l2 $ drop l1 xs
            in if l1 == length reprCasePattern'  &&
                  l2 == length reprCaseStructural' &&
                  l3 == length reprCaseLocals'
                  then RuleReprCase
                        (head reprCasePattern')
                        (listToMaybe reprCaseStructural')
                        reprCaseLocals'
                  else gplateError "RuleReprCase"
        )

instance MatchBind RuleReprCase

instance ParsePrint RuleReprCase where
    parse = do
        pattern <- reservedOp "***" >> parse
        cons    <- optionMaybe (reservedOp "~~>" >> parse)
        locals  <- parse
        return (RuleReprCase pattern cons locals)
    pretty RuleReprCase {..} = Pr.vcat $ [ "***" <+> pretty reprCasePattern ]
                                      ++ [ "~~>" <+> pretty s | Just s <- [reprCaseStructural] ]
                                      ++ map (Pr.nest 4 . pretty) reprCaseLocals
                                      ++ [ text "" ]



callRepr ::
    ( Applicative m
    , MonadError Doc m
    , MonadWriter [Doc] m
    ) => [RuleRepr] -> Spec -> m [Spec]
callRepr rules' specParam = do
    spec <- (enumIdentifiers >=> runSimplify) specParam
    let identifiers = [ nm | Identifier nm <- universe spec ]
    let qNames = freshNames \\ identifiers
    let rules = map (scopeIdentifiers newRuleVar) rules'
    bindings <- flip execStateT M.empty $ mapM_ addBinding' (lefts (topLevels spec))
    results <- flip evalStateT (bindings,qNames) $ applyReprsToSpec rules spec
    ss <- fmap (map cleanUp) $ mapM runSimplify $ map bubbleUp results
    return ss
    -- mapM runSimplify =<< concatMapM (quanDomRefine rules) =<< mapM runSimplify ss


type ReprResult = ( String     -- name of the representation
                  , Domain     -- replacement domain
                  , [Expr]     -- structural constraints
                  )

applyReprsToSpec ::
    ( Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState (BindingsMap, [String]) m
    , MonadWriter [Doc] m
    ) => [RuleRepr]
      -> Spec
      -> m [Spec]
applyReprsToSpec rules spec = do
    let
        remainingBindings :: M.Map String Domain
        remainingBindings = M.fromList $
            [ (nm, dom) | Find (Identifier nm) dom <- lefts (topLevels spec)
                        , needsRepresentation dom
                        , isNothing $ representationValue dom ]
            ++
            [ (nm, dom) | Given (Identifier nm) dom <- lefts (topLevels spec)
                        , needsRepresentation dom
                        , isNothing $ representationValue dom ]

    candidates :: M.Map String [ReprResult]
               <- forM remainingBindings $ applyReprsToDom rules

    let
        counts :: M.Map String Int
        counts =
            let lu = M.keysSet remainingBindings
            in  M.fromList
              $ map (head &&& length)
              . group
              . sort
              $  [ nm | Identifier nm <- concatMap universe (maybeToList $ objective spec), S.member nm lu ]
              ++ [ nm | Identifier nm <- concatMap universe (            constraints spec), S.member nm lu ]
              ++ [ nm | Identifier nm <- concatMap universe (     rights $ topLevels spec), S.member nm lu ]

    if M.null remainingBindings
        then return []
        else do
            forM_ (M.toList candidates) $ \ (nm,rs) -> case rs of
                [] -> throwError $ text nm <+> "needs a representation, but no rule matches it."
                _  -> return ()

            tell [ Pr.vcat $ "Representation options:"
                           : map (\ (nm,rs) -> Pr.nest 4 $ text nm <> Pr.colon
                                                                  <+> prettyListDoc id Pr.comma (map (text . fst3) rs)
                                 ) (M.toList candidates)
                 ]

            tell [ Pr.vcat $ "Number of occurrences in the specification:"
                           : map (\ (nm,c) -> Pr.nest 4 $ text nm <> Pr.colon <+> Pr.int c ) (M.toList counts)
                 ]

            let
                f :: ( MonadList m
                     , MonadWriter ([Binding],[Expr]) m
                     , MonadState [(String,String)] m                -- those added. name reprName pair.
                     ) => Expr -> m Expr
                f p@(EHole (Identifier nm)) = case M.lookup nm candidates of
                    Nothing -> return p
                    Just rs -> do
                        (nmR,dom,xs) <- option rs
                        st <- get
                        when ((nm,nmR) `notElem` st) $ do
                            let refn = nm ++ "_" ++ nmR
                            modify ((nm,nmR):)
                            tell ( map (bottomUp (identifierRenamer "refn" refn)) [ Find (Identifier refn) dom ]
                                 , map (bottomUp (identifierRenamer "refn" refn)) xs
                                 )
                        return $ EHole $ Identifier $ nm ++ "#" ++ nmR
                f p = return p

            results <- runListT $ flip runStateT [] $ runWriterT $ bottomUpM f spec
            forM results $ \ ((sp,(bs,xs)),stuffAdded) -> do
                let
                    channels :: [(String,[String])]
                    channels
                        = filter (\ (_,i) -> length i > 1 )                    -- remove those which have only one repr
                        $ map (\ i -> (fst (head i), map snd i) )
                        $ groupBy ((==) `on` fst)
                        $ sortBy (comparing fst)
                          stuffAdded
                    channelCons :: [Expr]
                    channelCons
                        = map (\ (a,b) -> EOp Eq [ EHole (Identifier a)
                                                 , EHole (Identifier b) ] )
                        $ concatMap allPairs
                        $ map (\ (nm,rs) -> [ nm ++ "_" ++ r | r <- rs ] )
                          channels
                return sp { topLevels   = topLevels   sp ++ map Left bs
                          , constraints = constraints sp ++ xs ++ channelCons
                          }


applyReprsToDom ::
    ( Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState (BindingsMap, [String]) m
    , MonadWriter [Doc] m
    ) => [RuleRepr]
      -> Domain
      -> m [ReprResult]
applyReprsToDom rules dom = withLog ("applyReprsToDom     " <+> pretty dom)
                          $ catMaybes <$> mapM tryApply rules
    where
        tryApply rule = catchError ( Just <$> applyReprToDom rule dom )
                                   ( \ e -> do {- tell [e]; -} return Nothing )


applyReprToDom ::
    ( Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState (BindingsMap, [String]) m
    , MonadWriter [Doc] m
    ) => RuleRepr
      -> Domain
      -> m ReprResult
applyReprToDom rule dom = do
    candidates <- mapM tryApply (reprCases rule)
    case msum candidates of
        Nothing -> throwError "Rule not applied."
        Just r  -> return r

    where
        tryApply rulecase = catchError ( Just <$> applyReprCaseToDom rule rulecase dom )
                                       ( \ e -> do {- tell [e]; -} return Nothing )


applyReprCaseToDom ::
    ( Applicative m
    , Monad m
    , MonadError Doc m
    , MonadState (BindingsMap, [String]) m
    , MonadWriter [Doc] m
    ) => RuleRepr
      -> RuleReprCase
      -> Domain
      -> m ReprResult
applyReprCaseToDom (RuleRepr {..}) (RuleReprCase {..}) dom = do
    st <- get
    (tmpl, str) <- flip evalStateT (fst st) $ do
        runMatch reprCasePattern dom
        forM_ (reprCaseLocals ++ reprLocals) $ \ l ->
            case l of
                Left  b -> withLog "addBinding" $ addBinding' b
                Right w -> withLog "checkWhere" $ checkWhere w
        tmpl <- runBind reprTemplate
        str  <- mapM (deepSimplify <=< runBind) (maybeToList reprStructural ++ maybeToList reprCaseStructural)
        return (tmpl, str)
    tmpl' <- quanRename tmpl
    str'  <- mapM quanRename str
    return (reprName,tmpl',str')


withLog :: MonadWriter [Doc] m => Doc -> m a -> m a
withLog _ = id
-- withLog d comp = do
--     tell ["START:" <+> d]
--     res <- comp
--     tell ["END:  " <+> d]
--     return res
