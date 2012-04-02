{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Essence.Spec where

import Control.Applicative
import Control.Monad ( unless )
import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.State ( evalStateT )
import Control.Monad.Writer ( MonadWriter )
import Data.Default ( Default, def )
import Data.Foldable ( forM_ )
import Data.Generics ( Data )
import Data.List ( intersperse )
import Data.Maybe ( listToMaybe, maybeToList )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag
                       , Hole
                       , GPlate, gplate, gplateError
                       , GNode, mkG, fromGs
                       , MatchBind, BindingsMap )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), (<>), text, Doc )
import Utils ( mapButLast )
import qualified PrintUtils as Pr

import Language.Essence.Binding
import Language.Essence.Expr
import Language.Essence.Metadata
import Language.Essence.Objective
import Language.Essence.Type
import Language.Essence.Where



data Spec
    = Spec { language    :: String
           , version     :: [Int]
           , topLevels   :: [Either Binding Where]
           , objective   :: Maybe Objective
           , constraints :: [Expr]
           , metadata    :: [Metadata]
           }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Default Spec where
    def = Spec def def def def def def

instance NodeTag Spec

instance Hole Spec

instance GPlate Spec where
    gplate Spec{..} =
        (  map mkG topLevels
        ++ map mkG (maybeToList objective)
        ++ map mkG constraints
        , \ xs -> let
            l1 = length topLevels
            l2 = length (maybeToList objective)
            l3 = length constraints
            topLevels'   = fromGs $ take l1 xs
            objectives'  = fromGs $ take l2 $ drop l1 xs
            constraints' = fromGs $ take l3 $ drop l2 $ drop l1 xs
            in if l1 == length topLevels'  &&
                  l2 == length objectives' &&
                  l3 == length constraints'
                  then Spec language version topLevels'
                                             (listToMaybe objectives')
                                             constraints'
                                             metadata
                  else gplateError "Spec"
        )

instance MatchBind Spec

instance ParsePrint Spec where
    parse = do
        whiteSpace
        (lang,ver) <- pLanguage
        topLevels  <- parse
        obj        <- optionMaybe parse
        cons       <- pConstraints
        eof
        return (Spec lang ver topLevels obj cons [])
        where
            pLanguage :: Parser (String,[Int])
            pLanguage = do
                l  <- reserved "language" *> identifier
                is <- sepBy1 integer dot
                return (l, map fromInteger is)

            pConstraints :: Parser [Expr]
            pConstraints = choiceTry [ do reserved "such"; reserved "that"; sepEndBy parse comma
                                     , return []
                                     ]
    pretty (Spec{..}) = Pr.vcat
        $  ("language" <+> text language <+> Pr.hcat (intersperse Pr.dot (map Pr.int version)))
        : text ""
        :  map pretty topLevels
        ++ case objective of Nothing -> []
                             Just o  -> ["", pretty o]
        ++ case constraints of [] -> []
                               _  -> ""
                                   : "such that"
                                   : ( mapButLast (<> Pr.comma)
                                     $ map (\ x -> Pr.nest 4 $ case x of Q q -> pretty q
                                                                         _   -> pretty x )
                                       constraints
                                     )
        ++ [text ""]


typeCheckSpec :: (Applicative m, MonadError Doc m, MonadWriter [Doc] m) => Spec -> m ()
typeCheckSpec Spec {topLevels, objective, constraints}
    = flip evalStateT (def :: ( BindingsMap
                              , [GNode]
                              , [(GNode,GNode)]
                              )) $ do
    forM_ topLevels $ \ tl ->
        case tl of
            Left  b -> addBinding' b
            Right w -> do
                t <- typeOf (whereExpr w)
                unless (t == TBool)
                       (throwError $ "Where statement isn't a boolean expression: " <+> pretty w)

    forM_ objective $ \ o -> do
        t <- typeOf (objExpr o)
        unless (isOrderedType t)
               (throwError $ "Objective has to be of an ordered type: " <+> pretty o)

    forM_ constraints $ \ c -> do
        t <- typeOf c
        unless (t == TBool)
               (throwError $ "Constraint isn't a boolean expression: " <+> pretty c)
