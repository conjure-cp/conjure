{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Essence.RuleRepr ( RuleRepr(..), RuleReprCase(..) ) where

import Data.Generics ( Data )
import Data.Maybe ( listToMaybe, maybeToList )
import Data.Typeable ( Typeable )

import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag, Hole
                       , GPlate, gplate, gplateError
                       , mkG, fromGs
                       , MatchBind
                       )
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), text )
import qualified PrintUtils as Pr

import Language.Essence.Binding
import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Where



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

