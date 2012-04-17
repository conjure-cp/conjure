{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Essence.RuleRefn ( RuleRefn(..) ) where

import Control.Applicative
import Data.Generics ( Data )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

import GenericOps.Core ( NodeTag
                       , Hole
                       , GPlate, gplate, gplateError 
                       , mkG, fromGs
                       , MatchBind )
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), text )
import qualified PrintUtils as Pr

import Language.EssenceLexer
import Language.EssenceLexerP
import Language.Essence.Binding
import Language.Essence.Expr
import Language.Essence.Where



data RuleRefn = RuleRefn
    { refnLevel     :: Maybe Int
    , refnFilename  :: String
    , refnPattern   :: Expr
    , refnTemplates :: [Expr]
    , refnLocals    :: [Either Binding Where]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag RuleRefn

instance Hole RuleRefn

instance GPlate RuleRefn where
    gplate RuleRefn {..} = 
        (  mkG refnPattern
        :  map mkG refnTemplates
        ++ map mkG refnLocals
        , \ xs -> let
            l1 = 1
            l2 = length refnTemplates
            l3 = length refnLocals
            refnPattern'   = fromGs $ take l1 xs
            refnTemplates' = fromGs $ take l2 $ drop l1 xs
            refnLocals'    = fromGs $ take l3 $ drop l2 $ drop l1 xs
            in if l1 == length refnPattern'  &&
                  l2 == length refnTemplates' &&
                  l3 == length refnLocals'
                  then RuleRefn
                        refnLevel
                        refnFilename
                        (head refnPattern')
                        refnTemplates'
                        refnLocals'
                  else gplateError "RuleRefn"
        )

instance MatchBind RuleRefn

instance ParsePrint RuleRefn where
    parse = inCompleteFile $ do
        level     <- optionMaybe (brackets (fromInteger <$> integer))
        pattern   <- parse
        templates <- some (lexeme L_SquigglyArrow >> parse)
        locals    <- parse
        return $ RuleRefn level "" pattern templates locals
    pretty RuleRefn {..} = Pr.vcat $ concat [ [ Pr.brackets (Pr.int l), text "" ] | Just l <- [refnLevel] ]
                                  ++ [ pretty refnPattern <+> Pr.vcat [ "~~>" <+> pretty t | t <- refnTemplates ] ]
                                  ++ [ text "" ]
                                  ++ map pretty refnLocals


