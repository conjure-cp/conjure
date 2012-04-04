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
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty, prettyList )
import PrintUtils ( (<+>), text )
import qualified PrintUtils as Pr

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
    parse = do
        whiteSpace
        level     <- optionMaybe (brackets (fromInteger <$> integer))
        pattern   <- parse
        templates <- reservedOp "~~>" >> try (braces (parse `sepBy1` comma)) <|> (return <$> parse)
        locals    <- parse
        eof
        return $ RuleRefn level "" pattern templates locals
    pretty RuleRefn {..} = Pr.vcat $ concat [ [ Pr.brackets (Pr.int l), text "" ] | Just l <- [refnLevel] ]
                                  ++ [ pretty refnPattern <+> "~~>"
                                                          <+> case refnTemplates of
                                                                   [t] -> pretty t
                                                                   ts  -> prettyList Pr.braces Pr.comma ts ]
                                  ++ [ text "" ]
                                  ++ map pretty refnLocals


