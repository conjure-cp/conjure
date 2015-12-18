{-# LANGUAGE TemplateHaskell #-}

module Conjure.Language.TH
    ( essence
    , essenceStmts
    , module X
    , locationTH            -- unused, exporting only to silence the warning
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Parser
import Conjure.Language.Lenses as X ( fixRelationProj ) -- reexporting because it is needed by the QQ


-- megaparsec
-- import Text.Megaparsec.Prim ( setPosition )
import Text.Megaparsec.Pos ( SourcePos, newPos )

-- template-haskell
import Language.Haskell.TH ( Q, Loc(..), location, mkName, ExpQ, varE, appE, PatQ, varP, wildP )
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

-- syb
import Data.Generics.Aliases ( extQ )


essenceStmts :: QuasiQuoter
essenceStmts = QuasiQuoter
    { quoteExp = \ str -> do
        e <- parseIO parseTopLevels str
        let e' = dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP `extQ` expN) e
        appE [| $(varE (mkName "fixRelationProj")) |] e'
    , quotePat  = \ str -> do
        e <- parseIO parseTopLevels str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP `extQ` patN) e
    , quoteType = bug "quoteType"
    , quoteDec  = bug "quoteDec"
    }

essence :: QuasiQuoter
essence = QuasiQuoter
    { quoteExp = \ str -> do
        e <- parseIO parseExpr str
        let e' = dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP `extQ` expN) e
        appE [| $(varE (mkName "fixRelationProj")) |] e'
    , quotePat  = \ str -> do
        e <- parseIO parseExpr str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP `extQ` patN) e
    , quoteType = bug "quoteType"
    , quoteDec  = bug "quoteDec"
    }

locationTH :: Q SourcePos
locationTH = do
    loc <- location
    let file = loc_filename loc
    let (line, col) = loc_start loc
    return (newPos file line col)

expE :: Expression -> Maybe ExpQ
expE (ExpressionMetaVar x) = Just [| $(varE (mkName x)) |]
expE _ = Nothing

expD :: Domain () Expression -> Maybe ExpQ
expD (DomainMetaVar x) = Just (appE [| $(varE (mkName "forgetRepr")) |]
                                    [| $(varE (mkName x)) |])
expD _ = Nothing

expAP :: AbstractPattern -> Maybe ExpQ
expAP (AbstractPatternMetaVar x) = Just [| $(varE (mkName x)) |]
expAP _ = Nothing

expN :: Name -> Maybe ExpQ
expN (NameMetaVar x) = Just [| $(varE (mkName x)) |]
expN _ = Nothing


patE :: Expression -> Maybe PatQ
patE (ExpressionMetaVar x) = toPat x
patE _ = Nothing

patD :: Domain () Expression -> Maybe PatQ
patD (DomainMetaVar x) = Just (varP (mkName x))
patD _ = Nothing

patAP :: AbstractPattern -> Maybe PatQ
patAP (AbstractPatternMetaVar x) = Just (varP (mkName x))
patAP _ = Nothing

patN :: Name -> Maybe PatQ
patN (NameMetaVar x) = Just (varP (mkName x))
patN _ = Nothing


toPat :: String -> Maybe PatQ
toPat "_" = Just wildP
toPat x = Just (varP (mkName x))
