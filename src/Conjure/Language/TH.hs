{-# LANGUAGE TemplateHaskell #-}

module Conjure.Language.TH ( essence, essenceStmts, module X ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Parser
import Conjure.Language.Lenses as X ( fixRelationProj ) -- reexporting because it is needed by the QQ


-- parsec
import Text.Parsec ( SourcePos, setPosition )
import Text.Parsec.Pos ( newPos )

-- template-haskell
import Language.Haskell.TH ( Q, runIO, Loc(..), location, mkName, ExpQ, varE, appE, PatQ, varP )
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

-- syb
import Data.Generics.Aliases ( extQ )


essenceStmts :: QuasiQuoter
essenceStmts = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseTopLevels) str
        dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP) e
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseTopLevels) str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP) e
    , quoteType = error "quoteType"
    , quoteDec  = error "quoteDec"
    }

essence :: QuasiQuoter
essence = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpr) str
        dataToExpQ (const Nothing `extQ` expE `extQ` expD `extQ` expAP) e
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpr) str
        dataToPatQ (const Nothing `extQ` patE `extQ` patD `extQ` patAP) e
    , quoteType = error "quoteType"
    , quoteDec  = error "quoteDec"
    }

locationTH :: Q SourcePos
locationTH = aux <$> location
    where
        aux :: Loc -> SourcePos
        aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

expE :: Expression -> Maybe ExpQ
expE (ExpressionMetaVar x) =
    Just (appE [| $(varE (mkName "fixRelationProj")) |]
               [| $(varE (mkName x)) |])
expE _ = Nothing

expD :: Domain () Expression -> Maybe ExpQ
expD (DomainMetaVar x) =
    Just (appE [| $(varE (mkName "fixRelationProj")) |]
               (appE [| $(varE (mkName "forgetRepr")) |]
                     [| $(varE (mkName x)) |]))
expD _ = Nothing

expAP :: AbstractPattern -> Maybe ExpQ
expAP (AbstractPatternMetaVar x) = Just [| $(varE (mkName x)) |]
expAP _ = Nothing


patE :: Expression -> Maybe PatQ
patE (ExpressionMetaVar x) = Just (varP (mkName x))
patE _ = Nothing

patD :: Domain () Expression -> Maybe PatQ
patD (DomainMetaVar x) = Just (varP (mkName x))
patD _ = Nothing

patAP :: AbstractPattern -> Maybe PatQ
patAP (AbstractPatternMetaVar x) = Just (varP (mkName x))
patAP _ = Nothing

