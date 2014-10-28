{-# LANGUAGE TemplateHaskell #-}

module Conjure.Language.TH ( essence ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Parser

-- parsec
import Text.Parsec ( SourcePos, setPosition )
import Text.Parsec.Pos ( newPos )

-- template-haskell
import Language.Haskell.TH ( Q, runIO, Loc(..), location, mkName, ExpQ, varE, PatQ, varP )
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

-- syb
import Data.Generics.Aliases ( extQ )


essence :: QuasiQuoter
essence = QuasiQuoter
    { quoteExp = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpression) str
        dataToExpQ (const Nothing `extQ` metaExp) e
    , quotePat  = \ str -> do
        l <- locationTH
        e <- runIO $ parseIO (setPosition l *> parseExpression) str
        dataToPatQ (const Nothing `extQ` metaPat) e        
    , quoteType = error "quoteType"
    , quoteDec  = error "quoteDec"
    }


locationTH :: Q SourcePos
locationTH = aux <$> location
    where
        aux :: Loc -> SourcePos
        aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)


metaExp :: Expression -> Maybe ExpQ
metaExp (ExpressionMetaVar x) = Just [| $(varE (mkName x)) |]
metaExp _ = Nothing


metaPat :: Expression -> Maybe PatQ
metaPat (ExpressionMetaVar x) = Just (varP (mkName x))
metaPat _ = Nothing

