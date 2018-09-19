{-# LANGUAGE TypeSynonymInstances #-}

module Conjure.Language.Pretty
    ( Pretty(..)
    , (<++>), (<+>), (<>)
    , prettyList, prettyListDoc
    , parensIf
    , render, renderNormal, renderWide
    , hang, hcat
    , prEmpty, prParens, prBrackets, prBraces
    , Doc
    , prettyContext
    , logDebugId
    , tracingPretty
    ) where

-- conjure
import Conjure.Prelude

-- base
import Text.Printf ( printf )

-- text
import qualified Data.Text as T ( Text, unpack )

-- pretty
import Text.PrettyPrint
    ( parens, brackets, braces, empty       -- will be exported with new names
    , text                                  -- only used in this module
    , style, renderStyle, lineLength, ribbonsPerLine
    )

-- aeson
import Data.Aeson as JSON
import Data.Scientific ( Scientific, floatingOrInteger )    -- scientific
import qualified Data.HashMap.Strict as M                   -- unordered-containers
import qualified Data.Vector as V                           -- vector


class Show a => Pretty a where
    pretty :: a -> Doc
    prettyPrec :: Int -> a -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty

instance Pretty Doc     where pretty = id
instance Pretty T.Text  where pretty = pretty . T.unpack
instance Pretty String  where pretty = text
instance Pretty ()      where pretty = pretty . show
instance Pretty Bool    where pretty = pretty . show
instance Pretty Int     where pretty = pretty . show
instance Pretty Integer where pretty = pretty . show
instance Pretty Double  where pretty x = pretty (printf "%.2f" x :: String)

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = prettyListDoc parens "," [pretty a, pretty b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pretty (a, b, c) = prettyListDoc parens "," [pretty a, pretty b, pretty c]

instance Pretty a => Pretty (Maybe a) where
    pretty Nothing  = "Nothing"
    pretty (Just x) = "Just" <+> parens (pretty x)


infixl 5 <++>
(<++>) :: Doc -> Doc -> Doc
a <++> b = hang a 4 b

prettyList :: Pretty a => (Doc -> Doc) -> Doc -> [a] -> Doc
prettyList wrap punc = prettyListDoc wrap punc . map pretty

prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . fsep . punctuate punc

parensIf :: Bool -> Doc -> Doc
parensIf = wrapIf parens
    where
        wrapIf :: (Doc -> Doc) -> Bool -> Doc -> Doc
        wrapIf wrap c = if c then wrap else id

renderNormal :: Pretty a => a -> String
renderNormal = render 120

renderWide :: Pretty a => a -> String
renderWide = render 240

render :: Pretty a => Int -> a -> String
render w = renderStyle (style { lineLength = w, ribbonsPerLine = 1 }) . pretty

prEmpty :: Doc
prEmpty = empty

prParens :: Doc -> Doc
prParens = parens

prBrackets :: Doc -> Doc
prBrackets = brackets

prBraces :: Doc -> Doc
prBraces = braces

prettyContext :: (Pretty a, Pretty b) => [(a,b)] -> [Doc]
prettyContext = map (\ (a,b) -> nest 4 $ pretty a <> ":" <+> pretty b )


--------------------------------------------------------------------------------
-- JSON ------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance Pretty JSON.Value where
    pretty (Object x) = pretty x
    pretty (Array x) = pretty x
    pretty (String x) = pretty (show x)
    pretty (Number x) = pretty x
    pretty (Bool False) = "false"
    pretty (Bool True) = "true"
    pretty Null = "null"

instance Pretty JSON.Object where
    pretty = prBraces . vcat . punctuate "," . map f . sortBy (comp `on` fst) . M.toList
        where
            f (key, Array value)
                | all (\ v -> case v of String{} -> True ; _ -> False ) value
                = pretty (show key) <> ":" <++> prettyArrayVCat value
            f (key, value) = pretty (show key) <> ":" <++> pretty value

            keyOrder :: M.HashMap Text Int
            keyOrder = M.fromList $
                zip [ "finds", "givens", "enumGivens", "enumLettings", "unnameds"
                    , "strategyQ", "strategyA"
                    , "trailCompact", "trailVerbose", "trailRewrites"
                    , "nameGenState", "nbExtraGivens"
                    , "representations", "representationsTree"
                    , "originalDomains"
                    , "questionAnswered"
                    , "before", "after"
                    ] [1..]

            comp a b =
                let preferred = compare <$> M.lookup a keyOrder
                                        <*> M.lookup b keyOrder
                in  fromMaybe (compare a b) preferred

instance Pretty JSON.Array where
    pretty = prBrackets . fsep . punctuate "," . map pretty . V.toList

prettyArrayVCat :: V.Vector Value -> Doc
prettyArrayVCat = prBrackets . vcat . punctuate "," . map pretty . V.toList

instance Pretty Scientific where
    pretty = either pretty pretty . (floatingOrInteger :: Scientific -> Either Double Integer)

logDebugId :: (MonadLog m, Pretty a) => Doc -> a -> m a
logDebugId msg a = logDebug (msg <++> pretty a) >> return a

tracingPretty :: Pretty a => Doc -> a -> a
tracingPretty s a = trace (renderWide $ "tracing " <+> s <> ": " <++> pretty a) a
