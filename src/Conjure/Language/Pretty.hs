{-# LANGUAGE TypeSynonymInstances #-}

module Conjure.Language.Pretty
    ( module X
    , Pretty(..)
    , (<++>), (<+>), (<>)
    , (<+->)
    , prettyList, prettyListDoc
    , parensIf
    , render, renderNormal, renderWide
    , prEmpty, prParens, prBrackets, prBraces
    , Doc
    , prettyContext
    , logDebugId
    , tracingPretty
    , prettyT
    ) where
import Text.PrettyPrint.Annotated.HughesPJ as X
   ( 
    (<>), (<+>), ($$)
    , hang, nest, punctuate , cat
    , hcat, vcat, fsep, hsep, sep
    )
-- conjure
import Conjure.Prelude

-- base
import Text.Printf ( printf )

-- text
-- text
import qualified Data.Text as T ( Text, unpack, length, singleton, concatMap, pack )

-- pretty


-- aeson
import Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific ( Scientific, floatingOrInteger )    -- scientific
import qualified Data.HashMap.Strict as M                   -- unordered-containers
import qualified Data.Vector as V                           -- vector
-- import qualified Prettyprinter.Render.String as Pr
-- import qualified Prettyprinter as Pr

import qualified Text.PrettyPrint.Annotated.HughesPJ as Pr
import Text.PrettyPrint.HughesPJ (Style(..))
import Text.PrettyPrint.Annotated.HughesPJ hiding (Doc,render)


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

-- | For debugging output, truncates the second argument to 5 lines
(<+->) :: Doc -> Doc -> Doc
a <+-> b = a <+> (Pr.vcat $ map pretty $ take 5 $ lines $ renderWide $ b)

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
-- render w = Pr.renderString . (Pr.layoutSmart (Pr.LayoutOptions $ AvailablePerLine w 1.0) . pretty)
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
prettyContext = map (\ (a,b) -> Pr.nest 4 $ pretty a <> ":" <+> pretty b )





--------------------------------------------------------------------------------
-- JSON ------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Not exhaustive, just the ones that are 
-- likely to appear and cause trouble in output.
jsonEsc :: Char -> Text
jsonEsc '"' = "\\\""
jsonEsc '\\' = "\\\\"
jsonEsc '\r' = "\\r"
jsonEsc '\n' = "\\n"
jsonEsc c = T.singleton c

instance Pretty JSON.Value where
    pretty (Object x) = pretty x
    pretty (Array x) = pretty x
    pretty (String x) = "\"" <> pretty (T.unpack (T.concatMap jsonEsc x)) <> "\""
    pretty (Number x) = pretty x
    pretty (Bool False) = "false"
    pretty (Bool True) = "true"
    pretty Null = "null"

instance Pretty JSON.Object where
    pretty = prBraces . fsep . Pr.punctuate "," . map f . sortBy (comp `on` fst) . KM.toList
        where
            f (key, Array value)
                | not (any (\ v -> case v of String t -> T.length t < 20 ; _ -> True ) value)
                = pretty (show key) <> ":" <++> prettyArrayVCat value
            f (key, value) = pretty (show key) <> ":" <++> pretty value

            keyOrder :: M.HashMap Key Int
            keyOrder = M.fromList $
                zip [ "finds", "givens", "enumGivens", "enumLettings", "unnameds"
                    , "strategyQ", "strategyA"
                    , "trailCompact", "trailVerbose", "trailRewrites"
                    , "nameGenState", "nbExtraGivens"
                    , "representations", "representationsTree"
                    , "originalDomains"
                    , "before", "after"
                    ] [1..]
            comp :: Key -> Key -> Ordering
            comp a b =
                let preferred = compare <$> M.lookup a keyOrder
                                        <*> M.lookup b keyOrder
                in  fromMaybe (compare a b) preferred

instance Pretty JSON.Array where
    pretty = prBrackets . fsep . Pr.punctuate "," . map pretty . V.toList

prettyArrayVCat :: V.Vector Value -> Doc
prettyArrayVCat = prBrackets . Pr.vcat . Pr.punctuate "," . map pretty . V.toList

instance Pretty Scientific where
    pretty = either pretty pretty . (floatingOrInteger :: Scientific -> Either Double Integer)

logDebugId :: (MonadLog m, Pretty a) => Doc -> a -> m a
logDebugId msg a = logDebug (msg <++> pretty a) >> return a

tracingPretty :: Pretty a => Doc -> a -> a
tracingPretty s a = trace (renderWide $ "tracing" <+> s <> ": " <++> pretty a) a

prettyT :: Pretty a => a -> Text
prettyT = T.pack.show.pretty