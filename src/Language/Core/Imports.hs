-- mostly for library dependencies.
-- defines a specialised prelude. kind-of.
-- things in Language.Core.* should import this.
-- also see "Language.Core"

module Language.Core.Imports
    ( module X
    , textToDoc
    , prettyListDoc
    ) where

import Control.Applicative    as X ( Applicative, (<$>), (<*>), (<*), (*>) )
import Control.Monad          as X ( MonadPlus, void, mzero, msum, when, unless, zipWithM )
import Control.Monad.State    as X ( MonadState, get, gets, put, modify )
import Control.Monad.Reader   as X ( MonadReader )
import Control.Monad.Writer   as X ( MonadWriter, tell )
import Control.Monad.Error    as X ( MonadError, catchError )

import Control.Monad.Trans.Class as X ( MonadTrans, lift )
import Control.Monad.Trans.Maybe as X ( MaybeT(..), runMaybeT )

import Data.Default      as X ( Default, def )
import Data.List         as X ( intersperse, minimumBy, sortBy )
import Data.Maybe        as X ( listToMaybe )
import Data.Monoid       as X ( Monoid, mconcat )
import Data.Ord          as X ( comparing )
import Data.Traversable  as X ( forM )

import Data.Text        as X ( Text, stripPrefix )
import Text.PrettyPrint as X ( Doc, nest, punctuate, sep, vcat, (<+>) )

import Safe as X ( headNote )

import Utils as X ( ppShow, ppPrint )

import Nested as X

import qualified Data.Text as T
import qualified Text.PrettyPrint as Pr

textToDoc :: Text -> Doc
textToDoc = Pr.text . T.unpack

prettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
prettyListDoc wrap punc = wrap . sep . punctuate punc

