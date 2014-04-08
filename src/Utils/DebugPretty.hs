{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.DebugPretty
    ( DebugPretty(..)
    ) where

import qualified Data.Text as T
import Text.PrettyPrint

import Text.Printf (printf)


class DebugPretty a where
    debugPretty :: a -> Doc

instance DebugPretty Doc     where debugPretty = id
instance DebugPretty T.Text  where debugPretty = debugPretty . T.unpack
-- instance DebugPretty String  where debugPretty = text
instance DebugPretty Char    where debugPretty = debugPretty . show
instance DebugPretty ()      where debugPretty = debugPretty . show
instance DebugPretty Bool    where debugPretty = debugPretty . show
instance DebugPretty Int     where debugPretty = debugPretty . show
instance DebugPretty Integer where debugPretty = debugPretty . show
instance DebugPretty Double  where debugPretty x = debugPretty (printf "%.2f" x :: String)

instance (DebugPretty a, DebugPretty b) => DebugPretty (a,b) where
    debugPretty (a,b) = debugPrettyListDoc parens "," [debugPretty a, debugPretty b]

instance (DebugPretty a, DebugPretty b, DebugPretty c) => DebugPretty (a,b,c) where
    debugPretty (a,b,c) = debugPrettyListDoc parens "," [debugPretty a, debugPretty b, debugPretty c]

instance (DebugPretty a, DebugPretty b, DebugPretty c, DebugPretty d) => DebugPretty (a,b,c,d) where
    debugPretty (a,b,c,d) = debugPrettyListDoc parens "," [debugPretty a, debugPretty b, debugPretty c, debugPretty d]

instance DebugPretty a => DebugPretty (Maybe a) where
    debugPretty Nothing  = "Nothing"
    debugPretty (Just x) = "Just" <+> parens (debugPretty x)

instance DebugPretty a => DebugPretty [a] where
    debugPretty = debugPrettyList brackets ","

debugPrettyList :: DebugPretty a => (Doc -> Doc) -> Doc -> [a] -> Doc
debugPrettyList wrap punc = debugPrettyListDoc wrap punc . map debugPretty

debugPrettyListDoc :: (Doc -> Doc) -> Doc -> [Doc] -> Doc
debugPrettyListDoc wrap punc = wrap . sep . punctuate punc

