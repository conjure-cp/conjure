{-# LANGUAGE NamedFieldPuns  #-}

module PrintUtils
    ( render, renderDoc, parensIf
	, dot
    , module Text.PrettyPrint
    ) where

import Text.PrettyPrint hiding (render)


render :: Show a => (a -> Maybe Doc) -> a -> String
render f x = case f x of Nothing -> error ("Cannot render: " ++ show x)
                         Just o  -> renderDoc o

renderDoc :: Doc -> String
renderDoc = renderStyle style { lineLength = 160 }

wrapIf :: (Doc -> Doc) -> Bool -> Doc -> Doc
wrapIf wrap c = if c then wrap else id


parensIf :: Bool -> Doc -> Doc
parensIf = wrapIf parens

dot :: Doc
dot = text "."
