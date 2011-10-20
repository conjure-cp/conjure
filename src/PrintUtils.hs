{-# LANGUAGE NamedFieldPuns  #-}

module PrintUtils (
    render, parensIf,
    module Text.PrettyPrint
    ) where

import Text.PrettyPrint hiding (render)


render :: Doc -> String
render = renderStyle style { lineLength = 120 }


wrapIf :: (Doc -> Doc) -> Bool -> Doc -> Doc
wrapIf wrap c = if c then wrap else id


parensIf :: Bool -> Doc -> Doc
parensIf = wrapIf parens

