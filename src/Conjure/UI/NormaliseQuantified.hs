module Conjure.UI.NormaliseQuantified
    ( normaliseQuantifiedVariables
    ) where

import Conjure.Prelude
import Conjure.Language


normaliseQuantifiedVariables :: Model -> Model
normaliseQuantifiedVariables m = descendBi (normX 1) (m { mInfo = def })

normX :: Int -> Expression -> Expression
normX nextInt p@(Comprehension _ gocs) =
    let
        quantifiedNames = concat
            [ case gen of
                GenDomainNoRepr  pat _ -> universeBi pat
                GenDomainHasRepr nm  _ -> [nm]
                GenInExpr        pat _ -> universeBi pat
            | Generator gen <- gocs
            ]
        oldNew =
            [ (qn, MachineName "q" i [])
            | (qn, i) <- zip quantifiedNames [nextInt..]
            ]
        nextInt' = nextInt + length oldNew
        f :: Name -> Name
        f nm = fromMaybe nm (lookup nm oldNew)
    in
        p |> descend (normX nextInt')
          |> transformBi f
normX nextInt p =
        p |> descend (normX nextInt)
