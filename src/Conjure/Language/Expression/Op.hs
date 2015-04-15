module Conjure.Language.Expression.Op
    ( module Conjure.Language.Expression.Op.Internal.Generated
    , module Conjure.Language.Expression.Op.Internal.Common
    , mkBinOp, mkOp
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.AdHoc
import Conjure.Language.Pretty
import Conjure.Language.Lexer
import Conjure.Language.Expression.Op.Internal.Common ( evaluateOp, simplifyOp, functionals, operators, Fixity(..) )
import Conjure.Language.Expression.Op.Internal.Generated



mkBinOp :: (Op x :< x, ExpressionLike x) => Text -> x -> x -> x
mkBinOp op a b =
    case textToLexeme op of
        Nothing -> bug ("Unknown binary operator:" <+> pretty op)
        Just l  ->
            let
                f = case l of
                    L_Plus        -> \ x y -> inject $ MkOpSum         $ OpSum     $ fromList [x,y]
                    L_Minus       -> \ x y -> inject $ MkOpMinus       $ OpMinus       x y
                    L_Times       -> \ x y -> inject $ MkOpProduct     $ OpProduct $ fromList [x,y]
                    L_Div         -> \ x y -> inject $ MkOpDiv         $ OpDiv         x y
                    L_Mod         -> \ x y -> inject $ MkOpMod         $ OpMod         x y
                    L_Pow         -> \ x y -> inject $ MkOpPow         $ OpPow         x y
                    L_Eq          -> \ x y -> inject $ MkOpEq          $ OpEq          x y
                    L_Neq         -> \ x y -> inject $ MkOpNeq         $ OpNeq         x y
                    L_Lt          -> \ x y -> inject $ MkOpLt          $ OpLt          x y
                    L_Leq         -> \ x y -> inject $ MkOpLeq         $ OpLeq         x y
                    L_Gt          -> \ x y -> inject $ MkOpGt          $ OpGt          x y
                    L_Geq         -> \ x y -> inject $ MkOpGeq         $ OpGeq         x y
                    L_in          -> \ x y -> inject $ MkOpIn          $ OpIn          x y
                    L_And         -> \ x y -> inject $ MkOpAnd         $ OpAnd     $ fromList [x,y]
                    L_Or          -> \ x y -> inject $ MkOpOr          $ OpOr      $ fromList [x,y]
                    L_Imply       -> \ x y -> inject $ MkOpImply       $ OpImply       x y
                    L_Iff         -> \ x y -> inject $ MkOpIff         $ OpIff         x y
                    L_subset      -> \ x y -> inject $ MkOpSubset      $ OpSubset      x y
                    L_subsetEq    -> \ x y -> inject $ MkOpSubsetEq    $ OpSubsetEq    x y
                    L_supset      -> \ x y -> inject $ MkOpSupset      $ OpSupset      x y
                    L_supsetEq    -> \ x y -> inject $ MkOpSupsetEq    $ OpSupsetEq    x y
                    L_subsequence -> \ x y -> inject $ MkOpSubsequence $ OpSubsequence x y
                    L_substring   -> \ x y -> inject $ MkOpSubstring   $ OpSubstring   x y
                    L_intersect   -> \ x y -> inject $ MkOpIntersect   $ OpIntersect   x y
                    L_union       -> \ x y -> inject $ MkOpUnion       $ OpUnion       x y
                    L_LexLt       -> \ x y -> inject $ MkOpLexLt       $ OpLexLt       x y
                    L_LexLeq      -> \ x y -> inject $ MkOpLexLeq      $ OpLexLeq      x y
                    L_LexGt       -> \ x y -> inject $ MkOpLexLt       $ OpLexLt       y x
                    L_LexGeq      -> \ x y -> inject $ MkOpLexLeq      $ OpLexLeq      y x
                    L_DotLt       -> \ x y -> inject $ MkOpDotLt       $ OpDotLt       x y
                    L_DotLeq      -> \ x y -> inject $ MkOpDotLeq      $ OpDotLeq      x y
                    L_DotGt       -> \ x y -> inject $ MkOpDotLt       $ OpDotLt       y x
                    L_DotGeq      -> \ x y -> inject $ MkOpDotLeq      $ OpDotLeq      y x
                    L_TildeLt     -> \ x y -> inject $ MkOpTildeLt     $ OpTildeLt     x y
                    L_TildeLeq    -> \ x y -> inject $ MkOpTildeLeq    $ OpTildeLeq    x y
                    L_TildeGt     -> \ x y -> inject $ MkOpTildeLt     $ OpTildeLt     y x
                    L_TildeGeq    -> \ x y -> inject $ MkOpTildeLeq    $ OpTildeLeq    y x
                    _ -> bug ("Unknown lexeme for binary operator:" <+> pretty (show l))
            in
                f a b


mkOp :: (Op x :< x, ReferenceContainer x) => Text -> [x] -> x
mkOp op xs =
    case textToLexeme op of
        Nothing -> case op of
            "and"       -> inject (MkOpAnd       (OpAnd       (headNote "and takes a single argument"       xs)))
            "or"        -> inject (MkOpOr        (OpOr        (headNote "or takes a single argument"        xs)))
            "sum"       -> inject (MkOpSum       (OpSum       (headNote "sum takes a single argument"       xs)))
            "product"   -> inject (MkOpProduct   (OpProduct   (headNote "product takes a single argument"   xs)))
            "not"       -> inject (MkOpNot       (OpNot       (headNote "not takes a single argument"       xs)))
            "negate"    -> inject (MkOpNegate    (OpNegate    (headNote "negate takes a single argument"    xs)))
            "twoBars"   -> inject (MkOpTwoBars   (OpTwoBars   (headNote "twoBars takes a single argument"   xs)))
            "factorial" -> inject (MkOpFactorial (OpFactorial (headNote "factorial takes a single argument" xs)))
            _     -> bug ("Unknown operator:" <+> vcat [pretty op, pretty $ show $ textToLexeme op])
            -- _     -> opImage (fromName (Name op)) xs
        Just l -> case l of
            L_true         -> inject $ MkOpTrue         $ OpTrue         (headNote "true takes a single argument."     xs)
            L_toInt        -> inject $ MkOpToInt        $ OpToInt        (headNote "toInt takes a single argument."    xs)
            L_defined      -> inject $ MkOpDefined      $ OpDefined      (headNote "defined takes a single argument."  xs)
            L_range        -> inject $ MkOpRange        $ OpRange        (headNote "range takes a single argument."    xs)
            L_restrict     -> inject $ MkOpRestrict     $ OpRestrict     (atNote "restrict 1" xs 0) (atNote "restrict 2" xs 1)
            L_allDiff      -> inject $ MkOpAllDiff      $ OpAllDiff      (headNote "allDiff takes a single argument."  xs)
            L_dontCare     -> inject $ MkOpDontCare     $ OpDontCare     (headNote "dontCare takes a single argument." xs)
            L_flatten      -> inject $ MkOpFlatten      $ OpFlatten      (headNote "flatten takes a single argument."  xs)
            L_toSet        -> inject $ MkOpToSet        $ OpToSet        (headNote "toSet takes a single argument."    xs)
            L_toMSet       -> inject $ MkOpToMSet       $ OpToMSet       (headNote "toMSet takes a single argument."   xs)
            L_toRelation   -> inject $ MkOpToRelation   $ OpToRelation   (headNote "toRelation takes a single argument."   xs)
            L_max          -> inject $ MkOpMax          $ OpMax          (headNote "max takes a single argument."   xs)
            L_min          -> inject $ MkOpMin          $ OpMin          (headNote "min takes a single argument."   xs)
            L_preImage     -> inject $ MkOpPreImage     $ OpPreImage     (atNote "preImage 1" xs 0) (atNote "preImage 2" xs 1)
            L_inverse      -> inject $ MkOpInverse      $ OpInverse      (atNote "inverse 1" xs 0)  (atNote "inverse 2"  xs 1)
            L_freq         -> inject $ MkOpFreq         $ OpFreq         (atNote "freq 1"     xs 0) (atNote "freq 2"     xs 1)
            L_hist         -> inject $ MkOpHist         $ OpHist         (atNote "hist 1"     xs 0)
            L_parts        -> inject $ MkOpParts        $ OpParts        (headNote "parts takes a single argument."    xs)
            L_together     -> inject $ MkOpTogether     $ OpTogether     (atNote "together 1" xs 0)
                                                                           (atNote "together 2" xs 1)
                                                                           (atNote "together 3" xs 2)
            L_apart        -> inject $ MkOpApart        $ OpApart        (atNote "apart 1"    xs 0)
                                                                           (atNote "apart 2"    xs 1)
                                                                           (atNote "apart 3"    xs 2)
            L_party        -> inject $ MkOpParty        $ OpParty        (atNote "party 1"    xs 0)
                                                                           (atNote "party 2"    xs 1)
            L_participants -> inject $ MkOpParticipants $ OpParticipants (headNote "participants takes a single argument." xs)
            L_active       -> inject $ MkOpActive       $ OpActive       (atNote "active 1" xs 0)
                                                                           (atNote "active 2" xs 1 |> nameOut |> fromMaybe (bug "active 2"))
            L_pred         -> inject $ MkOpPred         $ OpPred         (headNote "pred takes a single argument."    xs)
            L_succ         -> inject $ MkOpSucc         $ OpSucc         (headNote "succ takes a single argument."    xs)
            _ -> bug ("Unknown lexeme for operator:" <+> pretty (show l))
