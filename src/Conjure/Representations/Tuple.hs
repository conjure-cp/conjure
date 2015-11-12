{-# LANGUAGE ParallelListComp #-}

module Conjure.Representations.Tuple
    ( tuple
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Representations.Internal

-- text
import Data.Text ( pack )


tuple :: forall m . (MonadFail m, NameGen m) => Representation m
tuple = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck f _ (DomainTuple ds) = map DomainTuple . sequence <$> mapM f ds
        chck _ _ _ = return []

        mkName :: Name -> Int -> Name
        mkName name i = mconcat [name, "_", Name (pack (show i))]

        downD :: TypeOf_DownD m
        downD (name, DomainTuple ds) = return $ Just
            [ (mkName name i, d)
            | i <- [1..]
            | d <- ds
            ]
        downD _ = na "{downD}"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainTuple ds) = return $ \ tup -> do
            refs <- downX1 tup
            concat <$> sequence
                [ do
                    innerStructuralConsGen <- f dom
                    outs                   <- innerStructuralConsGen ref
                    return outs
                | (ref, dom) <- zip refs ds
                ]
        structuralCons _ _ _ = na "{structuralCons} tuple"

        downC :: TypeOf_DownC m
        downC (name, DomainTuple ds, ConstantAbstract (AbsLitTuple cs))
            | length ds == length cs = return $ Just
                [ (mkName name i, d, c)
                | i <- [1..]
                | d <- ds
                | c <- cs
                ]
        downC _ = na "{downC}"

        up :: TypeOf_Up m
        up ctxt (name, DomainTuple ds) = do
            let names = map (mkName name) [1 .. length ds]
            vals <- forM names $ \ n ->
                case lookup n ctxt of
                    Nothing -> fail $ vcat $
                        [ "(in Tuple up)"
                        , "No value for:" <+> pretty n
                        , "When working on:" <+> pretty name
                        , "With domain:" <+> pretty (DomainTuple ds)
                        ] ++
                        ("Bindings in context:" : prettyContext ctxt)
                    Just val -> return val
            return (name, ConstantAbstract (AbsLitTuple vals))
        up _ _ = na "{up}"

