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


tuple :: MonadFail m => Representation m
tuple = Representation chck tupleDown_ structuralCons tupleDown tupleUp

    where

        chck f (DomainTuple ds) = DomainTuple <$> mapM f ds
        chck _ _ = []

        mkName name i = mconcat [name, "_", Name (pack (show (i :: Int)))]

        tupleDown_ (name, DomainTuple ds) = return $ Just
            [ (mkName name i, d)
            | i <- [1..]
            | d <- ds
            ]
        tupleDown_ _ = na "{tupleDown_}"

        structuralCons f _ (DomainTuple ds) = return $ \ fresh refs ->
            concat <$> sequence
                [ do
                    innerStructuralConsGen <- f dom
                    outs <- innerStructuralConsGen (tail fresh) [ref]
                    return outs
                | (ref, dom) <- zip refs ds
                ]
        structuralCons _ _ _ = na "{structuralCons} tuple"

        -- TODO: check if (length ds == length cs)
        tupleDown (name, DomainTuple ds, ConstantAbstract (AbsLitTuple cs)) = return $ Just
            [ (mkName name i, d, c)
            | i <- [1..]
            | d <- ds
            | c <- cs
            ]
        tupleDown _ = na "{tupleDown}"

        tupleUp ctxt (name, DomainTuple ds) = do
            let names = map (mkName name) [1 .. length ds]
            vals <- forM names $ \ n ->
                case lookup n ctxt of
                    Nothing -> fail $ vcat $
                        [ "No value for:" <+> pretty n
                        , "When working on:" <+> pretty name
                        , "With domain:" <+> pretty (DomainTuple ds)
                        ] ++
                        ("Bindings in context:" : prettyContext ctxt)
                    Just val -> return val
            -- TODO: check if (length ds == length vals)
            return (name, ConstantAbstract (AbsLitTuple vals))
        tupleUp _ _ = na "{tupleUp}"

