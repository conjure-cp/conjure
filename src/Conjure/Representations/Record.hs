{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Record
    ( record
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Representations.Internal


record :: forall m . (MonadFail m, NameGen m) => Representation m
record = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainRecord ds) = do
            let names = map fst ds
            outDoms <- sequence <$> mapM (f . snd) ds
            return [ DomainRecord (zip names ds') | ds' <- outDoms ]
        chck _ _ = return []

        mkName name n = mconcat [name, "_", n]

        downD :: TypeOf_DownD m
        downD (name, DomainRecord ds) = return $ Just
            [ (mkName name n, d)
            | (n,d) <- ds
            ]
        downD _ = na "{downD}"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainRecord ds) = return $ \ tup -> do
            refs <- downX1 tup
            concat <$> sequence
                [ do
                    innerStructuralConsGen <- f dom
                    outs                   <- innerStructuralConsGen ref
                    return outs
                | (ref, (_n, dom)) <- zip refs ds
                ]
        structuralCons _ _ _ = na "{structuralCons} record"

        -- TODO: check if (length ds == length cs)
        downC :: TypeOf_DownC m
        downC (name, DomainRecord ds, ConstantAbstract (AbsLitRecord cs))
            | sort (map fst ds) == sort (map fst cs) = return $ Just
                [ case lookup n cs of
                    Nothing -> bug "Record.downC"
                    Just c  -> (mkName name n, d, c)
                | (n,d) <- ds
                ]
        downC (n, d, c) =
            na $ "{downC} record" <+> vcat
                [ "name  :" <+> pretty n
                , "domain:" <+> pretty d
                , "value :" <+> pretty c
                ]

        up :: TypeOf_Up m
        up ctxt (name, DomainRecord ds) = do
            let names = map (mkName name . fst) ds
            vals <- forM names $ \ n ->
                case lookup n ctxt of
                    Nothing -> fail $ vcat $
                        [ "(in Record up)"
                        , "No value for:" <+> pretty n
                        , "When working on:" <+> pretty name
                        , "With domain:" <+> pretty (DomainRecord ds)
                        ] ++
                        ("Bindings in context:" : prettyContext ctxt)
                    Just val -> return (n, val)
            -- TODO: check if (length ds == length vals)
            return (name, ConstantAbstract (AbsLitRecord vals))
        up _ _ = na "{up}"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            xs <- downX1 inp
            Just xsDoms' <- downD ("SO", domain)
            let xsDoms = map snd xsDoms'
            soValues <- sequence [ innerSO downX1 x xDom | (x, xDom) <- zip xs xsDoms ]
            return $ make opFlatten (fromList soValues)
