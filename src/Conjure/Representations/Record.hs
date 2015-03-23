{-# LANGUAGE ParallelListComp #-}

module Conjure.Representations.Record
    ( record
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Representations.Internal


record :: forall m . MonadFail m => Representation m
record = Representation chck downD structuralCons downC up searchStrategy

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainRecord ds) =
            let names = map fst ds
                outDoms = mapM (f . snd) ds
            in  [ DomainRecord (zip names ds') | ds' <- outDoms ]
        chck _ _ = []

        mkName name n = mconcat [name, "_", n]

        downD :: TypeOf_DownD m
        downD (name, DomainRecord ds) = return $ Just
            [ (mkName name n, d)
            | (n,d) <- ds
            ]
        downD _ = na "{downD}"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainRecord ds) = return $ \ fresh tup -> do
            refs <- downX1 tup
            concat <$> sequence
                [ do
                    innerStructuralConsGen <- f dom
                    outs                   <- innerStructuralConsGen (tail fresh) ref
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
        downC _ = na "{downC}"

        up :: TypeOf_Up m
        up ctxt (name, DomainRecord ds) = do
            let names = map (mkName name) (map fst ds)
            vals <- forM names $ \ n ->
                case lookup n ctxt of
                    Nothing -> fail $ vcat $
                        [ "No value for:" <+> pretty n
                        , "When working on:" <+> pretty name
                        , "With domain:" <+> pretty (DomainRecord ds)
                        ] ++
                        ("Bindings in context:" : prettyContext ctxt)
                    Just val -> return (n, val)
            -- TODO: check if (length ds == length vals)
            return (name, ConstantAbstract (AbsLitRecord vals))
        up _ _ = na "{up}"

        searchStrategy :: TypeOf_SearchStrategy m
        searchStrategy p = map (BranchingOn . fst) . fromJustNote "searchStrategy" <$> downD p
