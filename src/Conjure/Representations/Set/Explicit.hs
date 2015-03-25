{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.Explicit ( setExplicit ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal


setExplicit :: forall m . (MonadFail m, NameGen m) => Representation m
setExplicit = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck
        chck f (DomainSet _ attrs@(SetAttr SizeAttr_Size{}) innerDomain) =
            DomainSet "Explicit" attrs <$> f innerDomain
        chck _ _ = []

        outName name = mconcat [name, "_", "Explicit"]

        downD :: TypeOf_DownD m
        downD (name, DomainSet "Explicit" (SetAttr (SizeAttr_Size size)) innerDomain) = return $ Just
            [ ( outName name
              , DomainMatrix
                  (DomainInt [RangeBounded 1 size])
                  innerDomain
              ) ]
        downD _ = na "{downD} Explicit"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 (DomainSet "Explicit" (SetAttr (SizeAttr_Size size)) innerDomain) = do
            let
                ordering m = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            forAll &iPat : int(1..&size-1) .
                                &m[&i] < &m[&i+1]
                        |]

                innerStructuralCons m = do
                    (iPat, i) <- quantifiedVar
                    let activeZone b = [essence| forAll &iPat : int(1..&size) . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &m[&i] |]
                    outs <- innerStructuralConsGen inLoop
                    return (map activeZone outs)

            return $ \ ref -> do
                refs <- downX1 ref
                case refs of
                    [m] -> 
                        concat <$> sequence
                            [ ordering m
                            , innerStructuralCons m
                            ]
                    _ -> na "{structuralCons} Explicit"
        structuralCons _ _ _ = na "{structuralCons} Explicit"

        downC :: TypeOf_DownC m
        downC ( name
              , DomainSet "Explicit" (SetAttr (SizeAttr_Size size)) innerDomain
              , ConstantAbstract (AbsLitSet constants)
              ) =
            let outIndexDomain = mkDomainIntB 1 size
            in  return $ Just
                    [ ( outName name
                      , DomainMatrix outIndexDomain innerDomain
                      , ConstantAbstract $ AbsLitMatrix outIndexDomain constants
                      ) ]
        downC _ = na "{downC} Explicit"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainSet "Explicit" (SetAttr (SizeAttr_Size _)) _)) =
            case lookup (outName name) ctxt of
                Nothing -> fail $ vcat $
                    [ "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant ->
                    case constant of
                        ConstantAbstract (AbsLitMatrix _ vals) ->
                            return (name, ConstantAbstract (AbsLitSet vals))
                        _ -> fail $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName name)
                                , "But got:" <+> pretty constant
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
        up _ _ = na "{up} Explicit"
