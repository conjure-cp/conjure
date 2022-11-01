{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Matrix
    ( matrix
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Language.Instantiate
import Conjure.Process.Enumerate
import Conjure.Representations.Internal


-- | The matrix "representation rule".
--   This rule handles the plumbing for matrices.
matrix
    :: forall m . (MonadFailDoc m, NameGen m, MonadUserError m, EnumerateDomain m, ?typeCheckerMode :: TypeCheckerMode)
    => ((Name, DomainX Expression) -> m (Maybe [(Name, DomainX Expression)]))
    -> ((Name, DomainC, Constant) -> m (Maybe [(Name, DomainC, Constant)]))
    -> ((Name, DomainC) -> [(Name, Constant)] -> m (Name, Constant))
    -> Representation m
matrix downD1 downC1 up1 = Representation chck matrixDownD structuralCons matrixDownC matrixUp symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainMatrix indexDomain innerDomain) = map (DomainMatrix indexDomain) <$> f innerDomain
        chck _ _ = return []

        matrixDownD :: TypeOf_DownD m
        matrixDownD (name, DomainMatrix indexDomain innerDomain) = do
            mres <- downD1 (name, innerDomain)
            case mres of
                Nothing -> return Nothing
                Just mids -> return $ Just
                    [ (n, DomainMatrix indexDomain d) | (n, d) <- mids ]
        matrixDownD _ = na "{matrixDownD}"

        structuralCons :: TypeOf_Structural m
        structuralCons f _ (DomainMatrix indexDomain innerDomain) = do
            let
                innerStructuralCons inpMatrix = do
                    (iPat, i) <- quantifiedVarOverDomain indexDomain
                    let activeZone b = [essence| forAll &iPat : &indexDomain . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop r = [essence| &r[&i] |]
                    outs <- innerStructuralConsGen (inLoop inpMatrix)
                    return (map activeZone outs)

            return $ \ inpMatrix -> innerStructuralCons inpMatrix

        structuralCons _ _ _ = na "{structuralCons} matrix 2"

        matrixDownC :: TypeOf_DownC m
        matrixDownC ( name                                                  -- special-case for empty matrix literals
                    , domain@(DomainMatrix indexDomain _)
                    , ConstantAbstract (AbsLitMatrix _indexDomain2 [])
                    ) = do
            mids1
                :: Maybe [(Name, DomainX Expression)]
                <- downD1 (name, fmap Constant domain)
            let
                addEmptyLiteral :: (Name, DomainX Expression) -> m (Name, DomainC, Constant)
                addEmptyLiteral (nm, dom) = do
                    dom' <- mapM (instantiateExpression []) dom
                    return (nm, dom', ConstantAbstract (AbsLitMatrix indexDomain []))
            mapM (mapM addEmptyLiteral) mids1
        matrixDownC ( name
                    , domain@(DomainMatrix indexDomain innerDomain)
                    , constant@(ConstantAbstract (AbsLitMatrix indexDomain2 constants))
                    ) = do
            -- TODO: this may be too strict
            unless (indexDomain == indexDomain2) $
                userErr1 $ vcat
                    [ "Index mismatch."
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    , "With value :" <+> pretty constant
                    ]
            mids1
                :: [Maybe [(Name, DomainC, Constant)]]
                <- sequence [ downC1 (name, innerDomain, c) | c <- constants ]
            let mids2 = catMaybes mids1
            if null mids2                                       -- if all were `Nothing`s
                then return Nothing
                else
                    if length mids2 == length mids1             -- if all were `Just`s
                        then do
                            let
                                mids3 :: [(Name, DomainC, [Constant])]
                                mids3 = [ ( head [ n | (n,_,_) <- line ]
                                          , head [ d | (_,d,_) <- line ]
                                          ,      [ c | (_,_,c) <- line ]
                                          )
                                        | line <- transpose mids2
                                        ]
                            return $ Just
                                [ ( n
                                  , DomainMatrix indexDomain d
                                  , ConstantAbstract $ AbsLitMatrix indexDomain cs
                                  )
                                | (n, d, cs) <- mids3
                                ]
                        else
                            failDoc $ vcat
                                [ "This is weird. Heterogeneous matrix literal?"
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                ]
        matrixDownC _ = na "{matrixDownC}"

        matrixUp :: TypeOf_Up m
        matrixUp ctxt (name, DomainMatrix indexDomain innerDomain)= do

            mid1
                :: Maybe [(Name, DomainX Expression)]
                <- downD1 (name, fmap Constant innerDomain)

            case mid1 of
                Nothing ->
                    -- the inner domain doesn't require refinement
                    -- there needs to be a binding with "name"
                    -- and we just pass it through
                    case lookup name ctxt of
                        Nothing -> failDoc $ vcat $
                            [ "(in Matrix up 1)"
                            , "No value for:" <+> pretty name
                            , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                            ] ++
                            ("Bindings in context:" : prettyContext ctxt)
                        Just constant -> return (name, constant)
                Just mid2 -> do
                    -- the inner domain needs refinement
                    -- there needs to be bindings for each name in (map fst mid2)
                    -- we find those bindings, call (up1 name inner) on them, then lift
                    mid3
                        :: [(Name, [Constant])]
                        <- forM mid2 $ \ (n, _) ->
                            case lookup n ctxt of
                                Nothing -> failDoc $ vcat $
                                    [ "(in Matrix up 2)"
                                    , "No value for:" <+> pretty n
                                    , "When working on:" <+> pretty name
                                    , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                    ] ++
                                    ("Bindings in context:" : prettyContext ctxt)
                                Just constant ->
                                    -- this constant is a ConstantMatrix, containing one component of the things to go into up1
                                    case viewConstantMatrix constant of
                                        Just (_, vals) -> return (n, vals)
                                        _ -> failDoc $ vcat
                                            [ "Expecting a matrix literal for:" <+> pretty n
                                            , "But got:" <+> pretty constant
                                            , "When working on:" <+> pretty name
                                            , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                            ]

                    let
                        midNames :: [Name]
                        midNames     = map fst mid3

                        midConstants :: [[Constant]]
                        midConstants = map snd mid3

                        midConstantsMaxLength = maximum (0 : map length midConstants)

                        midConstantsPadded :: [[Constant]]
                        midConstantsPadded =
                            [ cs ++ replicate (midConstantsMaxLength - length cs) z
                            | let z = ConstantUndefined "midConstantsPadded" TypeAny
                            , cs <- midConstants
                            ]

                    -- -- assertion, midConstants should not be rugged
                    -- case midConstants of
                    --     (x:xs) | any (length x /=) (map length xs) -> failDoc $ vcat
                    --         [ "midConstants is rugged"
                    --         , "midConstants      :" <+> vcat (map (prettyList prBrackets ",") midConstants)
                    --         , "midConstantsPadded:" <+> vcat (map (prettyList prBrackets ",") midConstantsPadded)
                    --         ]
                    --     _ -> return ()

                    mid4
                        :: [(Name, Constant)]
                        <- sequence
                            [ up1 (name, innerDomain) (zip midNames cs)
                            | cs <- transpose midConstantsPadded
                            ]
                    let values = map snd mid4
                    return (name, ConstantAbstract $ AbsLitMatrix indexDomain values)
        matrixUp _ _ = na "{matrixUp}"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain =
            case domain of
                DomainMatrix indexDom innerDom -> do
                    (iPat, i) <- quantifiedVarOverDomain indexDom
                    let mi = [essence| &inp[&i] |]
                    res <- innerSO downX1 mi innerDom
                    return [essence| [ &res | &iPat : &indexDom ] |]
                _ -> bug $ "symmetryOrdering matrix" <+> pretty inp <+> pretty domain
