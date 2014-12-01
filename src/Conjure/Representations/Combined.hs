{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conjure.Representations.Combined
    ( downD, downC, up
    , downD1, downC1, up1
    , downToX1
    , reprOptions, getStructurals
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TH

import Conjure.Representations.Internal
import Conjure.Representations.Primitive
import Conjure.Representations.Tuple
import Conjure.Representations.Set.Occurrence
import Conjure.Representations.Set.Explicit
import Conjure.Representations.Set.ExplicitVarSizeWithMarker
import Conjure.Representations.Set.ExplicitVarSizeWithFlags
import Conjure.Representations.Function.Function1D
import Conjure.Representations.Function.Function1DPartial
import Conjure.Representations.Function.FunctionND
import Conjure.Representations.Function.FunctionNDPartial
import Conjure.Representations.Relation.RelationAsMatrix
import Conjure.Representations.Relation.RelationAsSet


-- | Refine (down) a domain, outputting refinement expressions (X) one level (1).
--   The domain is allowed to be at the class level.
downToX1
    :: MonadFail m
    => FindOrGiven
    -> Name
    -> DomainX Expression
    -> m [Expression]
downToX1 forg name domain = rDownToX (dispatch domain) forg name domain

-- | Refine (down) a domain (D), one level (1).
--   The domain is allowed to be at the class level.
downD1
    :: MonadFail m
    =>           (Name, DomainX Expression)
    -> m (Maybe [(Name, DomainX Expression)])
downD1 (name, domain) = rDownD (dispatch domain) (name, domain)

-- | Refine (down) a domain, together with a constant (C), one level (1).
--   The domain has to be fully instantiated.
downC1
    :: MonadFail m
    =>           (Name, DomainC, Constant)
    -> m (Maybe [(Name, DomainC, Constant)])
downC1 (name, domain, constant) = rDownC (dispatch domain) (name, domain, constant)


-- | Translate a bunch of low level constants up, one level.
--   The high level domain (i.e. the target domain) has to be given.
--   The domain has to be fully instantiated.
up1
    :: MonadFail m
    =>   (Name, DomainC)
    ->  [(Name, Constant)]
    -> m (Name, Constant)
up1 (name, domain) ctxt = rUp (dispatch domain) ctxt (name, domain)


-- | Refine (down) a domain (D), all the way.
--   The domain is allowed to be at the class level.
downD
    :: MonadFail m
    =>    (Name, DomainX Expression)
    -> m [(Name, DomainX Expression)]
downD inp@(_, domain) = do
    mout <- rDownD (dispatch domain) inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM downD outs

-- | Refine (down) a domain, together with a constant (C), all the way.
--   The domain has to be fully instantiated.
downC
    :: MonadFail m
    =>    (Name, DomainC, Constant)
    -> m [(Name, DomainC, Constant)]
downC inp = do
    mout <- downC1 inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM downC outs

-- | Translate a bunch of low level constants up, all the way.
--   The high level domain (i.e. the target domain) has to be given.
--   The domain has to be fully instantiated.
up
    :: MonadFail m
    =>  [(Name, Constant)]
    ->   (Name, DomainC)
    -> m (Name, Constant)
up ctxt (name, highDomain) = do
    toDescend'
        -- :: Maybe [(Name, DomainX x)]
        <- downD1 (name, fmap Constant highDomain)
    case toDescend' of
        Nothing ->
            case lookup name ctxt of
                Nothing -> fail $ vcat
                    $ ("No value for:" <+> pretty name)
                    : "Bindings in context:"
                    : prettyContext ctxt
                Just val -> return (name, val)
        Just toDescend -> do
            midConstants
                 :: [(Name, Constant)]
                 <- sequence [ up ctxt (n, fmap e2c d) | (n, d) <- toDescend ]
            up1 (name, highDomain) midConstants


-- | Combine all known representations into one.
--   Dispatch into the actual implementation of the representation depending on the provided domain.
dispatch :: (MonadFail m, Pretty x) => Domain HasRepresentation x -> Representation m
dispatch domain = do
    let nope = bug $ "No representation for the domain:" <+> pretty domain
    case domain of
        DomainBool{}    -> primitive
        DomainInt{}     -> primitive
        DomainTuple{}   -> tuple
        DomainMatrix{}  -> matrix
        DomainSet r _ _ -> case r of
            "Occurrence"                    -> setOccurrence
            "Explicit"                      -> setExplicit
            "ExplicitVarSizeWithMarker"     -> setExplicitVarSizeWithMarker
            "ExplicitVarSizeWithFlags"      -> setExplicitVarSizeWithFlags
            _ -> nope
        DomainFunction r _ _ _ -> case r of
            "Function1D"                    -> function1D
            "Function1DPartial"             -> function1DPartial
            "FunctionND"                    -> functionND
            "FunctionNDPartial"             -> functionNDPartial
            _ -> nope
        DomainRelation r _ _ -> case r of
            "RelationAsMatrix"              -> relationAsMatrix
            "RelationAsSet"                 -> relationAsSet dispatch
            _ -> nope
        _ -> nope


-- | A list of all representations.
--   As a crude measure, implementing levels here.
--   We shouldn't have levels between representations in the long run.
allReprs :: [[Representation (Either Doc)]]
allReprs =
    [ [ primitive, tuple, matrix
      , setOccurrence, setExplicit, setExplicitVarSizeWithMarker, setExplicitVarSizeWithFlags
      , function1D, function1DPartial, functionND, functionNDPartial
      , relationAsMatrix
      ]
    , [ -- functionAsRelation
        relationAsSet dispatch
      ]
    ]


-- | For a domain, produce a list of domains with different representation options.
--   This function should never return an empty list.
reprOptions :: (Pretty x, ExpressionLike x) => Domain r x -> [Domain HasRepresentation x]
reprOptions domain = go allReprs
    where
        go [] = []
        go (reprsThisLevel:reprsNextLevels) =
            let matchesOnThisLevel = concat [ rCheck r reprOptions domain | r <- reprsThisLevel ]
            in  if null matchesOnThisLevel
                    then go reprsNextLevels
                    else matchesOnThisLevel


-- | For a domain, returns the structural constraints.
--   Makes recursive calls to generate the complete structural constraints.
--   Takes in a function to refine inner guys.
getStructurals
    :: MonadFail m
    => (Expression -> m [Expression])
    -> DomainX Expression
    -> m ([Name] -> [Expression] -> m [Expression])
getStructurals downX1 domain = rStructural (dispatch domain) (getStructurals downX1) downX1 domain


-- | The matrix "representation rule".
--   This rule handles the plumbing for matrices.
--   It is in this module because it recursively calls the other representations via `allReprs`.
--   And it is also included in `allReprs`.
matrix :: MonadFail m => Representation m
matrix = Representation chck matrixDown_ structuralCons matrixDown matrixUp

    where

        chck f (DomainMatrix indexDomain innerDomain) = DomainMatrix indexDomain <$> f innerDomain
        chck _ _ = []

        matrixDown_ (name, DomainMatrix indexDomain innerDomain) = do
            mres <- downD1 (name, innerDomain)
            case mres of
                Nothing -> return Nothing
                Just mids -> return $ Just
                    [ (n, DomainMatrix indexDomain d) | (n, d) <- mids ]
        matrixDown_ _ = na "{matrixDown_}"

        structuralCons f _ (DomainMatrix indexDomain innerDomain) = do
            let
                innerStructuralCons fresh ref = do
                    let (iPat, i) = quantifiedVar (headInf fresh)
                    let activeZone b = [essence| forAll &iPat : &indexDomain . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &ref[&i] |]
                    outs <- innerStructuralConsGen (tail fresh) [inLoop]
                    return (map activeZone outs)

            return $ \ fresh refs -> concat <$> mapM (innerStructuralCons fresh) refs

        structuralCons _ _ _ = na "{structuralCons} matrix 2"

        -- TODO: check if indices are the same
        matrixDown ( name
                   , DomainMatrix indexDomain innerDomain
                   , ConstantAbstract (AbsLitMatrix _indexDomain2 constants)
                   ) = do
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
                            fail $ vcat
                                [ "This is weird. Heterogeneous matrix literal?"
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                ]
        matrixDown _ = na "{matrixDown}"

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
                        Nothing -> fail $ vcat $
                            [ "No value for:" <+> pretty name
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
                                Nothing -> fail $ vcat $
                                    [ "No value for:" <+> pretty n
                                    , "When working on:" <+> pretty name
                                    , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                    ] ++
                                    ("Bindings in context:" : prettyContext ctxt)
                                Just constant ->
                                    -- this constant is a ConstantMatrix, containing one component of the things to go into up1
                                    case constant of
                                        ConstantAbstract (AbsLitMatrix _ c) -> return (n, c)
                                        _ -> fail $ vcat
                                            [ "Expecting a matrix literal for:" <+> pretty n
                                            , "But got:" <+> pretty constant
                                            , "When working on:" <+> pretty name
                                            , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                            ]

                    let midNames     = map fst mid3
                    let midConstants = map snd mid3

                    mid4
                        :: [(Name, Constant)]
                        <- sequence
                            [ up1 (name, innerDomain) (zip midNames cs)
                            | cs <- transpose midConstants
                            ]
                    let values = map snd mid4
                    return (name, ConstantAbstract $ AbsLitMatrix indexDomain values)
        matrixUp _ _ = na "{matrixUp}"

