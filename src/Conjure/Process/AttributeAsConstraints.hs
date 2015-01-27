{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.AttributeAsConstraints
    ( attributeAsConstraints
    ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.DomainOf
import Conjure.Language.Ops
import Conjure.Language.Pretty
import Conjure.Language.TH


-- | From the top level constraints, find the AACs and lift them to the domains of the declarations.
--   Complain for any remaining AACs.
attributeAsConstraints :: MonadFail m => Model -> m Model
attributeAsConstraints m = do

    -- collecting top level attribute-as-constraints
    (statements1, topLevelAACs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
        Where xs -> do
            xs1 <- liftM concat $ forM xs $ \ x -> case x of
                Op (MkOpAAC (OpAttributeAsConstraint (Reference nm _) attr val)) -> do
                    tell [(nm, attr, val)]
                    return []
                _ -> return [x]
            return [ Where xs1 | not (null xs1) ]
        SuchThat xs -> do
            xs1 <- liftM concat $ forM xs $ \ x -> case x of
                Op (MkOpAAC (OpAttributeAsConstraint (Reference nm _) attr val)) -> do
                    tell [(nm, attr, val)]
                    return []
                _ -> return [x]
            return [ SuchThat xs1 | not (null xs1) ]
        _ -> return [st]

    -- adding the top level attribute-as-constraints as attributes to the relevant domains
    statements2 <- forM (concat statements1) $ \ st -> case st of
        Declaration (FindOrGiven forg name domain) -> do
            let newAttrs = [ (attr, val) | (nm, attr, val) <- topLevelAACs, name == nm ]
            domain' <- addAttributesToDomain domain newAttrs
            return (Declaration (FindOrGiven forg name domain'))
        _ -> return st

    let
        f (Op (MkOpAAC (OpAttributeAsConstraint thing attr mval))) = do
            dom  <- domainOf thing
            gen  <- attributeToConstraint dom attr mval
            cons <- gen thing
            return cons
        f p = return p

    statements3 <- evalStateT (transformBiM f statements2) (freshNames m { mStatements = statements2 })

    let
        check :: Expression -> Maybe Doc
        check p@(Op (MkOpAAC (OpAttributeAsConstraint thing _ _))) =
            Just $ vcat [ "Cannot add an attribute to this:" <++> pretty thing
                        , "If you think this should be supported, please get in touch."
                        , "The relevant expression:" <++> pretty p
                        , ""
                        ]
        check _ = Nothing

    -- perform the check
    let errors = mapMaybe check (universeBi statements3)
    unless (null errors) (fail (vcat errors))

    return m { mStatements = statements3 }


attributeToConstraint
    :: (MonadState [Name] m, MonadFail m, Pretty r, Eq r)
    => Domain r Expression                          -- the input domain
    -> Name                                         -- the name of the attribute
    -> Maybe Expression                             -- the value for the attribute
    -> m (Expression -> m Expression)               -- the constraint generator

attributeToConstraint domain@DomainSet{} = generator where
    generator    "size" (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize" (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize" (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator attr _ =
        fail $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@DomainMSet{} = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator "minOccur" (Just val) = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat in &x . freq(&x,&i) >= &val |]
    generator "maxOccur" (Just val) = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat in &x . freq(&x,&i) <= &val |]
    generator attr _ =
        fail $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@(DomainFunction _ _ inF inT) = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator "total"      Nothing  = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat : &inF . &i in defined(&x) |]
    generator "injective"  Nothing  = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
            (jPat, j) = quantifiedVar fresh1
        return [essence| forAll &iPat, &jPat : &inF . &i != &j -> &x(&i) != &x(&j) |]
    generator "surjective" Nothing  = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat : &inT . &i in range(&x) |]
    generator "bijective"  Nothing  = return $ \ x -> do
        a <- generator "injective"  Nothing >>= \ gen -> gen x
        b <- generator "injective"  Nothing >>= \ gen -> gen x
        return [essence| &a /\ &b |]
    generator attr _ =
        fail $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@(DomainRelation _ _ [dom,dom2]) | dom == dom2 = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]

    generator "reflexive" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
        return [essence| forAll &xP           : &dom . &rel(&x,&x) |]
    generator "irreflexive" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
        return [essence| forAll &xP           : &dom . !(&rel(&x,&x)) |]
    generator "coreflexive" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &x=&y |]
    generator "symmetric" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &rel(&y,&x) |]
    generator "antiSymmetric" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) /\ &rel(&y,&x) -> &x=&y |]
    generator "aSymmetric" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> !(&rel(&y,&x)) |]
    generator "transitive" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        fresh2 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
            (zP, z) = quantifiedVar fresh2
        return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&y,&z) -> &rel(&x,&z) |]
    generator "total" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) |]
    generator "Euclidean" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        fresh2 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
            (zP, z) = quantifiedVar fresh2
        return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&x,&z) -> &rel(&y,&z) |]
    generator "serial" Nothing = return $ \ rel -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let
            (xP, x) = quantifiedVar fresh0
            (yP, y) = quantifiedVar fresh1
        return [essence| forAll &xP : &dom . exists &yP : &dom . &rel(&x,&y) |]
    generator "equivalence"  Nothing = return $ \ rel -> do
        a <- generator "reflexive"  Nothing >>= \ gen -> gen rel
        b <- generator "symmetric"  Nothing >>= \ gen -> gen rel
        c <- generator "transitive" Nothing >>= \ gen -> gen rel
        return [essence| &a /\ &b /\ &c |]
    generator "partialOrder" Nothing = return $ \ rel -> do
        a <- generator "reflexive"     Nothing >>= \ gen -> gen rel
        b <- generator "antiSymmetric" Nothing >>= \ gen -> gen rel
        c <- generator "transitive"    Nothing >>= \ gen -> gen rel
        return [essence| &a /\ &b /\ &c |]
    generator attr _ =
        fail $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@(DomainRelation _ _ _) = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator attr _ =
        fail $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@(DomainPartition _ _ inner) = generator where
    generator    "size"     (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"     (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"     (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator "numParts"    (Just val) = return $ \ x -> return [essence| |parts(&x)|  = &val |]
    generator "minNumParts" (Just val) = return $ \ x -> return [essence| |parts(&x)| >= &val |]
    generator "maxNumParts" (Just val) = return $ \ x -> return [essence| |parts(&x)| <= &val |]
    generator "partSize"    (Just val) = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat in parts(&x) . |&i|  = &val |]
    generator "minPartSize" (Just val) = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat in parts(&x) . |&i| >= &val |]
    generator "maxPartSize" (Just val) = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat in parts(&x) . |&i| <= &val |]
    generator "complete" Nothing = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
        return [essence| forAll &iPat : &inner . &i in participants(&x) |]
    generator "regular" Nothing = return $ \ x -> do
        fresh0 <- gets head ; modify tail
        fresh1 <- gets head ; modify tail
        let (iPat, i) = quantifiedVar fresh0
            (jPat, j) = quantifiedVar fresh1
        return [essence| forAll &iPat, &jPat in parts(&x) . |&i| = |&j| |]
    generator attr _ =
        fail $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain = generator where
    generator attr _ =
        fail $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]
