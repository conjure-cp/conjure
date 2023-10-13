{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.AttributeAsConstraints
    ( attributeAsConstraints
    , mkAttributeToConstraint
    ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Domain.AddAttributes ( addAttributesToDomain )
import Conjure.Language.Expression.Op
import Conjure.Language.Pretty
import Conjure.Language.TH


-- | From the top level constraints, find the AACs and lift them to the domains of the declarations.
--   Complain for any remaining AACs.
attributeAsConstraints :: MonadFailDoc m => Model -> m Model
attributeAsConstraints m = do
    let statements0 = mStatements m
    statements1 <- transformBiM attributeAsConstraints_OnLocals statements0
    statements2 <- attributeAsConstraints_OnStmts statements1
    return m { mStatements = statements2 }


attributeAsConstraints_OnLocals :: MonadFailDoc m => Expression -> m Expression
attributeAsConstraints_OnLocals (WithLocals h (AuxiliaryVars locals)) =
    WithLocals h . AuxiliaryVars <$> attributeAsConstraints_OnStmts locals
attributeAsConstraints_OnLocals x = return x


attributeAsConstraints_OnStmts :: MonadFailDoc m => [Statement] -> m [Statement]
attributeAsConstraints_OnStmts statements0 = do

    -- collecting top level attribute-as-constraints
    (statements1, topLevelAACs) <- runWriterT $ forM statements0 $ \ st -> case st of
        Where xs -> do
            xs1 <- fmap concat $ forM xs $ \ x -> case x of
                Op (MkOpAttributeAsConstraint (OpAttributeAsConstraint (Reference nm _) attr val)) -> do
                    tell [(nm, attr, val)]
                    return []
                _ -> return [x]
            return [ Where xs1 | not (null xs1) ]
        SuchThat xs -> do
            xs1 <- fmap concat $ forM xs $ \ x -> case x of
                Op (MkOpAttributeAsConstraint (OpAttributeAsConstraint (Reference nm _) attr val)) -> do
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

    return statements2


mkAttributeToConstraint
    :: (NameGen m, MonadFailDoc m, Pretty r, Eq r)
    => Domain r Expression                          -- the input domain
    -> AttrName                                     -- the name of the attribute
    -> Maybe Expression                             -- the value for the attribute
    -> Expression                                   -- the input thing
    -> m Expression                                 -- the constraint

mkAttributeToConstraint domain attr mval x = do
    gen  <- attributeToConstraint domain attr mval
    cons <- gen x
    return cons

attributeToConstraint
    :: (NameGen m, MonadFailDoc m, Pretty r, Eq r)
    => Domain r Expression                          -- the input domain
    -> AttrName                                     -- the name of the attribute
    -> Maybe Expression                             -- the value for the attribute
    -> m (Expression -> m Expression)               -- the constraint generator

attributeToConstraint domain@DomainSet{} = generator where
    generator    "size" (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize" (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize" (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator attr _ =
        failDoc $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@DomainMSet{} = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator "minOccur" (Just val) = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| forAll &iPat in &x . freq(&x,&i) >= &val |]
    generator "maxOccur" (Just val) = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| forAll &iPat in &x . freq(&x,&i) <= &val |]
    generator attr _ =
        failDoc $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@(DomainFunction _ _ inF inT) = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator "total"      Nothing  = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| forAll &iPat : &inF . &i in defined(&x) |]
    generator "injective"  Nothing  = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| allDiff([ &x(&i) | &iPat : &inF ]) |]
    generator "surjective" Nothing  = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| forAll &iPat : &inT . &i in range(&x) |]
    generator "bijective"  Nothing  = return $ \ x -> do
        a <- generator "injective"  Nothing >>= \ gen -> gen x
        b <- generator "injective"  Nothing >>= \ gen -> gen x
        return [essence| &a /\ &b |]
    generator attr _ =
        failDoc $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@(DomainRelation _ _ [dom,dom2]) | dom == dom2 = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]

    generator "reflexive" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        return [essence| forAll &xP           : &dom . &rel(&x,&x) |]
    generator "irreflexive" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        return [essence| forAll &xP           : &dom . !(&rel(&x,&x)) |]
    generator "coreflexive" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &x=&y |]
    generator "symmetric" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> &rel(&y,&x) |]
    generator "antiSymmetric" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) /\ &rel(&y,&x) -> &x=&y |]
    generator "aSymmetric" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) -> !(&rel(&y,&x)) |]
    generator "transitive" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        (zP, z) <- quantifiedVar
        return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&y,&z) -> &rel(&x,&z) |]
    generator "total" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) |]
    generator "connex" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        return [essence| forAll &xP, &yP      : &dom . &rel(&x,&y) \/ &rel(&y,&x) \/ &x = &y |]
    generator "Euclidean" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
        (zP, z) <- quantifiedVar
        return [essence| forAll &xP, &yP, &zP : &dom . &rel(&x,&y) /\ &rel(&x,&z) -> &rel(&y,&z) |]
    generator "serial" Nothing = return $ \ rel -> do
        (xP, x) <- quantifiedVar
        (yP, y) <- quantifiedVar
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
        failDoc $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@DomainRelation{} = generator where
    generator    "size"  (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"  (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"  (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator attr _ =
        failDoc $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain@DomainPartition{} = generator where
    generator    "size"     (Just val) = return $ \ x -> return [essence| |&x| =  &val |]
    generator "minSize"     (Just val) = return $ \ x -> return [essence| |&x| >= &val |]
    generator "maxSize"     (Just val) = return $ \ x -> return [essence| |&x| <= &val |]
    generator "numParts"    (Just val) = return $ \ x -> return [essence| |parts(&x)|  = &val |]
    generator "minNumParts" (Just val) = return $ \ x -> return [essence| |parts(&x)| >= &val |]
    generator "maxNumParts" (Just val) = return $ \ x -> return [essence| |parts(&x)| <= &val |]
    generator "partSize"    (Just val) = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| forAll &iPat in parts(&x) . |&i|  = &val |]
    generator "minPartSize" (Just val) = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| forAll &iPat in parts(&x) . |&i| >= &val |]
    generator "maxPartSize" (Just val) = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        return [essence| forAll &iPat in parts(&x) . |&i| <= &val |]
    generator "regular" Nothing = return $ \ x -> do
        (iPat, i) <- quantifiedVar
        (jPat, j) <- quantifiedVar
        return [essence| forAll &iPat, &jPat in parts(&x) . |&i| = |&j| |]
    generator attr _ =
        failDoc $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]

attributeToConstraint domain = generator where
    generator attr _ =
        failDoc $ vcat [ "Unsupported attribute:" <+> pretty attr
                    , "For the domain:" <+> pretty domain
                    ]
