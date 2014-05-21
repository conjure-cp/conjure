{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

-- if a decision variable doesn't occur in the spec, no representation is
-- chosen for it by conjure in the repr phase.
-- see issue #36 for an example.
-- this transformation adds a true(x) constraint if there are no
-- constraints on x.
-- all the true(x) constraints should be removed just after the repr phase.

module Language.E.Pipeline.IntroduceFakeConstraints
    ( introduceFakeConstraints
    , removeFakeConstraints
    ) where

import Language.E


data St = Introduced Text | Used Text

introduceFakeConstraints :: MonadConjure m => Spec -> m Spec
introduceFakeConstraints spec@(Spec v s) = do
    let ss = statementAsList s
    st <- execWriterT (mapM_ inspect ss)
    let introduceds =     [ i | Introduced i <- st ]
    let useds       = nub [ i | Used       i <- st ]
    let unuseds     = introduceds \\ useds
    let mkFake i'   = let i = [xMake| reference := [Prim (S i')] |]
                          j = [eMake| true(&i) |]
                      in  [xMake| topLevel.suchThat := [j] |]
    let fakes   = map mkFake unuseds
    let outSpec =
            if null fakes
                then spec
                else Spec v $ listAsStatement (ss ++ fakes)
    return outSpec

    where
        inspect [xMatch| [Prim (S nm)] := topLevel.declaration.find .name.reference |] = tell [Introduced nm]
        inspect [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference |] = tell [Introduced nm]
        inspect [xMatch| [Prim (S nm)] := reference                                 |] = tell [Used       nm]
        inspect Prim {} = return ()
        inspect (Tagged _ xs) = mapM_ inspect xs
        inspect EOF {} = return ()
        inspect (StatementAndNext this next) = inspect this >> inspect next

removeFakeConstraints :: Spec -> Spec
removeFakeConstraints (Spec v s) = Spec v (transform helper s)
    where
        helper [eMatch| true(&_) |] = [eMake| true |]
        helper [eMatch| true(&_,&_) |] = [eMake| true |]
        helper x = x

