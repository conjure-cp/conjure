{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Permutation.AsFunction where

import Conjure.Rules.Import

-- | This just unwraps the function in a comprehension
-- Just like having the bare bijective total function in the comprehension
--
-- unclear if this should stay but it is here for now
--rule_Permute_Comprehension :: Rule
--rule_Permute_Comprehension = "permutation-comprehension{AsFunction}" `namedRule` theRule where
--    theRule (Comprehension body gensOrConds) = do
--        (gocBefore, (pat, perm, over), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
--            Generator (GenInExpr pat [essence| permute(&perm,&over) |]) -> return (pat, perm, over)
--            _ -> na "rule_Comprehension"
--        TypePermutation{}                 <- typeOf perm
--        Permutation_AsFunction <- representationOf perm
----        [f] <- downX1 perm 
----        horizontal?
--        return
--            ( "Vertical rule for permutation-comprehension, AsFunction representation"
--            , do
--                (iPat, i) <- quantifiedVar
--                return $ Comprehension body 
--                        $  gocBefore
--                        ++ [ Generator (GenInExpr pat [essence|
--                            [permute(&perm, &i) | &iPat <- &over]
--                                                 |]
--                                       )
--                           ]
--                        ++ gocAfter 
--            )
--    theRule _ = na "rule_Comprehension"


rule_Permute_Comprehension_Tuples :: Rule
rule_Permute_Comprehension_Tuples = "permutation-comprehension-tuples{AsFunction}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) =  do
        (gocBefore, (pat, perm), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat [essence| &perm |]  ) -> return (pat, perm)
            _ -> na "rule_Comprehension"
        TypePermutation{}                 <- typeOf perm
        Permutation_AsFunction <- representationOf perm
        [f] <- downX1 perm
        return
            ( "Vertical rule for permutation-comprehension-tuples, AsFunction representation"
            , do
                return $ Comprehension body 
                        $  gocBefore
                        ++ [ Generator (GenInExpr pat [essence| &f|])
                           ]
                        ++ gocAfter 
            )
    theRule _ = na "rule_Comprehension"



rule_Permute :: Rule
rule_Permute = "permutation-permute{AsFunction}" `namedRule` theRule where
  theRule [essence| permute(&p, &i) |] = do
    case p of WithLocals{} -> na "bubble-delay" ; _ -> return ()
    TypePermutation inner <- typeOf p 
    typeI <- typeOf i
    [f] <- downX1 p
    if typesUnify [inner, typeI]
       then return
              ( "Vertical rule for permutation application to a single value (permute), AsFunction representation"
              , do
                return [essence| [&i, catchUndef(image(&f,&i),0)][toInt(&i in defined(&f))+1] |]
              )
       else na "rule_Permute"
  theRule _ = na "rule_Permute"

--rule_Compose :: Rule
--rule_Compose = "permutation-compose{AsFunction}" `namedRule` theRule where
--  theRule [essence| compose(&p, &q) |] = do
--    TypePermutation innerP <- typeOf p
--    TypePermutation innerQ <- typeOf q
--    if typesUnify [innerP, innerQ]
--       then return
--              ( "Vertical rule for permutation composition, AsFunction representation"
--              , do
--
--              )
--       else na "rule_Compose"
--  theRule _ = na "rule_Compose"
