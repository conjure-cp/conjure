
data FlagKind = On | OnOff | Default Int [Int] | Ignore
    deriving (Eq, Show)

allFlags :: [(String, FlagKind)]
allFlags =
    [ ( "case-merge"                        , On                              )
    , ( "cse"                               , On                              )
    , ( "dicts-strict"                      , OnOff                           )
    , ( "do-eta-reduction"                  , On                              )
    , ( "do-lambda-eta-expansion"           , OnOff                           )
    , ( "eager-blackholing"                 , OnOff                           )
    , ( "enable-rewrite-rules"              , On                              )
    , ( "vectorise"                         , Ignore                          )
    , ( "avoid-vect"                        , Ignore                          )
    , ( "excess-precision"                  , OnOff                           )
    , ( "float-in"                          , On                              )
    , ( "full-laziness"                     , On                              )
    , ( "fun-to-thunk"                      , OnOff                           )
    , ( "ignore-asserts"                    , OnOff                           )
    , ( "ignore-interface-pragmas"          , OnOff                           )
    , ( "loopification"                     , OnOff                           )
    , ( "late-dmd-anal"                     , OnOff                           )
    , ( "liberate-case"                     , OnOff                           )
    , ( "liberate-case-threshold"           , Default 200 [100,200..1000]     )
    , ( "max-relevant-bindings"             , Ignore                          )
    , ( "max-simplifier-iterations"         , Default undefined []            )
    , ( "max-worker-args"                   , Default 10 [10,20,100]          )
    , ( "no-opt-coercion"                   , OnOff                           )
    , ( "no-pre-inlining"                   , OnOff                           )
    , ( "no-state-hack"                     , OnOff                           )
    , ( "pedantic-bottoms"                  , OnOff                           )
    , ( "omit-interface-pragmas"            , OnOff                           )
    , ( "simplifier-phases"                 , Default 2 [1,2..10]             )
    , ( "simpl-tick-factor"                 , Default 100 []                  )
    , ( "spec-constr"                       , OnOff                           )
    , ( "spec-constr-threshold"             , Default 200 []                  )
    , ( "spec-constr-count"                 , Default 3 []                    )
    , ( "specialise"                        , On                              )
    , ( "strictness"                        , On                              )
    , ( "strictness-before"                 , Ignore                          )
    , ( "static-argument-transformation"    , OnOff                           )
    , ( "unbox-strict-fields"               , OnOff                           )
    , ( "unbox-small-strict-fields"         , OnOff                           )
    , ( "unfolding-creation-threshold"      , Ignore                          )
    , ( "unfolding-fun-discount"            , Ignore                          )
    , ( "unfolding-keeness-factor"          , Ignore                          )
    , ( "unfolding-use-threshold"           , Ignore                          )
    ]

main :: IO ()
main = do
    let allOptions
            =  [ map (++f) ["-f", "-fno-"] | (f, OnOff) <- allFlags ]
            ++ [ [ "-f" ++ f ] | (f, On) <- allFlags ]
    mapM_ print $ sequence allOptions
