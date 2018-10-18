-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Partition.Occurrence ( partitionOccurrence ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )


-- | works for "partition from A", where A can be used as an index of a matrix
--   _WhichPart: matrix indexed by [A] of int(1..maxNumParts)
--      (indicating which part an element belongs to)
--   _NumParts : int(1..maxNumParts)
--      (indicating the total number of parts)
--   only use part numbers from 1.._NumParts, never use the others
--      part(i) is used -> part(i-1) is used, forAll i:int(3..maxNumParts)
partitionOccurrence :: forall m . (MonadFail m, NameGen m, EnumerateDomain m) => Representation m
partitionOccurrence = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainPartition _ attrs innerDomain)
            | domainCanIndexMatrix innerDomain
            = map (DomainPartition Partition_Occurrence attrs) <$> f innerDomain
        chck _ _ = return []

        nameNumParts   = mkOutName (Just "NumParts")
        nameWhichPart  = mkOutName (Just "WhichPart")
        namePartSizes  = mkOutName (Just "PartSizes")
        nameFirstIndex = mkOutName (Just "FirstIndex")

        getMaxNumParts attrs d = 
            case partsNum attrs of
                SizeAttr_Size       x   -> return x
                SizeAttr_MaxSize    x   -> return x
                SizeAttr_MinMaxSize _ x -> return x
                _ -> domainSizeOf d

        getMaxPartSizes attrs d =
            case partsSize attrs of
                SizeAttr_Size       x   -> return x
                SizeAttr_MaxSize    x   -> return x
                SizeAttr_MinMaxSize _ x -> return x
                _ -> domainSizeOf d

        -- downD :: TypeOf_DownD m
        downD (name, domain@(DomainPartition Partition_Occurrence attrs innerDomain))
            | domainCanIndexMatrix innerDomain = do
            maxNumParts <- getMaxNumParts attrs innerDomain
            maxPartSizes <- getMaxPartSizes attrs innerDomain
            return $ Just
                [
                -- number of active parts
                  ( nameNumParts domain name
                  , DomainInt Nothing [RangeBounded 1 maxNumParts]
                  )
                -- for each element, the part it belongs to
                , ( nameWhichPart domain name
                  , DomainMatrix
                      (forgetRepr innerDomain)
                      (DomainInt Nothing [RangeBounded 1 maxNumParts])
                  )
                -- for each part, number of elements in the part
                , ( namePartSizes domain name
                  , DomainMatrix
                      (DomainInt Nothing [RangeBounded 1 maxNumParts])
                      (DomainInt Nothing [RangeBounded 0 maxPartSizes])
                  )
                -- wtf was this?
                , ( nameFirstIndex domain name
                  , DomainMatrix
                      (DomainInt Nothing [RangeBounded 1 maxNumParts])
                      innerDomain                                           -- dontCare if not used
                  )
                ]
        downD _ = na "{downD} Occurrence"

        structuralCons :: TypeOf_Structural m
        structuralCons _ downX1 (DomainPartition _ attrs innerDomain)
                | domainCanIndexMatrix innerDomain = do
            maxNumParts <- getMaxNumParts attrs innerDomain
            let
                numPartsChannelling whichPart numPartsVar = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            &numPartsVar =
                                max([ &whichPart[&i] | &iPat : &innerDomain ])
                        |]

                partSizesChannelling whichPart partSizesVar = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ &partSizesVar[&i] = sum([ 1 | &jPat : &innerDomain , &whichPart[&j] = &i ])
                                | &iPat : int(1..&maxNumParts)
                                ])
                        |]

                firstIndexChannelling whichPart numPartsVar firstIndexVar = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return
                        [ -- firstIndexVar[i] is <= all indices belonging to part i
                          [essence|
                            forAll &iPat : int(1..&maxNumParts) , &i <= &numPartsVar .
                                forAll &jPat : &innerDomain .
                                    &whichPart[&j] = &i -> &firstIndexVar[&i] <= &j
                          |]
                        , -- firstIndexVar[i] is equal to one of those
                          [essence|
                            forAll &iPat : int(1..&maxNumParts) , &i <= &numPartsVar .
                                exists &jPat : &innerDomain .
                                    &whichPart[&j] = &i /\ &firstIndexVar[&i] = &j
                          |]
                        , -- firstIndexVar[i] is dontCare, if nothing is in part i
                          [essence|
                            forAll &iPat : int(1..&maxNumParts) , &i > &numPartsVar .
                                dontCare(&firstIndexVar[&i])
                          |]
                        ]

                symmetryBreaking numPartsVar firstIndexVar = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                          forAll &iPat, &jPat : int(1..&maxNumParts) , &i <= &numPartsVar /\ &j <= &numPartsVar .
                              &i < &j <-> &firstIndexVar[&i] < &firstIndexVar[&j]
                        |]

                numPartsCons numPartsVar =
                    return $ mkSizeCons (partsNum attrs) numPartsVar

                partSizeCons numPartsVar partSizesVar = do
                    (iPat, i) <- quantifiedVar
                    let theConsForI = make opAnd $ fromList $
                            mkSizeCons (partsSize attrs) [essence| &partSizesVar[&i] |]
                    return
                        [   [essence|
                                and([ &theConsForI
                                    | &iPat : int(1..&maxNumParts)          $ forAll part numbers
                                    , &i <= &numPartsVar                    $ that are active
                                    ])
                            |]
                        ,   [essence|
                                and([ &partSizesVar[&i] = 0
                                    | &iPat : int(1..&maxNumParts)          $ forAll part numbers
                                    , &i > &numPartsVar                     $ that are inactive
                                    ])
                            |]
                        ]

                noGaps whichPart numPartsVar = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ or([ &whichPart[&j] = &i                  $ there must be a member in that part
                                     | &jPat : &innerDomain
                                     ])
                                | &iPat : int(3..&maxNumParts)              $ forAll part numbers (except 1 and 2)
                                , &i <= &numPartsVar                        $ that are active
                                ])
                        |]

                fixedPartSize =
                    case attrs of
                        PartitionAttr _ SizeAttr_Size{} _ -> True
                        _                                 -> False

                regular numPartsVar partSizesVar | isRegular attrs && not fixedPartSize = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ &partSizesVar[&i-1] = &partSizesVar[&i]
                                | &iPat : int(2..&maxNumParts)
                                , &i <= &numPartsVar
                                ])
                        |]
                regular _ _ = return []

            return $ \ inpPartition -> do
                [numPartsVar, whichPart, partSizesVar, firstIndexVar] <- downX1 inpPartition
                concat <$> sequence
                    [ partSizeCons                    numPartsVar partSizesVar
                    , numPartsCons                    numPartsVar
                    , noGaps                whichPart numPartsVar
                    , regular                         numPartsVar partSizesVar
                    , numPartsChannelling   whichPart numPartsVar
                    , partSizesChannelling  whichPart             partSizesVar
                    , firstIndexChannelling whichPart numPartsVar              firstIndexVar
                    , symmetryBreaking                numPartsVar              firstIndexVar
                    ]
        structuralCons _ _ domain = na $ vcat [ "{structuralCons} Occurrence"
                                              , "domain:" <+> pretty domain
                                              ]

        downC :: TypeOf_DownC m
        downC ( name
              , inDom@(DomainPartition Partition_Occurrence attrs innerDomain)
              , inConstant@(ConstantAbstract (AbsLitPartition vals))
              ) = do
            Just [ ( numPartsVar   , numPartsDom   )
                 , ( whichPart     , whichPartDom  )
                 , ( partSizesVar  , partSizesDom  )
                 , ( firstIndexVar , firstIndexDom )
                 ] <- downD (name, inDom)
            members      <- domainValues innerDomain
            maxNumParts' <- getMaxNumParts attrs innerDomain
            maxNumParts  <- case viewConstantInt maxNumParts' of
                Just i -> return i
                Nothing -> bug ("expecting an integer literal, but got:" <++> pretty maxNumParts')
            z <- zeroVal innerDomain
            let
                whichPartValInside :: [(Integer, Constant)]
                whichPartValInside =
                        [ case whichPartIsIt of
                            [p] -> p
                            []  -> bug $ vcat [ "Not found:" <+> pretty mem
                                              , "Inside:" <+> pretty inConstant
                                              ]
                            _   -> bug $ vcat [ "Found multiple times:" <+> pretty mem
                                              , "Inside:" <+> pretty inConstant
                                              ]
                        | mem <- members
                        , let whichPartIsIt = [ (p, mem)
                                              | (p, pVals) <- zip [1..] vals
                                              , mem `elem` pVals
                                              ]
                        ]
                numPartsVal   = ConstantInt Nothing (genericLength vals)
                whichPartVal  = ConstantAbstract (AbsLitMatrix
                                    (forgetRepr innerDomain)
                                    (map (ConstantInt Nothing . fst) whichPartValInside))
                partSizesVal  = ConstantAbstract (AbsLitMatrix
                                    (DomainInt Nothing [RangeBounded 1 maxNumParts'])
                                    (map (ConstantInt Nothing . genericLength) vals
                                        ++ replicate (fromInteger (maxNumParts - genericLength vals))
                                                     (ConstantInt Nothing 0)))
                firstIndexVal = ConstantAbstract (AbsLitMatrix
                                    (DomainInt Nothing [RangeBounded 1 maxNumParts'])
                                    ([ case lookup p whichPartValInside of
                                        Nothing -> bug $ vcat [ "Not found:" <+> pretty p
                                                              , "Inside:" <+> prettyList id "," whichPartValInside
                                                              ]
                                        Just i  -> i
                                     | p <- [1..genericLength vals] ]
                                        ++ replicate (fromInteger (maxNumParts - genericLength vals))
                                                     z))
            return $ Just
                [ ( numPartsVar   , numPartsDom   , numPartsVal   )
                , ( whichPart     , whichPartDom  , whichPartVal  )
                , ( partSizesVar  , partSizesDom  , partSizesVal  )
                , ( firstIndexVar , firstIndexDom , firstIndexVal )
                ]
        downC (name, domain, constant) = na $ vcat [ "{downC} Occurrence"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainPartition Partition_Occurrence _ innerDomain)) =
            case (lookup (nameNumParts domain name) ctxt, lookup (nameWhichPart domain name) ctxt) of
                ( Just (viewConstantInt -> Just numPartsValue) ,
                  Just (viewConstantMatrix -> Just (_, whichPartValues)) ) -> do
                    members <- domainValues innerDomain
                    return
                        ( name
                        , normaliseConstant $ ConstantAbstract $ AbsLitPartition
                            [ [ member | (member, b) <- zip members whichPartValues, b == ConstantInt Nothing bucket ]
                            | bucket <- [1..numPartsValue]
                            ]
                        )
                (Just val, _) -> fail $ vcat $
                    [ "(in Partition Occurrence up)"
                    , "Expecting an integer literal for:" <+> pretty (nameNumParts domain name)
                    , "But got:" <+> pretty val
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Just val) -> fail $ vcat $
                    [ "(in Partition Occurrence up)"
                    , "Expecting a matrix literal for:" <+> pretty (nameWhichPart domain name)
                    , "But got:" <+> pretty val
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (Nothing, _) -> fail $ vcat $
                    [ "(in Partition Occurrence up)"
                    , "No value for:" <+> pretty (nameNumParts domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up ctxt (name, domain) =
            na $ vcat [ "{up} Occurrence"
                      , "name:" <+> pretty name
                      , "domain:" <+> pretty domain
                      , "ctxt:" <+> vcat (map pretty ctxt)
                      ]
