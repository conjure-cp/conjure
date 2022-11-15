{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.DontCare where

import Conjure.Rules.Import
import Conjure.Process.Enumerate ( EnumerateDomain )


rule_Bool :: Rule
rule_Bool = "dontCare-bool" `namedRule` theRule where
    theRule p = do
        x          <- match opDontCare p
        DomainBool <- domainOf x
        return
            ( "dontCare value for bools is false."
            , return $ make opEq x (fromBool False)
            )


rule_Int :: Rule
rule_Int = "dontCare-int" `namedRule` theRule where
    theRule p = do
        x         <- match opDontCare p
        TypeInt t <- typeOf x
        xDomain   <- domainOf x
        let raiseBug = bug ("dontCare on domain:" <+> pretty xDomain)
        let val = reTag t $ case xDomain of
                DomainInt _ [] -> raiseBug
                DomainInt _ (r:_) -> case r of
                    RangeOpen -> raiseBug
                    RangeSingle v -> v
                    RangeLowerBounded v -> v
                    RangeUpperBounded v -> v
                    RangeBounded v _ -> v
                DomainIntE v -> [essence| min(&v) |]
                _ -> raiseBug
        return
            ( "dontCare value for this integer is" <+> pretty val
            , return $ make opEq x val
            )


rule_Tuple :: Rule
rule_Tuple = "dontCare-tuple" `namedRule` theRule where
    theRule p = do
        x           <- match opDontCare p
        TypeTuple{} <- typeOf x
        xs          <- downX1 x
        return
            ( "dontCare handling for tuple"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


rule_Record :: Rule
rule_Record = "dontCare-record" `namedRule` theRule where
    theRule p = do
        x            <- match opDontCare p
        TypeRecord{} <- typeOf x
        xs           <- downX1 x
        return
            ( "dontCare handling for record"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


rule_Variant :: Rule
rule_Variant = "dontCare-variant" `namedRule` theRule where
    theRule p = do
        x             <- match opDontCare p
        TypeVariant{} <- typeOf x
        xs            <- downX1 x
        return
            ( "dontCare handling for variant"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


rule_Matrix :: Rule
rule_Matrix = "dontCare-matrix" `namedRule` theRule where
    theRule p = do
        x                    <- match opDontCare p
        DomainMatrix index _ <- domainOf x
        return
            ( "dontCare handling for matrix"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| forAll &iPat : &index . dontCare(&x[&i]) |]
            )


rule_Abstract :: Rule
rule_Abstract = "dontCare-abstract" `namedRule` theRule where
    theRule p = do
        x  <- match opDontCare p
        ty <- typeOf x
        case ty of
            TypeSet      {} -> return ()
            TypeMSet     {} -> return ()
            TypeSequence {} -> return ()
            TypeFunction {} -> return ()
            TypeRelation {} -> return ()
            TypePartition{} -> return ()
            _ -> na "not a known abstract domain"
        hasRepresentation x
        xs <- downX1 x
        return
            ( "dontCare handling for an abstract domain"
            , return $ make opAnd $ fromList $ map (make opDontCare) xs
            )


handleDontCares ::
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Expression -> m Expression
handleDontCares p =
    case match opDontCare p of
        Nothing -> return p
        Just x -> do
            typX <- typeOf x
            case typX of
                TypeBool -> return (make opEq x (fromBool False))
                TypeInt _ -> do
                    domX <- domainOf x
                    let raiseBug = bug ("dontCare on domain:" <+> pretty domX)
                    let val = case domX of
                            DomainInt _ [] -> raiseBug
                            DomainInt _ (r:_) -> case r of
                                RangeOpen -> raiseBug
                                RangeSingle v -> v
                                RangeLowerBounded v -> v
                                RangeUpperBounded v -> v
                                RangeBounded v _ -> v
                            DomainIntE v -> [essence| min(&v) |]
                            _ -> raiseBug
                    return $ make opEq x val
                TypeTuple{} -> do
                    xs  <- downX1 x
                    xs' <- mapM (handleDontCares . make opDontCare) xs
                    return $ make opAnd $ fromList xs'
                TypeRecord{} -> do
                    xs  <- downX1 x
                    xs' <- mapM (handleDontCares . make opDontCare) xs
                    return $ make opAnd $ fromList xs'
                TypeVariant{} -> do
                    xs  <- downX1 x
                    xs' <- mapM (handleDontCares . make opDontCare) xs
                    return $ make opAnd $ fromList xs'
                TypeMatrix{} -> do
                    domX <- domainOf x
                    case domX of
                        DomainMatrix index _ -> do
                            (iPat@(Single nm), _) <- quantifiedVar
                            -- direct name resolution
                            let i = Reference nm (Just (DeclNoRepr Find nm index NoRegion))
                            inner <- handleDontCares [essence| dontCare(&x[&i]) |]
                            return [essence| forAll &iPat : &index . &inner |]
                        _ -> bug ("dontCare on domain, expecting matrix, but got:" <+> pretty domX)
                _ -> do
                    case representationOf x of
                        Nothing -> failDoc "doesn't seem to have a representation, during handleDontCares"
                        Just _  -> do
                            xs  <- downX1 x
                            xs' <- mapM (handleDontCares . make opDontCare) xs
                            return $ make opAnd $ fromList xs'

