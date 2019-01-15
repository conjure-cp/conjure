{-# LANGUAGE TupleSections #-}

module Conjure.UI.TypeCheck ( typeCheckModel_StandAlone, typeCheckModel ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.CategoryOf ( categoryChecking )
import Conjure.Language.Pretty
import Conjure.Language.Lenses
import Conjure.Process.Enums ( removeEnumsFromModel )
import Conjure.Process.Unnameds ( removeUnnamedsFromModel )
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Process.Sanity ( sanityChecks )



typeCheckModel_StandAlone
    :: ( MonadFail m
       , MonadUserError m
       , MonadLog m
       , NameGen m
       , ?typeCheckerMode :: TypeCheckerMode
       )
    => Model
    -> m Model
typeCheckModel_StandAlone model0 = do
    -- for better error messages, type-check and category-check before sanity-checking.
    -- sanity checking will modify the model.
    -- then, type-check once more just in case the newly generated
    -- stuff is broken.
    model1 <- return model0             >>= logDebugId "[input]"
          >>= removeUnnamedsFromModel   >>= logDebugId "[removeUnnamedsFromModel]"
          >>= removeEnumsFromModel      >>= logDebugId "[removeEnumsFromModel]"
          >>= resolveNames              >>= logDebugId "[resolveNames]"
          >>= typeCheckModel            >>= logDebugId "[typeCheckModel]"
          >>= categoryChecking          >>= logDebugId "[categoryChecking]"
          >>= sanityChecks              >>= logDebugId "[sanityChecks]"
          >>= typeCheckModel            >>= logDebugId "[typeCheckModel]"
    return model1


typeCheckModel
    :: ( MonadFail m
       , MonadUserError m
       , ?typeCheckerMode :: TypeCheckerMode
       )
    => Model
    -> m Model
typeCheckModel model1 = do
    let model2 = fixRelationProj model1
    (statements3, errs) <- runWriterT $ forM (mStatements model2) $ \ st ->
        case st of
            Declaration decl -> do
                case decl of
                    FindOrGiven _ _ domain -> do
                        mty <- runExceptT $ typeOf domain
                        case mty of
                            Right _ -> return ()
                            Left err -> tell $ return $ vcat
                                [ "In a declaration statement:" <++> pretty st
                                , "Error:" <++> pretty err
                                ]
                    Letting _ x -> do
                        mty <- runExceptT $ typeOf x
                        case mty of
                            Right _ -> return ()
                            Left err -> tell $ return $ vcat
                                [ "In a letting statement:" <++> pretty st
                                , "Error:" <++> pretty err
                                ]
                    GivenDomainDefnEnum{} -> return ()
                    LettingDomainDefnEnum{} -> return ()
                    LettingDomainDefnUnnamed _ x -> do
                        mty <- runExceptT $ typeOf x
                        case mty of
                            Right TypeInt{} -> return ()
                            Left err -> tell $ return $ vcat
                                [ "In the declaration of an unnamed type:" <++> pretty st
                                , "Error:" <++> pretty err
                                ]
                            Right ty -> tell $ return $ vcat
                                [ "In the declaration of an unnamed type:" <++> pretty st
                                , "Expected type `int`, but got:" <++> pretty ty
                                ]
                return st
            SearchOrder xs -> do
                forM_ xs $ \case
                    BranchingOn{} -> return ()                          -- TODO: check if the name is defined
                    Cut x -> do
                        mty <- runExceptT $ typeOf x
                        case mty of
                            Right TypeBool{} -> return ()
                            Left err -> tell $ return $ vcat
                                [ "In a 'branching on' statement:" <++> pretty x
                                , "Error:" <++> pretty err
                                ]
                            Right ty -> tell $ return $ vcat
                                [ "In a 'branching on' statement:" <++> pretty x
                                , "Expected type `bool`, but got:" <++> pretty ty
                                ]
                return st
            SearchHeuristic{} -> return st
            Where xs -> do
                xs' <- forM xs $ \ x -> do
                    mty <- runExceptT $ typeOf x
                    case mty of
                        Right TypeBool{} -> return x
                        Right (TypeList TypeBool) -> return (make opAnd x)
                        Right (TypeMatrix _ TypeBool) -> return (make opAnd x)
                        Left err -> do
                            tell $ return $ vcat
                                [ "In a 'where' statement:" <++> pretty x
                                , "Error:" <++> pretty err
                                ]
                            return x
                        Right ty -> do
                            tell $ return $ vcat
                                [ "In a 'where' statement:" <++> pretty x
                                , "Expected type `bool`, but got:" <++> pretty ty
                                ]
                            return x
                return (Where xs')
            Objective _ x -> do
                mty <- runExceptT $ typeOf x
                case mty of
                    Right TypeInt{} -> return ()
                    Left err -> tell $ return $ vcat
                        [ "In the objective:" <++> pretty st
                        , "Error:" <++> pretty err
                        ]
                    Right ty -> tell $ return $ vcat
                        [ "In the objective:" <++> pretty st
                        , "Expected type `int`, but got:" <++> pretty ty
                        ]
                return st
            SuchThat xs -> do
                xs' <- forM xs $ \ x -> do
                    mty <- runExceptT $ typeOf x
                    case mty of
                        Right TypeBool{} -> return x
                        Right (TypeList TypeBool) -> return (make opAnd x)
                        Right (TypeMatrix _ TypeBool) -> return (make opAnd x)
                        Left err -> do
                            tell $ return $ vcat
                                [ "In a 'such that' statement:" <++> pretty x
                                , "Error:" <++> pretty err
                                ]
                            return x
                        Right ty -> do
                            tell $ return $ vcat
                                [ "In a 'such that' statement:" <++> pretty x
                                , "Expected type `bool`, but got:" <++> pretty ty
                                ]
                            return x
                return (SuchThat xs')
            SNS_Group{}                -> return st         -- TODO
            SNS_Neighbourhood{}        -> return st         -- TODO
            SNS_Out_Neighbourhood{}    -> return st         -- TODO
            SNS_Out_IncumbentMapping{} -> return st         -- TODO
    unless (null errs) (userErr errs)

    -- now that everything knows its type, we can recover
    -- DomainInt [RangeSingle x] from DomainIntE x, if x has type int
    let
        domainIntERecover :: forall m . MonadFail m => Domain () Expression -> m (Domain () Expression)
        domainIntERecover d@(DomainIntE x) = do
            ty <- typeOf x
            return $ case ty of
                TypeInt t -> DomainInt t [RangeSingle x]
                _       -> d
        domainIntERecover d = return d
    statements4 <- transformBiM domainIntERecover statements3

    return model2 { mStatements = statements4 }

