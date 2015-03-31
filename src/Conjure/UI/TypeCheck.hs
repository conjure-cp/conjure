{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Conjure.UI.TypeCheck ( typeCheckModel ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Language.Lenses
import Conjure.Process.Enums ( removeEnumsFromModel )
import Conjure.Process.Unnameds ( removeUnnamedsFromModel )
import Conjure.Language.NameResolution ( resolveNames )


typeCheckModel
    :: forall m .
       ( MonadFail m
       , MonadLog m
       , NameGen m
       )
    => Model
    -> m Model

typeCheckModel model0 = do
    model1 <- return model0             >>= logDebugId "[input]"
          >>= removeUnnamedsFromModel   >>= logDebugId "[removeUnnamedsFromModel]"
          >>= removeEnumsFromModel      >>= logDebugId "[removeEnumsFromModel]"
          >>= resolveNames              >>= logDebugId "[resolveNames]"
    errs <- execWriterT $ forM (mStatements model1) $ \ st ->
        case st of
            Declaration{} -> return ()
            SearchOrder xs -> forM_ xs $ \case
                BranchingOn{} -> return ()                      -- TODO: check if the name is defined
                Cut x -> do
                    mty <- runExceptT $ typeOf x
                    case mty of
                        Right TypeBool{} -> return ()
                        Left err -> tell
                            [ "In a 'branching on' statement:" <++> pretty x
                            , "Error:" <++> pretty err
                            ]
                        Right ty -> tell
                            [ "In a 'branching on' statement:" <++> pretty x
                            , "Expected type `bool`, but got:" <++> pretty ty
                            ]
            Where xs -> forM_ xs $ \ x -> do
                mty <- runExceptT $ typeOf x
                case mty of
                    Right TypeBool{} -> return ()
                    Left err -> tell
                        [ "In a 'where' statement:" <++> pretty x
                        , "Error:" <++> pretty err
                        ]
                    Right ty -> tell
                        [ "In a 'where' statement:" <++> pretty x
                        , "Expected type `bool`, but got:" <++> pretty ty
                        ]
            Objective _ x -> do
                mty <- runExceptT $ typeOf x
                case mty of
                    Right TypeInt{} -> return ()
                    Left err -> tell
                        [ "In the objective:" <++> pretty st
                        , "Error:" <++> pretty err
                        ]
                    Right ty -> tell
                        [ "In the objective:" <++> pretty st
                        , "Expected type `int`, but got:" <++> pretty ty
                        ]
            SuchThat xs -> forM_ xs $ \ x -> do
                mty <- runExceptT $ typeOf x
                case mty of
                    Right TypeBool{} -> return ()
                    Left err -> tell
                        [ "In a 'such that' statement:" <++> pretty x
                        , "Error:" <++> pretty err
                        ]
                    Right ty -> tell
                        [ "In a 'such that' statement:" <++> pretty x
                        , "Expected type `bool`, but got:" <++> pretty ty
                        ]
    unless (null errs)
        (fail $ vcat $ "There were type errors."
                     : ""
                     : errs)

    let
        fixRelationProj :: Expression -> m Expression
        fixRelationProj p = case match opRelationProj p of
            Just (f, [Just arg]) -> do
                tyF <- typeOf f
                return $ case tyF of
                    TypeFunction{} -> make opImage f arg
                    TypeSequence{} -> make opImage f arg
                    _              -> p
            _ -> return p

    transformBiM fixRelationProj model1

