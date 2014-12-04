{-# LANGUAGE TupleSections #-}

module Conjure.UI.TypeCheck ( typeCheckModel ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Process.Enums ( removeEnumsFromModel )
import Conjure.Process.Unnameds ( removeUnnamedsFromModel )
import Conjure.Language.NameResolution ( resolveNames )


typeCheckModel
    :: ( MonadFail m
       , MonadLog m
       )
    => Model      -- essence param
    -> m ()

typeCheckModel model0 = do
    model1 <- return model0             >>= logDebugId "[input]"
          >>= removeUnnamedsFromModel   >>= logDebugId "[removeUnnamedsFromModel]"
          >>= removeEnumsFromModel      >>= logDebugId "[removeEnumsFromModel]"
          >>= resolveNames              >>= logDebugId "[resolveNames]"
    errs <- execWriterT $ forM (mStatements model1) $ \ st ->
        case st of
            Declaration{} -> return ()
            SearchOrder{} -> return ()
            Where xs -> forM_ xs $ \ x -> do
                ty <- typeOf x
                case ty of
                    TypeBool{} -> return ()
                    _ -> tell [ "In a 'where' statement:" <++> pretty x
                              , "Expected type `bool`, but got:" <++> pretty ty
                              ]
            Objective _ x -> do
                ty <- typeOf x
                case ty of
                    TypeInt{} -> return ()
                    _ -> tell [ "In the objective:" <++> pretty st
                              , "Expected type `int`, but got:" <++> pretty ty
                              ]
            SuchThat xs -> forM_ xs $ \ x -> do
                ty <- typeOf x
                case ty of
                    TypeBool{} -> return ()
                    _ -> tell [ "In a 'such that' statement:" <++> pretty x
                              , "Expected type `bool`, but got:" <++> pretty ty
                              ]
    unless (null errs)
        (fail $ vcat $ "There were type errors."
                     : ""
                     : errs)
