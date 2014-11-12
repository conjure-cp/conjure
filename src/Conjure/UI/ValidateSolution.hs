module Conjure.UI.ValidateSolution ( validateSolution ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enums ( deenumifyParam )


validateSolution
    :: ( MonadFail m
       , MonadLog m
       )
    => Model      -- essence model
    -> Model      -- essence param
    -> Model      -- essence solution
    -> m ()
validateSolution essenceModel essenceParam' essenceSolution' = flip evalStateT [] $ do
    essenceParam    <- deenumifyParam essenceModel essenceParam'
    essenceSolution <- deenumifyParam essenceModel essenceSolution'
    forM_ (mStatements essenceModel) $ \ st -> case st of
        Declaration (FindOrGiven Given nm dom) ->
            case [ val | Declaration (Letting nm2 val) <- mStatements essenceParam, nm == nm2 ] of
                [val] -> modify ((nm, val) :)
                []    -> fail $ vcat [ "No value for" <+> pretty nm <+> "in the parameter file."
                                     , "Its domain:" <++> pretty dom
                                     ]
                vals  -> fail $ vcat [ "Multiple values for" <+> pretty nm <+> "in the parameter file."
                                     , "Its domain:" <++> pretty dom
                                     , "Values:" <++> vcat (map pretty vals)
                                     ]
        Declaration (FindOrGiven Find nm dom) ->
            case [ val | Declaration (Letting nm2 val) <- mStatements essenceSolution, nm == nm2 ] of
                [val] -> modify ((nm,val) :)
                []    -> fail $ vcat [ "No value for" <+> pretty nm <+> "in the solution file."
                                     , "Its domain:" <++> pretty dom
                                     ]
                vals  -> fail $ vcat [ "Multiple values for" <+> pretty nm <+> "in the solution file."
                                     , "Its domain:" <++> pretty dom
                                     , "Values:" <++> vcat (map pretty vals)
                                     ]
        Declaration (Letting nm val) -> modify ((nm, val) :)
        Declaration GivenDomainDefnEnum{}      -> fail $ "not expected here, statement:" <+> pretty st
        Declaration LettingDomainDefnEnum{}    -> fail $ "not expected here, statement:" <+> pretty st
        Declaration LettingDomainDefnUnnamed{} -> fail $ "not expected here, statement:" <+> pretty st
        SearchOrder{} -> return ()
        Where xs -> do
            vals     <- gets id
            forM_ xs $ \ x -> do
                constant <- instantiateExpression vals x
                case constant of
                    ConstantBool True -> return ()
                    _ -> fail $ "Invalid." <++> vcat [ "Statement evaluates to:" <+> pretty constant
                                                     , "Original statement was:" <+> pretty x
                                                     ]
        Objective{} -> return ()
        SuchThat xs -> do
            vals     <- gets id
            forM_ xs $ \ x -> do
                constant <- instantiateExpression vals x
                case constant of
                    ConstantBool True -> return ()
                    _ -> fail $ "Invalid." <++> vcat [ "Statement evaluates to:" <+> pretty constant
                                                     , "Original statement was:" <+> pretty x
                                                     ]
