module Conjure.UI.TranslateSolution ( translateSolution ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Representations ( up )


translateSolution
    :: ( MonadFail m
       , MonadLog m
       )
    => Model      -- eprime model
    -> Model      -- essence param
    -> Model      -- eprime solution
    -> m Model    -- essence solution
translateSolution eprimeModel essenceParam eprimeSolution = do

    let eprimeLettingsForEnums =
            [ (nm, fromInt (length vals))
            | nm1                                          <- eprimeModel |> mInfo |> miEnumGivens
            , Declaration (LettingDomainDefnEnum nm2 vals) <- essenceParam |> mStatements 
            , nm1 == nm2
            , let nm = nm1 `mappend` "_EnumSize"
            ]

    logDebug $ "translateSolution eprimeLettingsForEnums:" <+> prettyList id "," eprimeLettingsForEnums

    let eprimeLettings = extractLettings essenceParam ++
                         extractLettings eprimeSolution ++
                         eprimeLettingsForEnums
    let essenceFindNames = eprimeModel |> mInfo |> miFinds
    let essenceFinds     = eprimeModel |> mInfo |> miRepresentations
                                       |> filter (\ (n,_) -> n `elem` essenceFindNames )

    eprimeLettings' <- forM eprimeLettings $ \ (name, val) -> do
        constant <- instantiateExpression eprimeLettings val
        return (name, constant)

    essenceFinds' <- forM essenceFinds $ \ (name, dom) -> do
        constant <- instantiateDomain eprimeLettings dom
        return (name, constant)

    essenceLettings <- forM essenceFinds' $ \ (name, domain) -> do
        (_, constant) <- up eprimeLettings' (name, domain)
        let origDomain = eprimeModel
                |> mInfo |> miOriginalDeclarations
                |> lookup name
                |> fromMaybe (bug ("Missing original domain for:" <+> pretty name))
        return (name, origDomain, constant)

    let
        intToEnumConstant :: [((Int, Name), Constant)]
        intToEnumConstant = concat $
            [ [ ((i,ename), ConstantEnum ename vals v)
              | (i,v) <- zip allNats vals
              ]
            | Declaration (LettingDomainDefnEnum ename vals)
                    <- mStatements essenceParam
                    ++ eprimeModel |> mInfo |> miEnumLettings |> map Declaration
            ]

    logDebug $ "translateSolution intToEnumConstant" <+> prettyList id "," (map show intToEnumConstant)

    return def
        { mStatements = sortNub
            [ Declaration (Letting n (Constant y))
            | (n, d, x) <- essenceLettings
            , let y = enumify intToEnumConstant d x
            ]
        }


extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model ]


-- | Using the original domains from the Essence file.
--   Converting integers back to enum constants.
-- TODO: complete enumify
enumify
    :: [((Int, Name), Constant)]
    -> Domain () Expression
    -> Constant
    -> Constant
enumify ctxt (DomainEnum ename _) (ConstantInt i) =
    fromMaybe (bug $ "enumify:" <+> pretty (i, ename))
              (lookup (i, ename) ctxt)
enumify ctxt (DomainReference ename _) (ConstantInt i) =
    fromMaybe (bug $ "enumify:" <+> pretty (i, ename))
              (lookup (i, ename) ctxt)
enumify ctxt (DomainTuple ds) (ConstantTuple cs) =
    ConstantTuple [ enumify ctxt d c
                  | (d,c) <- zip ds cs ]

enumify _ DomainBool  c = c
enumify _ DomainInt{} c = c

enumify _ dom _ = bug ("enumify:" <+> pretty dom)

