module Conjure.UI.TranslateSolution ( translateSolution ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Constant ( normaliseConstant )
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enums ( deenumifyParam, enumify )
import Conjure.Representations ( up )


translateSolution
    :: ( MonadFail m
       , MonadLog m
       )
    => Model      -- eprime model
    -> Model      -- essence param
    -> Model      -- eprime solution
    -> m Model    -- essence solution
translateSolution eprimeModel essenceParam' eprimeSolution = do

    essenceParam <- deenumifyParam eprimeModel essenceParam'

    let eprimeLettingsForEnums =
            [ (nm, fromInt (length vals))
            | nm1                                          <- eprimeModel |> mInfo |> miEnumGivens
            , Declaration (LettingDomainDefnEnum nm2 vals) <- essenceParam' |> mStatements 
            , nm1 == nm2
            , let nm = nm1 `mappend` "_EnumSize"
            ]

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
                |> mInfo |> miOriginalDomains
                |> lookup name
                |> fromMaybe (bug ("Missing original domain for:" <+> pretty name))
        return (name, origDomain, constant)

    let
        intToEnumConstant :: [((Int, Name), Constant)]
        intToEnumConstant = concat
            [ [ ((i,ename), ConstantEnum ename vals v)
              | (i,v) <- zip allNats vals
              ]
            | Declaration (LettingDomainDefnEnum ename vals)
                    <- mStatements essenceParam'
                    ++ eprimeModel |> mInfo |> miEnumLettings |> map Declaration
            ]

    return def
        { mStatements = sortNub
            [ Declaration (Letting n (Constant (normaliseConstant y)))
            | (n, d, x) <- essenceLettings
            , let y = enumify intToEnumConstant d x
            ]
        }

extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model
             , not (isDomain x)
             ]
    where isDomain Domain{} = True
          isDomain _ = False

