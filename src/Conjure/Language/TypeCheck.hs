module Conjure.Language.TypeCheck
    ( typeCheckModel
    , typeCheckModelIO
    , typeOfDomain
    , typeOfExpression
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition


typeCheckModelIO :: Model -> IO ()
typeCheckModelIO m =
    case typeCheckModel m of
        Nothing -> return ()
        Just msg -> userErr $ sep ["Type error, specifically:", msg]


-- | returns `Just msg` if the model is type-incorrect, msg being an explanation.
--   returns `Nothing` if the model is type-correct.
typeCheckModel :: Model -> Maybe Doc
typeCheckModel _ = Nothing
-- typeCheckModel _ = Just "Just Plain Wrong (TM)"

typeOfDomain :: Domain r c -> Type
typeOfDomain DomainBool                = TypeBool
typeOfDomain DomainInt{}               = TypeInt
typeOfDomain (DomainEnum    defn _   ) = TypeEnum defn
typeOfDomain (DomainUnnamed defn     ) = TypeUnnamed defn
typeOfDomain (DomainTuple         xs ) = TypeTuple      (map typeOfDomain xs)
typeOfDomain (DomainMatrix ind inn   ) = TypeMatrix     (typeOfDomain ind) (typeOfDomain inn)
typeOfDomain (DomainSet       _ _ x  ) = TypeSet        (typeOfDomain x)
typeOfDomain (DomainMSet      _ _ x  ) = TypeMSet       (typeOfDomain x)
typeOfDomain (DomainFunction  _ _ x y) = TypeFunction   (typeOfDomain x) (typeOfDomain y)
typeOfDomain (DomainRelation  _ _ xs ) = TypeRelation   (map typeOfDomain xs)
typeOfDomain (DomainPartition _ _ x  ) = TypePartition  (typeOfDomain x)
typeOfDomain DomainOp{}                = bug "typeOfDomain"
typeOfDomain DomainHack{}              = bug "typeOfDomain"

typeOfExpression :: Expression -> Type
typeOfExpression (Constant x) = typeOfConstant x
typeOfExpression (Domain x)   = typeOfDomain x
typeOfExpression Reference{}  = TypeAny -- TODO: fix
typeOfExpression Op{}         = TypeAny -- TODO: fix
typeOfExpression Lambda{}     = TypeAny -- TODO: fix

typeOfConstant :: Constant -> Type
typeOfConstant ConstantBool{}            = TypeBool
typeOfConstant ConstantInt{}             = TypeInt
typeOfConstant (ConstantEnum defn _    ) = TypeEnum defn
typeOfConstant (ConstantTuple        xs) = TypeTuple (map typeOfConstant xs)
typeOfConstant (ConstantMatrix ind inn ) = TypeMatrix (typeOfDomain ind) (homoType (map typeOfConstant inn))
typeOfConstant (ConstantSet         xs ) = TypeSet  (homoType (map typeOfConstant xs))
typeOfConstant (ConstantMSet        xs ) = TypeMSet (homoType (map typeOfConstant xs))
typeOfConstant (ConstantFunction    xs ) = TypeFunction (homoType (map (typeOfConstant . fst) xs))
                                                        (homoType (map (typeOfConstant . fst) xs))
typeOfConstant (ConstantRelation    xss) =
    case homoType $ map (typeOfConstant . ConstantTuple) xss of
        TypeTuple ts -> TypeRelation ts
        _ -> bug "expecting TypeTuple in typeOfConstant"
typeOfConstant (ConstantPartition   xss) = TypePartition (homoType (map typeOfConstant (concat xss)))

homoType :: [Type] -> Type
homoType [] = userErr "empty collection, what's the type?"
homoType (x:xs) =
    if all (==x) xs
        then x
        else userErr "not a homoType"

