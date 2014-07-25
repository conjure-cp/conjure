module Conjure.Language.TypeCheck
    ( typeCheckModel
    , typeCheckModelIO
    , typeOf
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

class TypeOf a where
    typeOf :: a -> Type

instance TypeOf (Domain r c) where
    typeOf DomainBool                = TypeBool
    typeOf DomainInt{}               = TypeInt
    typeOf (DomainEnum    defn _   ) = TypeEnum defn
    typeOf (DomainUnnamed defn     ) = TypeUnnamed defn
    typeOf (DomainTuple         xs ) = TypeTuple      (map typeOf xs)
    typeOf (DomainMatrix ind inn   ) = TypeMatrix     (typeOf ind) (typeOf inn)
    typeOf (DomainSet       _ _ x  ) = TypeSet        (typeOf x)
    typeOf (DomainMSet      _ _ x  ) = TypeMSet       (typeOf x)
    typeOf (DomainFunction  _ _ x y) = TypeFunction   (typeOf x) (typeOf y)
    typeOf (DomainRelation  _ _ xs ) = TypeRelation   (map typeOf xs)
    typeOf (DomainPartition _ _ x  ) = TypePartition  (typeOf x)
    typeOf DomainOp{}                = TypeAny -- TODO: fix
    typeOf DomainHack{}              = TypeAny -- TODO: fix

instance TypeOf Expression where
    typeOf (Constant x) = typeOf x
    typeOf (AbstractLiteral x) = typeOf x
    typeOf (Domain x)   = typeOf x
    typeOf Reference{}  = TypeAny -- TODO: fix
    typeOf (WithLocals x _) = typeOf x                -- TODO: do this properly (looking into locals and other ctxt)
    typeOf Op{}         = TypeAny -- TODO: fix
    typeOf Lambda{}     = TypeAny -- TODO: fix

instance TypeOf Constant where
    typeOf ConstantBool{}            = TypeBool
    typeOf ConstantInt{}             = TypeInt
    typeOf (ConstantEnum defn _    ) = TypeEnum defn
    typeOf (ConstantTuple        xs) = TypeTuple    (map typeOf xs)
    typeOf (ConstantMatrix ind inn ) = TypeMatrix   (typeOf ind) (homoType (map typeOf inn))
    typeOf (ConstantSet         xs ) = TypeSet      (homoType (map typeOf xs))
    typeOf (ConstantMSet        xs ) = TypeMSet     (homoType (map typeOf xs))
    typeOf (ConstantFunction    xs ) = TypeFunction (homoType (map (typeOf . fst) xs))
                                                    (homoType (map (typeOf . fst) xs))
    typeOf (ConstantRelation    xss) =
        case homoType $ map (typeOf . ConstantTuple) xss of
            TypeTuple ts -> TypeRelation ts
            _ -> bug "expecting TypeTuple in typeOf"
    typeOf (ConstantPartition   xss) = TypePartition (homoType (map typeOf (concat xss)))

instance TypeOf a => TypeOf (AbstractLiteral a) where
    typeOf (AbsLitTuple        xs) = TypeTuple    (map typeOf xs)
    typeOf (AbsLitMatrix ind inn ) = TypeMatrix   (typeOf ind) (homoType (map typeOf inn))
    typeOf (AbsLitSet         xs ) = TypeSet      (homoType (map typeOf xs))
    typeOf (AbsLitMSet        xs ) = TypeMSet     (homoType (map typeOf xs))
    typeOf (AbsLitFunction    xs ) = TypeFunction (homoType (map (typeOf . fst) xs))
                                                  (homoType (map (typeOf . fst) xs))
    typeOf (AbsLitRelation    xss) =
        case homoType $ map (typeOf . AbsLitTuple) xss of
            TypeTuple ts -> TypeRelation ts
            _ -> bug "expecting TypeTuple in typeOf"
    typeOf (AbsLitPartition   xss) = TypePartition (homoType (map typeOf (concat xss)))

homoType :: [Type] -> Type
homoType [] = userErr "empty collection, what's the type?"
homoType (x:xs) =
    if all (==x) xs
        then x
        else userErr "not a homoType"

