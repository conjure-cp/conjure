{-# LANGUAGE FlexibleContexts #-}

module Conjure.Language.TypeCheck
    ( typeCheckModel
    , typeCheckModelIO
    , typeOf
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Pretty


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
    typeOf
        :: ( Applicative m
           , MonadState [(Name, Domain HasRepresentation Expression)] m
           )
        => a
        -> m Type

instance TypeOf (Domain r c) where
    typeOf DomainBool                = return TypeBool
    typeOf DomainInt{}               = return TypeInt
    typeOf (DomainEnum    defn _   ) = return (TypeEnum defn)
    typeOf (DomainUnnamed defn     ) = return (TypeUnnamed defn)
    typeOf (DomainTuple         xs ) = TypeTuple      <$> (mapM typeOf xs)
    typeOf (DomainMatrix ind inn   ) = TypeMatrix     <$> (typeOf ind) <*> (typeOf inn)
    typeOf (DomainSet       _ _ x  ) = TypeSet        <$> (typeOf x)
    typeOf (DomainMSet      _ _ x  ) = TypeMSet       <$> (typeOf x)
    typeOf (DomainFunction  _ _ x y) = TypeFunction   <$> (typeOf x) <*> (typeOf y)
    typeOf (DomainRelation  _ _ xs ) = TypeRelation   <$> (mapM typeOf xs)
    typeOf (DomainPartition _ _ x  ) = TypePartition  <$> (typeOf x)
    typeOf DomainOp{}                = return TypeAny -- TODO: fix
    typeOf DomainHack{}              = return TypeAny -- TODO: fix

instance TypeOf Expression where
    typeOf (Constant x) = typeOf x
    typeOf (AbstractLiteral x) = typeOf x
    typeOf (Domain x)   = typeOf x
    typeOf (Reference nm) = do
        mdom <- gets (lookup nm)
        case mdom of
             Nothing -> bug ("Type error:" <+> pretty nm)
             Just dom -> typeOf dom
    typeOf (WithLocals x _) = typeOf x                -- TODO: do this properly (looking into locals and other ctxt)
    typeOf (Op op args) = typeOfOp op args
    typeOf Lambda{}     = return TypeAny -- TODO: fix

typeOfOp
    :: ( Applicative m
       , MonadState [(Name, Domain HasRepresentation Expression)] m
       )
    => Name
    -> [Expression]
    -> m Type
typeOfOp "indexing" [m,i] = do
    ty <- typeOf m
    case ty of
        TypeMatrix _ inner -> return inner
        TypeTuple ts -> case i of
            Constant (ConstantInt j) -> return (ts !! (j-1))
            _ -> bug "Tuple indexing out of range."
        _ -> bug "Indexing: neither a matrix nor a tuple."
typeOfOp _ _ = return TypeAny

instance TypeOf Constant where
    typeOf ConstantBool{}            = return TypeBool
    typeOf ConstantInt{}             = return TypeInt
    typeOf (ConstantEnum defn _    ) = return (TypeEnum defn)
    typeOf (ConstantTuple        xs) = TypeTuple    <$> (mapM typeOf xs)
    typeOf (ConstantMatrix ind inn ) = TypeMatrix   <$> (typeOf ind) <*> (homoType <$> (mapM typeOf inn))
    typeOf (ConstantSet         xs ) = TypeSet      <$> (homoType <$> (mapM typeOf xs))
    typeOf (ConstantMSet        xs ) = TypeMSet     <$> (homoType <$> (mapM typeOf xs))
    typeOf (ConstantFunction    xs ) = TypeFunction <$> (homoType <$> (mapM (typeOf . fst) xs))
                                                    <*> (homoType <$> (mapM (typeOf . fst) xs))
    typeOf (ConstantRelation    xss) = do
        ty <- homoType <$> mapM (typeOf . ConstantTuple) xss
        case ty of
            TypeTuple ts -> return (TypeRelation ts)
            _ -> bug "expecting TypeTuple in typeOf"
    typeOf (ConstantPartition   xss) = TypePartition <$> (homoType <$> (mapM typeOf (concat xss)))

instance TypeOf a => TypeOf (AbstractLiteral a) where
    typeOf (AbsLitTuple        xs) = TypeTuple    <$> (mapM typeOf xs)
    typeOf (AbsLitMatrix ind inn ) = TypeMatrix   <$> (typeOf ind) <*> (homoType <$> (mapM typeOf inn))
    typeOf (AbsLitSet         xs ) = TypeSet      <$> (homoType <$> (mapM typeOf xs))
    typeOf (AbsLitMSet        xs ) = TypeMSet     <$> (homoType <$> (mapM typeOf xs))
    typeOf (AbsLitFunction    xs ) = TypeFunction <$> (homoType <$> (mapM (typeOf . fst) xs))
                                                  <*> (homoType <$> (mapM (typeOf . fst) xs))
    typeOf (AbsLitRelation    xss) = do
        ty <- homoType <$> mapM (typeOf . AbsLitTuple) xss
        case ty of
            TypeTuple ts -> return (TypeRelation ts)
            _ -> bug "expecting TypeTuple in typeOf"
    typeOf (AbsLitPartition   xss) = TypePartition <$> (homoType <$> (mapM typeOf (concat xss)))

homoType :: [Type] -> Type
homoType [] = userErr "empty collection, what's the type?"
homoType (x:xs) =
    if all (==x) xs
        then x
        else userErr "not a homoType"

