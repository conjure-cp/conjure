module Conjure.Language.AdHoc where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Type
import Conjure.Language.Name
import Conjure.Language.Pretty

-- aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON ( Value )
import qualified Data.Vector as V               -- vector

-- scientific
import Data.Scientific ( floatingOrInteger )


class ExpressionLike a where
    fromInt :: Integer -> a
    fromIntWithTag :: Integer -> IntTag -> a
    intOut :: MonadFail m => Doc -> a -> m Integer

    fromBool :: Bool -> a
    boolOut :: MonadFail m => a -> m Bool

    fromList :: [a] -> a
    listOut :: MonadFail m => a -> m [a]

class ReferenceContainer a where
    fromName :: Name -> a
    nameOut :: MonadFail m => a -> m Name

class DomainContainer a dom where
    fromDomain :: dom a -> a
    domainOut :: MonadFail m => a -> m (dom a)

class CanBeAnAlias a where
    isAlias :: a -> Maybe a

class VarSymBreakingDescription a where
    varSymBreakingDescription :: a -> JSON.Value

class (:<) a b where
    inject :: a -> b
    project :: MonadFail m => b -> m a

class SimpleJSON a where
    toSimpleJSON :: MonadUserError m => a -> m JSON.Value
    fromSimpleJSON :: MonadUserError m => JSON.Value -> m a

instance SimpleJSON Integer where
    toSimpleJSON = return . toJSON
    fromSimpleJSON x =
        case x of
            JSON.Number y ->
                case floatingOrInteger y of
                    Right z -> return z
                    Left (_ :: Double) -> noFromSimpleJSON
            _ -> noFromSimpleJSON

instance SimpleJSON x => SimpleJSON [x] where
    toSimpleJSON xs = do
        ys <- mapM toSimpleJSON xs
        return $ JSON.Array $ V.fromList ys
    fromSimpleJSON _ = noFromSimpleJSON

instance (SimpleJSON x, SimpleJSON y) => SimpleJSON (x,y) where
    toSimpleJSON (x,y) = do
        x' <- toSimpleJSON x
        y' <- toSimpleJSON y
        return $ JSON.Array $ V.fromList [x', y']
    fromSimpleJSON _ = noFromSimpleJSON


noToSimpleJSON :: (MonadUserError m, Pretty a) =>  a -> m b
noToSimpleJSON a = userErr1 $ vcat
    [ "Cannot convert the following to simple JSON:"
    , ""
    , pretty a
    , ""
    , "Let us know if you need support for this please!"
    , "As a workaround you can use --output-format=astjson"
    ]

noFromSimpleJSON :: MonadUserError m => m a
noFromSimpleJSON = userErr1 $ vcat
    [ "Cannot convert simple JSON to Essence yet."
    , "Let us know if you need support for this please!"
    , "As a workaround you can use --output-format=astjson"
    ]

