module Mensam.Server.Database.Extra where

import Mensam.API.Order

import Control.Monad.Catch
import Data.Kind
import Data.Maybe
import Data.Proxy
import Data.Text qualified as T
import Data.Typeable
import Database.Selda
import GHC.TypeLits

-- | Run 'query', but throw an error when there are multiple results.
queryUnique :: (MonadSelda m, MonadThrow m, Result a) => Query (Backend m) a -> m (Maybe (Res a))
queryUnique q =
  query q >>= \case
    _ : _ : _ -> throwM $ UnsafeError "Expected unique result, but the query returned multiple results."
    [spaceId] -> pure $ Just spaceId
    [] -> pure Nothing

-- | Run 'query', but throw an error unless there is exactly one result.
queryOne :: (MonadSelda m, MonadThrow m, Result a) => Query (Backend m) a -> m (Res a)
queryOne q =
  queryUnique q >>= \case
    Just spaceId -> pure spaceId
    Nothing -> throwM $ UnsafeError "Expected exactly one result, but the query didn't return any results."

-- | Run 'insert', but throw an error unless exactly one row is being inserted.
insertOne :: (MonadSelda m, MonadThrow m, Relational a) => Table a -> a -> m ()
insertOne t a =
  insert t [a] >>= \case
    1 -> pure ()
    0 -> throwM $ UnsafeError "Tried to insert exactly one row, but no rows have been inserted at all."
    n -> throwM $ UnsafeError $ "Tried to insert exactly one row, but multiple rows have been inserted: " <> show n

-- | Run 'deleteFrom', but throw an error when multiple rows have been deleted.
deleteUniqueFrom :: (MonadSelda m, MonadThrow m, Relational a) => Table a -> (Row (Backend m) a -> Col (Backend m) Bool) -> m Bool
deleteUniqueFrom t f = do
  deleteFrom t f >>= \case
    1 -> pure True
    0 -> pure False
    n -> throwM $ UnsafeError $ "Expected to delete unique row, but the constraint matches multiple rows: " <> show n

-- | Run 'deleteFrom', but throw an error unless exactly one row has been deleted.
deleteOneFrom :: (MonadSelda m, MonadThrow m, Relational a) => Table a -> (Row (Backend m) a -> Col (Backend m) Bool) -> m ()
deleteOneFrom t f = do
  deleteUniqueFrom t f >>= \case
    True -> pure ()
    False -> throwM $ UnsafeError "Expected to delete exactly one row, but the constraint didn't match any rows."

type SomeCol :: Type -> Type
data SomeCol t = forall a. SqlType a => MkSomeCol (Col t a)

orderFlexible ::
  Same s t =>
  (c -> SomeCol s) ->
  OrderByCategories c ->
  Query t ()
orderFlexible _ (MkOrderByCategories []) = pure ()
orderFlexible categoryToCol (MkOrderByCategories (c : cs)) =
  case categoryToCol (orderByCategoryCategory c) of
    MkSomeCol col -> do
      order col (translateOrder $ orderByCategoryOrder c)
      orderFlexible categoryToCol (MkOrderByCategories cs)
 where
  translateOrder :: Mensam.API.Order.Order -> Database.Selda.Order
  translateOrder = \case
    Ascending -> ascending
    Descending -> descending

type SqlEnumStripPrefix :: Symbol -> Type -> Type
newtype SqlEnumStripPrefix prefix a = MkSqlEnumStripPrefix {unSqlEnumStripPrefix :: a}
  deriving newtype (Bounded, Enum)

instance (Typeable a, Bounded a, Enum a, Show a, Read a, KnownSymbol prefix) => SqlEnum (SqlEnumStripPrefix prefix a) where
  toText = fromJust . T.stripPrefix (T.pack (symbolVal $ Proxy @prefix)) . (T.pack . show @a) . unSqlEnumStripPrefix
  fromText = MkSqlEnumStripPrefix . (read @a . T.unpack) . (T.pack (symbolVal $ Proxy @prefix) <>)
