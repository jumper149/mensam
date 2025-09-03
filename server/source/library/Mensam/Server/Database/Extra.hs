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
    _ : _ : _ -> throwM MkSqlErrorMensamNotUniqueQuery
    [result] -> pure $ Just result
    [] -> pure Nothing

-- | Expected unique result, but the query returned multiple results.
type SqlErrorMensamNotUniqueQuery :: Type
data SqlErrorMensamNotUniqueQuery = MkSqlErrorMensamNotUniqueQuery
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

-- | Run 'query', but throw an error unless there is exactly one result.
queryOne :: (MonadSelda m, MonadCatch m, Result a) => Query (Backend m) a -> m (Res a)
queryOne q = do
  queryUniqueResult <- catch (queryUnique q) $ \case
    MkSqlErrorMensamNotUniqueQuery -> throwM $ MkSqlErrorMensamNotOneQuery $ Just MkSqlErrorMensamNotUniqueQuery
  case queryUniqueResult of
    Just result -> pure result
    Nothing -> throwM $ MkSqlErrorMensamNotOneQuery Nothing

-- | Expected exactly one result, but the query didn't return a lone result.
type SqlErrorMensamNotOneQuery :: Type
newtype SqlErrorMensamNotOneQuery = MkSqlErrorMensamNotOneQuery (Maybe SqlErrorMensamNotUniqueQuery)
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

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

-- | Run 'update', but throw an error when multiple rows are affected.
updateUnique :: (MonadSelda m, MonadThrow m, Relational a) => Table a -> (Row (Backend m) a -> Col (Backend m) Bool) -> (Row (Backend m) a -> Row (Backend m) a) -> m ()
updateUnique t p u =
  update t p u >>= \case
    0 -> pure ()
    1 -> pure ()
    _ -> throwM MkSqlErrorMensamNotUniqueUpdate

-- | Expected to update a single row, but the update matched multiple rows.
type SqlErrorMensamNotUniqueUpdate :: Type
data SqlErrorMensamNotUniqueUpdate = MkSqlErrorMensamNotUniqueUpdate
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

-- | Run 'update', but throw an error unless exactly one row is affected.
updateOne :: (MonadSelda m, MonadThrow m, Relational a) => Table a -> (Row (Backend m) a -> Col (Backend m) Bool) -> (Row (Backend m) a -> Row (Backend m) a) -> m ()
updateOne t p u = do
  update t p u >>= \case
    0 -> throwM $ MkSqlErrorMensamNotOneUpdate Nothing
    1 -> pure ()
    _ -> throwM $ MkSqlErrorMensamNotOneUpdate $ Just MkSqlErrorMensamNotUniqueUpdate

-- | Expected to update exactly one row, but the update did not match exactly one row.
type SqlErrorMensamNotOneUpdate :: Type
newtype SqlErrorMensamNotOneUpdate = MkSqlErrorMensamNotOneUpdate (Maybe SqlErrorMensamNotUniqueUpdate)
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

type SomeCol :: Type -> Type
type role SomeCol _
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
type role SqlEnumStripPrefix nominal _
newtype SqlEnumStripPrefix prefix a = MkSqlEnumStripPrefix {unSqlEnumStripPrefix :: a}
  deriving newtype (Bounded, Enum)

instance (Typeable a, Bounded a, Enum a, Show a, Read a, KnownSymbol prefix) => SqlEnum (SqlEnumStripPrefix prefix a) where
  toText = fromJust . T.stripPrefix (T.pack (symbolVal $ Proxy @prefix)) . (T.pack . show @a) . unSqlEnumStripPrefix
  fromText = MkSqlEnumStripPrefix . (read @a . T.unpack) . (T.pack (symbolVal $ Proxy @prefix) <>)
