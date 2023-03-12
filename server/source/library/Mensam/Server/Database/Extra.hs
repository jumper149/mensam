module Mensam.Server.Database.Extra where

import Control.Monad.Catch
import Database.Selda

-- | Run 'query', but throw an error when there are multiple results.
queryUnique :: (MonadSelda m, MonadThrow m, Result a) => Query (Backend m) a -> m (Maybe (Res a))
queryUnique q =
  query q >>= \case
    _ : _ : _ -> throwM $ UnsafeError "Expected unique result, but the query returned multiple results."
    [spaceId] -> pure $ Just spaceId
    [] -> pure Nothing

-- | Run 'query', but throw an error unless there is exactly on result.
queryOne :: (MonadSelda m, MonadThrow m, Result a) => Query (Backend m) a -> m (Res a)
queryOne q =
  queryUnique q >>= \case
    Just spaceId -> pure spaceId
    Nothing -> throwM $ UnsafeError "Expected exactly one result, but the query didn't return any results."
