module Homepage.Application.Blog.Class where

import Homepage.Configuration.Blog

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Data.Text qualified as T

type MonadBlog :: (Type -> Type) -> Constraint
class Monad m => MonadBlog m where
  readBlogEntryHtml :: BlogId -> m (Maybe T.Text)

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadBlog m
  ) =>
  MonadBlog (Elevator t m)
  where
  readBlogEntryHtml = lift . readBlogEntryHtml

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadBlog (t2 m)
    ) =>
    MonadBlog (ComposeT t1 t2 m)
