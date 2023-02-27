{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Blog where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Configuration.Blog

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Identity
import Data.ByteString qualified as BS
import Data.Kind
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import UnliftIO qualified

type BlogT :: (Type -> Type) -> Type -> Type
newtype BlogT m a = BlogT {unBlogT :: IdentityT m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance (MonadConfigured m, MonadLogger m, MonadUnliftIO m) => MonadBlog (BlogT m) where
  readBlogEntryHtml blogId = do
    dir <- lift $ configDirectoryBlog <$> configuration
    let file = dir <> "/" <> T.unpack (unBlogId blogId) <> ".html"
    lift $
      UnliftIO.catchIO (liftIO $ Just . T.decodeUtf8 <$> BS.readFile file) $ \err -> do
        logError $ "Failed to read HTML for blog entry '" <> T.pack (show blogId) <> "' with '" <> T.pack (show err) <> "'."
        pure Nothing

deriving via
  BlogT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    (MonadConfigured (t2 m), MonadLogger (t2 m), MonadUnliftIO (t2 m)) => MonadBlog (ComposeT BlogT t2 m)

runBlogT ::
  BlogT m a ->
  m a
runBlogT = runIdentityT . unBlogT

runAppBlogT ::
  forall m a.
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  BlogT m a ->
  m a
runAppBlogT tma = runBlogT $ checkBlogEntries >> tma
 where
  checkBlogEntries = do
    lift $ logInfo "Probing configured blog entries."
    entries <- lift $ M.toList . unBlogEntries . configBlogEntries <$> configuration
    results <- traverse (uncurry checkBlogEntry) entries
    if and results
      then lift $ logInfo "Finished probing blog entries."
      else lift $ logError "Problems occured while checking blog entries."
   where
    checkBlogEntry :: BlogId -> BlogEntry -> BlogT m Bool
    checkBlogEntry blogId blogEntry = do
      lift $ logInfo $ "Checking blog entry '" <> T.pack (show (blogId, blogEntry)) <> "'."
      isJust <$> readBlogEntryHtml blogId
