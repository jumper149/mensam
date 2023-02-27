module Homepage.Server.FileServer where

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Data.Text qualified as T
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

fileServerSettings ::
  (MonadLogger m, MonadUnliftIO m) =>
  FilePath ->
  m StaticSettings
fileServerSettings path =
  withRunInIO $ \runInIO ->
    pure $ case defaultFileServerSettings path of
      defaultSettings@StaticSettings {ssLookupFile, ssGetMimeType} ->
        defaultSettings
          { ssLookupFile = \pieces -> do
              runInIO . logInfo $ "Looking up file: " <> T.pack (show pieces)
              ssLookupFile pieces
          , ssGetMimeType = \file -> do
              mimeType <- ssGetMimeType file
              runInIO . logInfo $ "Determined mime type: " <> T.pack (show mimeType)
              pure mimeType
          , ssAddTrailingSlash = True -- Disable directory overview without trailing slash, because some links are broken.
          }
