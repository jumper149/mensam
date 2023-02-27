module Mensam.Server.Route.Files where

import Mensam.Application.Configured.Class
import Mensam.Configuration
import Mensam.Configuration.Files
import Mensam.Server.Err404
import Mensam.Server.FileServer
import Mensam.Server.Html.Depth
import Mensam.Server.Html.Document
import Mensam.Server.Route.Files.Type
import Mensam.Server.Route.Type qualified
import Mensam.Server.Tab

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Data.Foldable
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Calendar
import Network.Wai.Trans
import Servant
import Servant.RawM.Server qualified as RawM
import Servant.Server.Generic
import Text.Blaze.Html5
import WaiAppStatic.Types

routes ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeOverview = overviewHandler
    , routeFiles = filesHandler
    }

overviewHandler ::
  (MonadConfigured m, MonadLogger m) =>
  m Html
overviewHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  fileEntries <- configFileEntries <$> configuration
  logInfo "Serve files overview."
  let description = (describeDocument contactInformation $ Just TabFiles) {documentDepth = Just 0}
  pure . document baseUrl contactInformation revision description $ do
    h2 "my Files"
    let
      entryGroups = groupFiles $ unFileEntries fileEntries
      topLevelEntries = fromMaybe mempty $ M.lookup Nothing entryGroups
      otherEntries = M.mapKeys fromJust . M.delete Nothing $ entryGroups
      markupEntries entrySet = ul $ toMarkup $ fileToMarkup <$> L.sortOn (Down . fileTimestamp) (S.toList entrySet)
       where
        fileToMarkup file@FileEntry {Mensam.Configuration.Files.fileName, fileTimestamp} =
          li $ do
            toMarkup $ T.pack (showGregorian fileTimestamp)
            " - "
            toMarkup fileName
            " [ "
            toMarkup $ L.intersperse " | " $ fileFormat <$> fileFormats file
            " ]"
         where
          fileFormat FileFormat {fileFormatName, fileFormatExtension} =
            a ! hrefWithDepth baseUrl (Just 0) (textValue $ T.pack (show $ linkURI filesRawLink) <> fileAddress) $
              toMarkup fileFormatName
           where
            filesRawLink = routeFiles . Mensam.Server.Route.Type.routeFiles $ allFieldLinks
            fileAddress = "/" <> fileIdentifier file <> maybe "" ("." <>) fileFormatExtension
    markupEntries topLevelEntries
    let markupSection (sectionName, entrySet) = do
          h2 $ text $ "my " <> sectionName
          markupEntries entrySet
    traverse_ markupSection $ M.toList otherEntries

filesHandler ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  ServerT RawM.RawM m
filesHandler = do
  directory <- configDirectoryFiles <$> configuration
  fallbackApplication <- runApplicationT application404
  logInfo "Serve file download."
  settings <- fileServerSettings directory
  RawM.serveDirectoryWith settings {ss404Handler = Just fallbackApplication}
