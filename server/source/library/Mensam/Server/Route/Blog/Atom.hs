module Mensam.Server.Route.Blog.Atom where

import Mensam.Application.Blog.Class
import Mensam.Application.Configured.Class
import Mensam.Configuration
import Mensam.Configuration.BaseUrl
import Mensam.Configuration.Blog
import Mensam.Configuration.Contact
import Mensam.Server.Route.Blog.Atom.Type
import Mensam.Server.Route.Blog.Type qualified
import Mensam.Server.Route.Type qualified

import Control.Monad.Logger.CallStack
import Data.List qualified as L
import Data.Map qualified as M
import Data.Ord
import Data.Text qualified as T
import Data.Time qualified as T
import Servant
import Text.Atom.Feed qualified as Atom
import Text.Feed.Export qualified as Feed
import Text.Feed.Types qualified as Feed

handler ::
  (MonadBlog m, MonadConfigured m, MonadLogger m) =>
  ServerT API m
handler = do
  blogs <- configBlogEntries <$> configuration
  atomMaxLength <- configAtomMaxLength <$> configuration
  let recentBlogs = recentBlogEntries atomMaxLength blogs
  let entryList = L.sortOn (Down . blogTimestamp . snd) $ M.toList (unBlogEntries recentBlogs)
  maybeEntries <- traverse (uncurry atomEntry) entryList
  case sequence @[] @Maybe maybeEntries of
    Nothing -> do
      logError "Failed to serialize atleast one Atom feed entry."
      respond $ WithStatus @500 NoContent
    Just entries -> do
      feed <- Feed.AtomFeed <$> atomFeed entries
      case Feed.textFeed feed of
        Nothing -> do
          logError "Failed to serialize Atom feed."
          respond $ WithStatus @500 NoContent
        Just text -> do
          logInfo "Serve Atom feed."
          respond . WithStatus @200 $ AtomFeed text

atomFeed ::
  MonadConfigured m =>
  [Atom.Entry] ->
  m Atom.Feed
atomFeed entries = do
  baseUrl <- configBaseUrl <$> configuration
  personName <- contactName . configContactInformation <$> configuration
  person <- atomPerson
  pure
    Atom.Feed
      { Atom.feedId =
          (displayBaseUrl baseUrl <>)
            . T.pack
            . show
            . linkURI
            . Mensam.Server.Route.Blog.Type.routeFeed
            $ Mensam.Server.Route.Type.routeBlog allFieldLinks
      , Atom.feedTitle = Atom.TextString $ personName <> "'s Blog"
      , Atom.feedUpdated = case entries of
          [] -> "1997-09-14T12:00:00+01:00" -- This is just a fallback in case there aren't any entries.
          Atom.Entry {Atom.entryUpdated} : _ -> entryUpdated -- Expects sorted entries.
      , Atom.feedAuthors = [person]
      , Atom.feedCategories = []
      , Atom.feedContributors = []
      , Atom.feedGenerator =
          Just
            Atom.Generator
              { Atom.genURI = Just "https://github.com/jumper149/homepage"
              , Atom.genVersion = Nothing
              , Atom.genText = personName <> "'s Homepage"
              }
      , Atom.feedIcon = Just $ displayBaseUrl baseUrl <> "favicon.png"
      , Atom.feedLinks =
          [ Atom.Link
              { Atom.linkHref =
                  (displayBaseUrl baseUrl <>)
                    . T.pack
                    . show
                    . linkURI
                    . Mensam.Server.Route.Blog.Type.routeFeed
                    $ Mensam.Server.Route.Type.routeBlog allFieldLinks
              , Atom.linkRel = Just $ Left "self"
              , Atom.linkType = Just "application/atom+xml"
              , Atom.linkHrefLang = Nothing
              , Atom.linkTitle = Nothing
              , Atom.linkLength = Nothing
              , Atom.linkAttrs = []
              , Atom.linkOther = []
              }
          , Atom.Link
              { Atom.linkHref =
                  (displayBaseUrl baseUrl <>)
                    . T.pack
                    . show
                    . linkURI
                    . Mensam.Server.Route.Blog.Type.routeOverview
                    $ Mensam.Server.Route.Type.routeBlog allFieldLinks
              , Atom.linkRel = Just $ Left "alternate"
              , Atom.linkType = Just "text/html"
              , Atom.linkHrefLang = Nothing
              , Atom.linkTitle = Nothing
              , Atom.linkLength = Nothing
              , Atom.linkAttrs = []
              , Atom.linkOther = []
              }
          ]
      , Atom.feedLogo = Nothing
      , Atom.feedRights = Just $ Atom.TextString "Attribution 4.0 International (CC BY 4.0)"
      , Atom.feedSubtitle = Nothing
      , Atom.feedEntries = entries
      , Atom.feedAttrs = []
      , Atom.feedOther = []
      }

atomEntry ::
  (MonadBlog m, MonadConfigured m, MonadLogger m) =>
  BlogId ->
  BlogEntry ->
  m (Maybe Atom.Entry)
atomEntry blogId BlogEntry {blogTitle, blogTimestamp} = do
  baseUrl <- configBaseUrl <$> configuration
  person <- atomPerson
  logInfo $ "Read blog article from file: " <> T.pack (show blogId)
  maybeContent <- readBlogEntryHtml blogId
  pure $ case maybeContent of
    Nothing -> Nothing
    Just content ->
      Just
        Atom.Entry
          { Atom.entryId =
              (displayBaseUrl baseUrl <>)
                . T.pack
                . show
                . linkURI
                $ Mensam.Server.Route.Blog.Type.routeArticle
                  (Mensam.Server.Route.Type.routeBlog allFieldLinks)
                  blogId
          , Atom.entryTitle = Atom.TextString blogTitle
          , Atom.entryUpdated = T.pack $ T.formatTime T.defaultTimeLocale "%0Y-%0m-%0dT12:00:00+01:00" blogTimestamp
          , Atom.entryAuthors = [person]
          , Atom.entryCategories = []
          , Atom.entryContent = Just $ Atom.HTMLContent content
          , Atom.entryContributor = []
          , Atom.entryLinks =
              [ Atom.Link
                  { Atom.linkHref =
                      (displayBaseUrl baseUrl <>)
                        . T.pack
                        . show
                        . linkURI
                        $ Mensam.Server.Route.Blog.Type.routeArticle
                          (Mensam.Server.Route.Type.routeBlog allFieldLinks)
                          blogId
                  , Atom.linkRel = Just $ Left "alternate"
                  , Atom.linkType = Just "text/html"
                  , Atom.linkHrefLang = Nothing
                  , Atom.linkTitle = Nothing
                  , Atom.linkLength = Nothing
                  , Atom.linkAttrs = []
                  , Atom.linkOther = []
                  }
              ]
          , Atom.entryPublished = Nothing
          , Atom.entryRights = Nothing
          , Atom.entrySource = Nothing
          , Atom.entrySummary = Nothing
          , Atom.entryInReplyTo = Nothing
          , Atom.entryInReplyTotal = Nothing
          , Atom.entryAttrs = []
          , Atom.entryOther = []
          }

atomPerson ::
  MonadConfigured m =>
  m Atom.Person
atomPerson = do
  baseUrl <- configBaseUrl <$> configuration
  personName <- contactName . configContactInformation <$> configuration
  maybePersonEmail <- contactEmailAddress . configContactInformation <$> configuration
  pure
    Atom.Person
      { Atom.personName = personName
      , Atom.personURI = Just $ displayBaseUrl baseUrl
      , Atom.personEmail = maybePersonEmail
      , Atom.personOther = []
      }
