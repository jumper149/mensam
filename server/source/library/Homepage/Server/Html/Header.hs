module Homepage.Server.Html.Header where

import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Contact
import Homepage.Server.Html.Depth
import Homepage.Server.Tab

import Control.Monad
import Data.Foldable
import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes as HA

headerTab ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  Tab ->
  -- | active
  Bool ->
  Html
headerTab baseUrl depth tab active = a ! hrefWithDepth baseUrl depth (toValue tabPath) ! classActive $ toMarkup tabName
 where
  TabDescription {tabName, tabPath} = describeTab tab
  classActive =
    if active
      then class_ "active"
      else mempty

headerTabsHelper ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  Maybe Tab ->
  Html
headerTabsHelper baseUrl depth activeTab = traverse_ f [minBound .. maxBound]
 where
  f :: Tab -> Html
  f tab =
    let active = case activeTab of
          Just activeTab' -> tab == activeTab'
          _ -> False
     in headerTab baseUrl depth tab active

headerTabs ::
  BaseUrl ->
  ContactInformation ->
  -- | depth
  Maybe Natural ->
  Maybe Tab ->
  Html
headerTabs baseUrl ContactInformation {contactHeaderIcons, contactHomepageLabel, contactGithubUsername, contactGitlabUsername} depth activeTab =
  header $
    H.div ! class_ "bar" $ do
      headerTabsHelper baseUrl depth activeTab
      H.span $ do
        when (headerIconFeed contactHeaderIcons) $
          a ! hrefWithDepth baseUrl depth "blog/atom.xml" ! class_ "icon" $
            img ! src (withDepth baseUrl depth "icons/feed.png") ! HA.title "Feed" ! class_ "icon"
        when (headerIconGithub contactHeaderIcons) $
          case contactGithubUsername of
            Nothing -> mempty
            Just githubUsername ->
              a ! href ("https://github.com/" <> textValue githubUsername) ! class_ "icon" $
                img ! src (withDepth baseUrl depth "icons/GitHub.png") ! HA.title "GitHub" ! class_ "icon"
        when (headerIconGitlab contactHeaderIcons) $
          case contactGitlabUsername of
            Nothing -> mempty
            Just gitlabUsername ->
              a ! href ("https://gitlab.com/" <> textValue gitlabUsername) ! class_ "icon" $
                img ! src (withDepth baseUrl depth "icons/GitLab.png") ! HA.title "GitLab" ! class_ "icon"
        let maybeHomepageLabel = case contactHomepageLabel of
              Nothing -> baseUrlAuthorityHost <$> baseUrlAuthority baseUrl
              Just _ -> contactHomepageLabel
        case maybeHomepageLabel of
          Nothing -> mempty
          Just homepageLabel -> a ! href (textValue $ displayBaseUrl baseUrl) $ toMarkup homepageLabel
