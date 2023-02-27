module Mensam.Server.Route.Home where

import Mensam.Application.Configured.Class
import Mensam.Configuration
import Mensam.Configuration.Blog
import Mensam.Configuration.Contact
import Mensam.Server.Html.Blog
import Mensam.Server.Html.Depth
import Mensam.Server.Html.Document
import Mensam.Server.Route.Blog.Type qualified
import Mensam.Server.Route.Donate.Type qualified
import Mensam.Server.Route.Files.Type qualified
import Mensam.Server.Route.Home.Type
import Mensam.Server.Route.Type qualified
import Mensam.Server.Tab

import Control.Monad.Logger.CallStack
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Servant
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html5.Extra

handler ::
  (MonadConfigured m, MonadLogger m) =>
  ServerT API m
handler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  blogs <- configBlogEntries <$> configuration
  blogPreviewMaxLength <- configBlogPreviewMaxLength <$> configuration
  logInfo "Serve main page."
  let description = (describeDocument contactInformation $ Just TabHome) {documentDepth = Just 0}
  pure . document baseUrl contactInformation revision description $ do
    img ! src "portrait.jpg" ! class_ "portrait" ! alt ("Portrait of " <> textValue (contactName contactInformation))
    h1 $ toMarkup $ contactName contactInformation

    h2 "Welcome"
    -- TODO: Configure author specific information.
    p $ do
      "I am living in Germany and working as a Software Engineer at "
      a ! href "https://www.possehl-analytics.com/" $ "Possehl Analytics"
      " in Augsburg."
    p $ do
      "I recently completed my Bachelor's degree in Physics with a minor in Computer Science, which I studied for at "
      a ! href "https://www.uni-hannover.de/en/" $ "Leibniz Universität Hannover"
      "."
    p . fold $
      L.intersperse
        " "
        [ "In my free time I like messing around with Linux and programming."
        , "I am spending most of my time with functional languages like Haskell."
        , "If I want to take a break from the computer I enjoy playing guitar or badminton."
        , "I also love green tea."
        ]

    h2 "recent Blog"
    p $ do
      "You can stay up to date by subscribing to this "
      a ! hrefWithDepth baseUrl (Just 0) (stringValue $ show $ linkURI blogFeedLink) $ s "RSS" <> "/Atom Feed"
      "."
    blogList baseUrl (Just 0) $ recentBlogEntries blogPreviewMaxLength blogs
    p $ do
      "The full list of blog articles can be accessed "
      a ! hrefWithDepth baseUrl (Just 0) (stringValue $ show $ linkURI blogOverviewLink) $ "here"
      "."

    h2 "shared Files"
    p $ do
      "You can download some of my shared files "
      a ! hrefWithDepth baseUrl (Just 0) (stringValue $ show $ linkURI filesLink) $ "here"
      "."

    h2 "Contact"
    contactHtml contactInformation

    h2 "Donate"
    p $ do
      "If you want to support me, you can donate to me "
      a ! hrefWithDepth baseUrl (Just 0) (stringValue $ show $ linkURI donateLink) $ "here"
      "."
 where
  blogFeedLink = Mensam.Server.Route.Blog.Type.routeFeed . Mensam.Server.Route.Type.routeBlog $ allFieldLinks
  blogOverviewLink = Mensam.Server.Route.Blog.Type.routeOverview . Mensam.Server.Route.Type.routeBlog $ allFieldLinks
  donateLink = Mensam.Server.Route.Donate.Type.routeDonate . Mensam.Server.Route.Type.routeDonate $ allFieldLinks
  filesLink = Mensam.Server.Route.Files.Type.routeOverview . Mensam.Server.Route.Type.routeFiles $ allFieldLinks

contactHtml :: ContactInformation -> Html
contactHtml
  ContactInformation
    { contactEmailAddress
    , contactMatrix
    , contactLiberaChat
    , contactGithubUsername
    , contactGitlabUsername
    , contactHackageUsername
    , contactAurUsername
    } = ul $ toMarkup $ li <$> contactItems
   where
    contactItems =
      catMaybes
        [ markupEmailAddress <$> contactEmailAddress
        , markupMatrix <$> contactMatrix
        , markupLiberaChat <$> contactLiberaChat
        , markupGitHub <$> contactGithubUsername
        , markupGitLab <$> contactGitlabUsername
        , markupHackage <$> contactHackageUsername
        , markupAUR <$> contactAurUsername
        ]
    markupEmailAddress emailAddress = do
      a ! href ("mailto:" <> textValue emailAddress) $ "E-Mail"
      ": " <> text emailAddress
    markupMatrix matrix = "Matrix: " <> text matrix
    markupLiberaChat liberaChat = "IRC (Libera Chat): " <> text liberaChat
    markupGitHub githubUsername = do
      a ! href ("https://github.com/" <> textValue githubUsername) $ "GitHub"
      ": " <> text githubUsername
    markupGitLab gitlabUsername = do
      a ! href ("https://gitlab.com/" <> textValue gitlabUsername) $ "GitLab"
      ": " <> text gitlabUsername
    markupHackage hackageUsername = do
      a ! href ("https://hackage.haskell.org/user/" <> textValue hackageUsername) $ "Hackage"
      ": " <> text hackageUsername
    markupAUR aurUsername = do
      a ! href ("https://aur.archlinux.org/packages/?K=" <> textValue aurUsername <> "&SeB=m") $ "AUR"
      ": " <> text aurUsername
