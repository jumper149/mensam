module Mensam.Server.Route.Home where

import Mensam.Application.Configured.Class
import Mensam.Configuration
import Mensam.Configuration.Contact
import Mensam.Server.Html.Document
import Mensam.Server.Route.Home.Type
import Mensam.Server.Tab

import Control.Monad.Logger.CallStack
import Data.Foldable
import Data.List qualified as L
import Data.Maybe
import Servant
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

handler ::
  (MonadConfigured m, MonadLogger m) =>
  ServerT API m
handler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
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

    h2 "Contact"
    contactHtml contactInformation

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
