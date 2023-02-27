module Homepage.Server.Html.Document where

import Homepage.Configuration.BaseUrl
import Homepage.Configuration.Contact
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Header
import Homepage.Server.Tab

import Data.Kind
import Data.Maybe
import Data.Text qualified as T
import GHC.Generics
import Numeric.Natural
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

describeDocument :: ContactInformation -> Maybe Tab -> DocumentDescription
describeDocument contactInformation maybeTab =
  MkDocumentDescription
    { documentTitle =
        let titleName = maybe "Homepage" (tabPageName . describeTab) maybeTab
         in Just $ contactName contactInformation <> "'s " <> titleName
    , documentDescription = tabMetaDescription . describeTab =<< maybeTab
    , documentAuthor = Just $ contactName contactInformation
    , documentTab = maybeTab
    , documentDepth = Nothing
    }

type DocumentDescription :: Type
data DocumentDescription = MkDocumentDescription
  { documentTitle :: Maybe T.Text
  , documentDescription :: Maybe T.Text
  , documentAuthor :: Maybe T.Text
  , documentTab :: Maybe Tab
  , documentDepth :: Maybe Natural
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

document ::
  BaseUrl ->
  ContactInformation ->
  -- | revision
  Maybe T.Text ->
  DocumentDescription ->
  -- | body
  Html ->
  Html
document baseUrl contactInformation maybeRev description x =
  docTypeHtml ! lang "en" $ do
    H.head $ do
      meta ! charset "UTF-8"
      case documentAuthor description of
        Nothing -> pure ()
        Just author -> meta ! name "author" ! content (textValue author)
      case documentDescription description of
        Nothing -> pure ()
        Just descriptionText -> meta ! name "description" ! content (textValue descriptionText)
      meta ! name "viewport" ! content "width=500"
      case documentTitle description of
        Nothing -> pure ()
        Just titleText -> H.title $ toMarkup titleText
      link
        ! rel "icon"
        ! type_ "image/png"
        ! sizes "32x32"
        ! hrefWithDepth baseUrl (documentDepth description) "favicon.png"
      link
        ! rel "icon"
        ! type_ "image/png"
        ! sizes "192x192"
        ! hrefWithDepth baseUrl (documentDepth description) "favicon-192x192.png"
      link
        ! rel "icon"
        ! type_ "image/png"
        ! sizes "512x512"
        ! hrefWithDepth baseUrl (documentDepth description) "favicon-512x512.png"
      link
        ! rel "apple-touch-icon"
        ! type_ "image/png"
        ! sizes "512x512"
        ! hrefWithDepth baseUrl (documentDepth description) "favicon-512x512.png"
      link
        ! rel "stylesheet"
        ! type_ "text/css"
        ! hrefWithDepth baseUrl (documentDepth description) "stylesheet.css"
    body $ do
      headerTabs baseUrl contactInformation (documentDepth description) (documentTab description)
      x
      H.div ! class_ "footer" $ do
        let withHref = case contactSourceUrl contactInformation of
              Nothing -> Prelude.id @(Html -> Html)
              Just url -> (! href (textValue url))
        withHref a $ toMarkup $ "source @ " <> fromMaybe "unknown-revision" maybeRev
