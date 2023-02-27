module Homepage.Server.Html.Depth where

import Homepage.Configuration.BaseUrl

import Numeric.Natural
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

hrefWithDepth ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  AttributeValue ->
  Attribute
hrefWithDepth baseUrl depth ref = href $ withDepth baseUrl depth ref

withDepth ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  AttributeValue ->
  AttributeValue
withDepth baseUrl Nothing ref = textValue (displayBaseUrl baseUrl) <> ref
withDepth _ (Just 0) ref = "./" <> ref
withDepth _ (Just 1) ref = "../" <> ref
withDepth baseUrl (Just n) ref = withDepth baseUrl (Just $ pred n) $ "../" <> ref
