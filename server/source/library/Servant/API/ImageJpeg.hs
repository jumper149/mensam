module Servant.API.ImageJpeg where

import Codec.Picture.Jpg
import Control.Lens
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.OpenApi
import Data.Proxy
import GHC.Generics
import Network.HTTP.Media qualified
import Servant.API

type ImageJpegBytes :: Type
newtype ImageJpegBytes = MkImageJpegBytes {unImageJpegBytes :: BL.ByteString}
  deriving stock (Eq, Generic, Ord, Read, Show)

instance ToParamSchema ImageJpegBytes where
  toParamSchema Proxy =
    mempty
      & type_ ?~ OpenApiString
      & format ?~ "binary"
instance ToSchema ImageJpegBytes where
  declareNamedSchema = pure . NamedSchema (Just "ImageJpegBytes") . paramSchemaToSchema

type ImageJpeg :: Type
data ImageJpeg

instance Accept ImageJpeg where
  contentType Proxy = "image" Network.HTTP.Media.// "jpeg"

instance MimeRender ImageJpeg ImageJpegBytes where
  mimeRender Proxy = unImageJpegBytes

instance MimeUnrender ImageJpeg ImageJpegBytes where
  mimeUnrender Proxy bytes =
    case decodeJpeg $ BL.toStrict bytes of
      Left err -> Left err
      Right _ -> Right $ MkImageJpegBytes bytes
