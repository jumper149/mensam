module Mensam.Server.Jpeg where

import Data.ByteString.Lazy qualified as BL
import Data.Kind
import GHC.Generics
import Servant.API.ImageJpeg

type ByteStringJpeg :: Type
newtype ByteStringJpeg = MkByteStringJpegUnsafe {unByteStringJpeg :: BL.ByteString}
  deriving stock (Eq, Generic, Ord, Read, Show)

jpegConvert :: ImageJpegBytes -> ByteStringJpeg
jpegConvert = MkByteStringJpegUnsafe . unImageJpegBytes
