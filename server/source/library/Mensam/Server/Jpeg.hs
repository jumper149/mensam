module Mensam.Server.Jpeg where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Control.Monad.Trans.State
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import GHC.Generics
import Servant.API.ImageJpeg

type ByteStringJpeg :: Type
newtype ByteStringJpeg = MkByteStringJpegUnsafe {unByteStringJpeg :: BL.ByteString}
  deriving stock (Eq, Generic, Ord, Read, Show)

jpegConvertProfilePicture :: ImageJpegBytes -> Either String ByteStringJpeg
jpegConvertProfilePicture bytesIn = do
  dynamicImage <- decodeJpeg $ BL.toStrict $ unImageJpegBytes bytesIn
  let imageIn = convertRGB8 dynamicImage
  imageOut <- execStateT resizeProfilePicture imageIn
  let bytesOut = encodeJpeg $ convertImage imageOut
  pure $ MkByteStringJpegUnsafe bytesOut
 where
  resizeProfilePicture :: StateT (Image PixelRGB8) (Either String) ()
  resizeProfilePicture = do
    let targetSize :: Int = 640
    originalWidth <- gets imageWidth
    originalHeight <- gets imageHeight
    case compare originalWidth originalHeight of
      EQ -> pure ()
      LT -> do
        let croppedHeight = originalWidth
        let croppedHeightCutOff = (originalHeight - croppedHeight) `div` 2
        modify $ crop 0 croppedHeightCutOff originalWidth croppedHeight
      GT -> do
        let croppedWidth = originalHeight
        let croppedWidthCutOff = (originalWidth - croppedWidth) `div` 2
        modify $ crop croppedWidthCutOff 0 croppedWidth originalHeight
    modify $ scaleBilinear targetSize targetSize

jpegConvertSpacePicture :: ImageJpegBytes -> Either String ByteStringJpeg
jpegConvertSpacePicture = jpegConvertProfilePicture
