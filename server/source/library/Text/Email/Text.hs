module Text.Email.Text where

import Data.Attoparsec.ByteString qualified as Attoparsec.B
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Text.Email.Parser

toText :: EmailAddress -> T.Text
toText emailAddress =
  case T.decodeUtf8' $ toByteString emailAddress of
    Left err -> error $ "Failed to decode EmailAddress: " <> show err
    Right text -> text

fromText :: T.Text -> Either String EmailAddress
fromText text = Attoparsec.B.parseOnly (addrSpec <* Attoparsec.B.endOfInput) $ T.encodeUtf8 text

fromTextUnsafe :: T.Text -> EmailAddress
fromTextUnsafe text =
  case fromText text of
    Left err -> error $ "Failed to parse EmailAddress: " <> show err
    Right emailAddress -> emailAddress
