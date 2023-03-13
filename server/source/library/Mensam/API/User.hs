module Mensam.API.User where

import Mensam.API.Aeson
import Mensam.API.User.Username

import Data.Aeson qualified as A
import Data.Attoparsec.ByteString qualified as Attoparsec.B
import Data.Int
import Data.Kind
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Text.Email.Parser

type User :: Type
data User = MkUser
  { userId :: IdentifierUser
  , userName :: Username
  , userEmail :: EmailAddress
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "user") User

type IdentifierUser :: Type
newtype IdentifierUser = MkIdentifierUser {unIdentifierUser :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

-- TODO
instance A.FromJSON EmailAddress where
  parseJSON value = do
    text <- A.parseJSON @T.Text value
    case fromText text of
      Left err -> fail err
      Right emailAddress -> pure emailAddress

instance A.ToJSON EmailAddress where
  toJSON = A.String . toText

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
