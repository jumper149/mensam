{-# LANGUAGE MultiWayIf #-}

module Mensam.API.Data.User.Username where

import Control.Applicative
import Data.Aeson qualified as A
import Data.Attoparsec.Combinator qualified as P
import Data.Attoparsec.Text qualified as P
import Data.Kind
import Data.Text qualified as T
import GHC.Generics
import Servant.API

type Username :: Type
newtype Username = MkUsernameUnsafe {unUsername :: T.Text}
  deriving stock (Eq, Generic, Ord)

deriving newtype instance Show Username
instance Read Username where
  readsPrec p string = do
    (usernameText, rest) <- readsPrec @T.Text p string
    case mkUsername usernameText of
      Left err -> fail err
      Right username -> pure (username, rest)

deriving newtype instance A.ToJSON Username
instance A.FromJSON Username where
  parseJSON value = do
    text <- A.parseJSON @T.Text value
    case mkUsername text of
      Left err -> fail err
      Right username -> pure username

mkUsername :: T.Text -> Either String Username
mkUsername = P.parseOnly $ do
  let alphanumeric = (P.digit <|> P.letter) P.<?> "unexpected non-alphanumeric character"
  chars <- P.manyTill alphanumeric P.endOfInput
  if
      | length chars > 32 -> fail "too long"
      | length chars < 4 -> fail "too short"
      | otherwise -> pure $ MkUsernameUnsafe $ T.pack chars

deriving newtype instance ToHttpApiData Username
instance FromHttpApiData Username where
  parseUrlPiece input = do
    text <- parseUrlPiece @T.Text input
    case mkUsername text of
      Left err -> Left $ T.pack err
      Right parsed -> Right parsed
