{-# LANGUAGE MultiWayIf #-}

module Mensam.API.Data.User.Password where

import Control.Applicative
import Data.Aeson qualified as A
import Data.Attoparsec.Combinator qualified as P
import Data.Attoparsec.Text qualified as P
import Data.Kind
import Data.Text qualified as T
import GHC.Generics
import Servant.API

type Password :: Type
newtype Password = MkPasswordUnsafe {unPassword :: T.Text}
  deriving stock (Eq, Generic, Ord)

deriving newtype instance Show Password
instance Read Password where
  readsPrec p string = do
    (passwordText, rest) <- readsPrec @T.Text p string
    case mkPassword passwordText of
      Left err -> fail err
      Right password -> pure (password, rest)

deriving newtype instance A.ToJSON Password
instance A.FromJSON Password where
  parseJSON value = do
    text <- A.parseJSON @T.Text value
    case mkPassword text of
      Left err -> fail err
      Right password -> pure password

mkPassword :: T.Text -> Either String Password
mkPassword = P.parseOnly $ do
  let
    symbol = P.choice $ P.char <$> passwordValidSymbols
    alphanumeric = (P.digit <|> P.letter <|> symbol) P.<?> "unexpected non-alphanumeric and non-symbol character"
  chars <- P.manyTill alphanumeric P.endOfInput
  if
    | length chars > 32 -> fail "too long"
    | length chars < 4 -> fail "too short"
    | otherwise -> pure $ MkPasswordUnsafe $ T.pack chars

deriving newtype instance ToHttpApiData Password
instance FromHttpApiData Password where
  parseUrlPiece input = do
    text <- parseUrlPiece @T.Text input
    case mkPassword text of
      Left err -> Left $ T.pack err
      Right parsed -> Right parsed

passwordValidSymbols :: [Char]
passwordValidSymbols =
  [ ' '
  , '~'
  , '`'
  , '!'
  , '?'
  , '@'
  , '#'
  , '$'
  , '%'
  , '^'
  , '&'
  , '*'
  , '_'
  , '-'
  , '+'
  , '='
  , '<'
  , '>'
  , '('
  , ')'
  , '{'
  , '}'
  , '['
  , ']'
  , '|'
  , '\''
  , '"'
  , ','
  , '.'
  , ':'
  , ';'
  , '/'
  , '\\'
  ]
