{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Mensam.User where

import Mensam.Aeson
import Mensam.Application.SeldaPool.Class
import Mensam.Database
import Mensam.Database.Extra qualified as Selda

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Aeson qualified as A
import Data.Attoparsec.Combinator qualified as P
import Data.Attoparsec.Text qualified as P
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.Selda qualified as Selda
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API
import Servant.Auth.JWT
import Servant.Auth.Server

type User :: Type
data User = MkUser
  { userId :: Selda.Identifier DbUser
  , userName :: Username
  , userEmail :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "user") User
  deriving anyclass (FromJWT, ToJWT)

instance FromBasicAuthData User where
  fromBasicAuthData BasicAuthData {basicAuthUsername, basicAuthPassword} MkRunLoginInIO {runLoginInIO} = runLoginInIO $ do
    logInfo "Starting password authentication."
    logDebug $ "Decoding UTF-8 username: " <> T.pack (show basicAuthUsername)
    case T.decodeUtf8' basicAuthUsername of
      Left err -> do
        logInfo $ "Failed to decode username as UTF-8: " <> T.pack (show err)
        pure NoSuchUser
      Right usernameText -> do
        logDebug $ "Decoded UTF-8 username: " <> T.pack (show usernameText)
        logDebug $ "Parsing username: " <> T.pack (show usernameText)
        case mkUsername usernameText of
          Left err -> do
            logInfo $ "Failed parse username: " <> T.pack (show err)
            pure NoSuchUser
          Right username -> do
            logDebug $ "Parsed username: " <> T.pack (show username)
            logDebug "Decoding UTF-8 password."
            case mkPassword <$> T.decodeUtf8' basicAuthPassword of
              Left _err -> do
                logInfo "Failed to decode password as UTF-8."
                pure BadPassword
              Right password -> do
                logDebug "Decoded UTF-8 password."
                runSeldaTransactionT $ userAuthenticate username password

userAuthenticate ::
  (MonadLogger m, MonadSeldaPool m) =>
  Username ->
  Password ->
  SeldaTransactionT m (AuthResult User)
userAuthenticate username password = do
  lift $ logDebug $ "Querying user " <> T.pack (show username) <> " from database for password authentication."
  maybeUser :: Maybe DbUser <- Selda.queryUnique $ do
    user <- Selda.select tableUser
    Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal (unUsername username)
    return user
  case maybeUser of
    Nothing -> pure NoSuchUser
    Just dbUser -> do
      let
        user =
          MkUser
            { userId = Selda.toIdentifier $ dbUser_id dbUser
            , userName = username
            , userEmail = dbUser_email dbUser
            }
        passwordHash = PasswordHash $ dbUser_password_hash dbUser
      lift $ logDebug $ "Queried user " <> T.pack (show user) <> " from database for password authentication. Checking password now."
      case checkPassword password passwordHash of
        PasswordCheckFail -> do
          lift $ logInfo "Password authentication failed because of wrong password."
          pure BadPassword
        PasswordCheckSuccess -> do
          lift $ logInfo "Password authentication succeeded."
          pure $ Authenticated user

userLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Username ->
  SeldaTransactionT m (Maybe (Selda.Identifier DbUser))
userLookupId username = do
  lift $ logDebug $ "Looking up user identifier with name: " <> T.pack (show username)
  maybeDbId <- Selda.queryUnique $ do
    dbUser <- Selda.select tableUser
    Selda.restrict $ dbUser Selda.! #dbUser_name Selda..== Selda.literal (unUsername username)
    pure $ dbUser Selda.! #dbUser_id
  case maybeDbId of
    Nothing -> do
      lift $ logWarn $ "Failed to look up user. Name doesn't exist: " <> T.pack (show username)
      pure Nothing
    Just dbId -> do
      lift $ logInfo "Looked up user successfully."
      pure $ Just $ Selda.toIdentifier dbId

userGet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Selda.Identifier DbUser ->
  SeldaTransactionT m User
userGet identifier = do
  lift $ logDebug $ "Get user info with identifier: " <> T.pack (show identifier)
  dbUser <- Selda.queryOne $ do
    dbUser <- Selda.select tableUser
    Selda.restrict $ dbUser Selda.! #dbUser_id Selda..== Selda.literal (Selda.fromIdentifier identifier)
    pure dbUser
  lift $ logInfo "Got user info successfully."
  pure
    MkUser
      { userId = Selda.toIdentifier $ dbUser_id dbUser
      , userName = MkUsernameUnsafe $ dbUser_name dbUser
      , userEmail = dbUser_email dbUser
      }

userCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Username ->
  -- | email
  T.Text ->
  Password ->
  SeldaTransactionT m ()
userCreate username email password = do
  lift $ logDebug "Creating user."
  passwordHash :: PasswordHash Bcrypt <- hashPassword password
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = unUsername username
          , dbUser_password_hash = unPasswordHash passwordHash
          , dbUser_email = email
          }
  lift $ logDebug "Inserting user into database."
  Selda.insert_ tableUser [dbUser]
  lift $ logInfo "Created user successfully."

userProfile ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Username ->
  SeldaTransactionT m (Maybe (Selda.Identifier DbUser))
userProfile username = do
  lift $ logDebug $ "Looking up user identifier with name: " <> T.pack (show username)
  maybeDbId <- Selda.queryUnique $ do
    dbUser <- Selda.select tableUser
    Selda.restrict $ dbUser Selda.! #dbUser_name Selda..== Selda.literal (unUsername username)
    pure $ dbUser Selda.! #dbUser_id
  case maybeDbId of
    Nothing -> do
      lift $ logWarn $ "Failed to look up user. Name doesn't exist: " <> T.pack (show username)
      pure Nothing
    Just dbId -> do
      lift $ logInfo "Looked up user successfully."
      pure $ Just $ Selda.toIdentifier dbId

-- * Soon to be moved away from this module

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

type instance BasicAuthCfg = RunLoginInIO

type RunLoginInIO :: Type
data RunLoginInIO = forall m.
  (MonadLogger m, MonadSeldaPool m) =>
  MkRunLoginInIO {runLoginInIO :: m (AuthResult User) -> IO (AuthResult User)}

-- TODO: Remove hardcoded JWK from source code.
jwtSettings :: JWTSettings
jwtSettings = defaultJWTSettings $ fromSecret "\155\EM\235\181/\128\236M\130\245\173\142\231<\241A\159Ds\238S|m\219\234\188\224\162\n(\212\148\212\147\180E>\138\141\"\161\US\154\173\212Y\FS\213\206\ENQ\196^\172\220\142\167r(\172\r\217\143~\234\179\244\204n\\2\149\n\195\NAKEh\176\130\bUt\141A_BE\146\211\247P\251\&5\231\DC2b\131=\b\GS\211\FS_\138z?A 6\178v\213\191\218\&7\ETB\161N\185\157\229\DC1m]\144\203\155\234\136\FS\148Q\228^\247\140\150g?_\ETX\164f\163\140\170\233{\153\189\248\r)\170'V'\RS\206q\f \222;8>\\\244\166\f\168\209B-\244\241\ACKZs\134f\225\&4\DLE\151\230\215\145]\160Z\t\ACKk\US'\174p\221\&9\ETB\214mV\140\STX\253P\179<BV^Ssp\163\&3\173>P\245T\160\\cr\194\ETX\178.D\253\233'\170y\148\250\232z\229\SYNK\212)>T\NUL2I\165\175\164\170\143\RS"

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings
