{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Mensam.User where

import Mensam.Application.SeldaPool.Class
import Mensam.Database
import Mensam.Database.Extra qualified as Selda

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.Selda qualified as Selda
import GHC.Generics
import Servant.Auth.JWT
import Servant.Auth.Server

type User :: Type
data User = MkUser
  { userId :: UserId
  , userName :: T.Text
  , userEmail :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)
  deriving anyclass (FromJWT, ToJWT)

type UserId :: Type
newtype UserId = MkUserId {unUserId :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

instance FromBasicAuthData User where
  fromBasicAuthData BasicAuthData {basicAuthUsername, basicAuthPassword} MkRunLoginInIO {runLoginInIO} = runLoginInIO $ do
    logInfo "Starting password authentication."
    logDebug $ "Decoding UTF-8 username: " <> T.pack (show basicAuthUsername)
    case T.decodeUtf8' basicAuthUsername of
      Left err -> do
        logInfo $ "Failed to decode username as UTF-8: " <> T.pack (show err)
        pure Indefinite
      Right username -> do
        logDebug $ "Decoded UTF-8 username: " <> T.pack (show username)
        logDebug "Decoding UTF-8 password."
        case mkPassword <$> T.decodeUtf8' basicAuthPassword of
          Left _err -> do
            logInfo "Failed to decode password as UTF-8."
            pure Indefinite
          Right password -> do
            logDebug "Decoded UTF-8 password."
            runSeldaTransactionT $ userAuthenticate username password

userAuthenticate ::
  (MonadLogger m, MonadSeldaPool m) =>
  -- | username
  T.Text ->
  Password ->
  SeldaTransactionT m (AuthResult User)
userAuthenticate username password = do
  lift $ logDebug $ "Querying user " <> T.pack (show username) <> " from database for password authentication."
  maybeUser :: Maybe DbUser <- Selda.queryUnique $ do
    user <- Selda.select tableUser
    Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal username
    return user
  case maybeUser of
    Nothing -> pure NoSuchUser
    Just dbUser -> do
      let
        user =
          MkUser
            { userId = MkUserId {unUserId = Selda.fromId $ dbUser_id dbUser}
            , userName = dbUser_name dbUser
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

userCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | name
  T.Text ->
  -- | email
  T.Text ->
  -- | password
  Password ->
  SeldaTransactionT m ()
userCreate name email password = do
  lift $ logDebug "Creating user."
  passwordHash :: PasswordHash Bcrypt <- hashPassword password
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = name
          , dbUser_password_hash = unPasswordHash passwordHash
          , dbUser_email = email
          }
  lift $ logDebug "Inserting user into database."
  Selda.insert_ tableUser [dbUser]
  lift $ logInfo "Created user successfully."

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
