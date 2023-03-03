{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Mensam.User where

import Mensam.Application.SeldaConnection.Class
import Mensam.Database

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Data.Aeson qualified as A
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
  { userName :: T.Text
  , userEmail :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)
  deriving anyclass (FromJWT, ToJWT)

instance FromBasicAuthData User where
  fromBasicAuthData BasicAuthData {basicAuthUsername, basicAuthPassword} MkRunLoginInIO {runLoginInIO} = runLoginInIO $ do
    logInfo "Starting password authentication."
    logDebug $ "Decoding UTF-8 username: " <> T.pack (show basicAuthPassword)
    case T.decodeUtf8' basicAuthUsername of
      Left err -> do
        logInfo $ "Failed to decode username as UTF-8: " <> T.pack (show err)
        pure Indefinite
      Right username -> do
        logDebug $ "Decoded UTF-8 username: " <> T.pack (show username)
        logDebug "Decoding UTF-8 password."
        case mkPassword <$> T.decodeUtf8' basicAuthUsername of
          Left _err -> do
            logInfo "Failed to decode password as UTF-8."
            pure Indefinite
          Right password -> do
            logDebug "Decoded UTF-8 password."
            userLogin username password

userLogin ::
  (MonadLogger m, MonadSeldaConnection m) =>
  -- | username
  T.Text ->
  Password ->
  m (AuthResult User)
userLogin username password = do
  logDebug $ "Querying user " <> T.pack (show username) <> "from database for password authentication."
  matchingUsers :: [DbUser] <- runSeldaTransaction $ Selda.query $ do
    user <- Selda.select usersTable
    Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal username
    return user
  case matchingUsers of
    [] -> pure NoSuchUser
    [dbUser] -> do
      let
        user =
          MkUser
            { userName = dbUser_name dbUser
            , userEmail = dbUser_email dbUser
            }
        passwordHash = PasswordHash $ dbUser_password_hash dbUser
      logDebug $ "Queried user " <> T.pack (show user) <> " from database for password authentication. Checking password now."
      case checkPassword password passwordHash of
        PasswordCheckFail -> do
          logInfo "Password authentication failed because of wrong password."
          pure BadPassword
        PasswordCheckSuccess -> do
          logInfo "Password authentication succeeded."
          pure $ Authenticated user
    dbUsers@(_ : _) -> do
      logError $ "Multiple matching users have been found in the database. This should be impossible: " <> T.pack (show dbUsers)
      pure Indefinite

userCreate :: (MonadIO m, MonadLogger m, MonadSeldaConnection m) => User -> Password -> m ()
userCreate user@MkUser {userName, userEmail} password = do
  logDebug $ "Creating new user: " <> T.pack (show user)
  passwordHash :: PasswordHash Bcrypt <- hashPassword password
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = userName
          , dbUser_password_hash = unPasswordHash passwordHash
          , dbUser_email = userEmail
          }
  logDebug "Inserting new user into database."
  runSeldaTransaction $
    Selda.insert_ usersTable [dbUser]
  logInfo "Created new user successfully."
  pure ()

type instance BasicAuthCfg = RunLoginInIO

type RunLoginInIO :: Type
data RunLoginInIO = forall m.
  (MonadLogger m, MonadSeldaConnection m) =>
  MkRunLoginInIO {runLoginInIO :: m (AuthResult User) -> IO (AuthResult User)}
