{-# LANGUAGE OverloadedLabels #-}

module Mensam.User where

import Mensam.Aeson
import Mensam.Application.SeldaPool.Class
import Mensam.Database.Extra qualified as Selda
import Mensam.Database.Schema
import Mensam.User.Username

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Aeson qualified as A
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Database.Selda qualified as Selda
import Deriving.Aeson qualified as A
import GHC.Generics

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

type AuthenticationError :: Type
data AuthenticationError
  = AuthenticationErrorUserDoesNotExist
  | AuthenticationErrorWrongPassword
  deriving stock (Eq, Generic, Ord, Read, Show)

userAuthenticate ::
  (MonadLogger m, MonadSeldaPool m) =>
  Username ->
  Password ->
  SeldaTransactionT m (Either AuthenticationError User)
userAuthenticate username password = do
  lift $ logDebug $ "Querying user " <> T.pack (show username) <> " from database for password authentication."
  maybeUser :: Maybe DbUser <- Selda.queryUnique $ do
    user <- Selda.select tableUser
    Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal (unUsername username)
    return user
  case maybeUser of
    Nothing -> pure $ Left AuthenticationErrorUserDoesNotExist
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
          pure $ Left AuthenticationErrorWrongPassword
        PasswordCheckSuccess -> do
          lift $ logInfo "Password authentication succeeded."
          pure $ Right user

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
  Password ->
  -- | email
  T.Text ->
  -- | email visible
  Bool ->
  SeldaTransactionT m ()
userCreate username password email emailVisible = do
  lift $ logDebug "Creating user."
  passwordHash :: PasswordHash Bcrypt <- hashPassword password
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = unUsername username
          , dbUser_password_hash = unPasswordHash passwordHash
          , dbUser_email = email
          , dbUser_email_visible = emailVisible
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
