{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.User where

import Mensam.API.User
import Mensam.API.User.Username
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Database.Selda qualified as Selda
import GHC.Generics
import Text.Email.Parser
import Text.Email.Text

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
            { userId = MkIdentifierUser $ Selda.fromId $ dbUser_id dbUser
            , userName = username
            , userEmail = fromTextUnsafe $ dbUser_email dbUser
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
  SeldaTransactionT m (Maybe IdentifierUser)
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
      pure $ Just $ MkIdentifierUser $ Selda.fromId dbId

userGet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  SeldaTransactionT m User
userGet identifier = do
  lift $ logDebug $ "Get user info with identifier: " <> T.pack (show identifier)
  dbUser <- Selda.queryOne $ do
    dbUser <- Selda.select tableUser
    Selda.restrict $ dbUser Selda.! #dbUser_id Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser identifier)
    pure dbUser
  lift $ logInfo "Got user info successfully."
  pure
    MkUser
      { userId = MkIdentifierUser $ Selda.fromId $ dbUser_id dbUser
      , userName = MkUsernameUnsafe $ dbUser_name dbUser
      , userEmail = fromTextUnsafe $ dbUser_email dbUser
      }

userCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Username ->
  Password ->
  EmailAddress ->
  -- | email address visible
  Bool ->
  SeldaTransactionT m ()
userCreate username password emailAddress emailAddressVisible = do
  lift $ logDebug "Creating user."
  passwordHash :: PasswordHash Bcrypt <- hashPassword password
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = unUsername username
          , dbUser_password_hash = unPasswordHash passwordHash
          , dbUser_email = toText emailAddress
          , dbUser_email_visible = emailAddressVisible
          }
  lift $ logDebug "Inserting user into database."
  Selda.insert_ tableUser [dbUser]
  lift $ logInfo "Created user successfully."

userProfile ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Username ->
  SeldaTransactionT m (Maybe IdentifierUser)
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
      pure $ Just $ MkIdentifierUser $ Selda.fromId @DbUser dbId
