{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.User where

import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Time qualified as T
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
  SeldaTransactionT m (Either AuthenticationError UserAuthenticated)
userAuthenticate username password = do
  lift $ logDebug $ "Querying user " <> T.pack (show username) <> " from database for password authentication."
  maybeUser :: Maybe DbUser <- Selda.queryUnique $ do
    user <- Selda.select tableUser
    Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal (unUsername username)
    return user
  case maybeUser of
    Nothing -> do
      lift $ logInfo "Password authentication failed because username does not exist."
      pure $ Left AuthenticationErrorUserDoesNotExist
    Just dbUser -> do
      let passwordHash = PasswordHash $ dbUser_password_hash dbUser
      lift $ logDebug "Queried user from database for password authentication. Checking password now."
      case checkPassword password passwordHash of
        PasswordCheckFail -> do
          lift $ logInfo "Password authentication failed because of wrong password."
          pure $ Left AuthenticationErrorWrongPassword
        PasswordCheckSuccess -> do
          lift $ logInfo "Password authentication succeeded."
          pure $
            Right
              MkUserAuthenticated
                { userAuthenticatedId = MkIdentifierUser $ Selda.fromId @DbUser $ dbUser_id dbUser
                , userAuthenticatedSession = Nothing
                }

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
  SeldaTransactionT m IdentifierUser
userCreate username password emailAddress emailAddressVisible = do
  lift $ logDebug "Creating user."
  passwordHash :: PasswordHash Bcrypt <- hashPassword password
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = unUsername username
          , dbUser_password_hash = unPasswordHash passwordHash
          , dbUser_email = toText emailAddress
          , dbUser_email_visibility =
              if emailAddressVisible
                then MkDbEmailVisibility_visible
                else MkDbEmailVisibility_hidden
          }
  lift $ logDebug "Inserting user into database."
  dbUserId <- Selda.insertWithPK tableUser [dbUser]
  lift $ logInfo "Created user successfully."
  pure $ MkIdentifierUser $ Selda.fromId @DbUser dbUserId

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

type SessionValidity :: Type
data SessionValidity
  = SessionValid
  | SessionInvalid
  deriving stock (Eq, Generic, Ord, Read, Show)

userSessionValidate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSession ->
  SeldaTransactionT m SessionValidity
userSessionValidate identifier = do
  lift $ logDebug $ "Querying session " <> T.pack (show identifier) <> " from database to validate."
  maybeSession :: Maybe DbSession <- Selda.queryUnique $ do
    session <- Selda.select tableSession
    Selda.restrict $ session Selda.! #dbSession_id Selda..== Selda.literal (Selda.toId @DbSession $ unIdentifierSession identifier)
    return session
  case maybeSession of
    Nothing -> do
      lift $ logWarn "Session validation failed because session does not exist."
      pure SessionInvalid
    Just _dbSession -> do
      lift $ logDebug "Queried session from database for validation. Checking whether session is still valid."
      -- TODO: Currently there is no flag to disable sessions in the database, but that can be added in the future.
      --       We could also just remove sessions from the table, but a flag is probably better.
      lift $ logInfo "Session validation succeeded."
      pure SessionValid

userSessionGet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSession ->
  SeldaTransactionT m Session
userSessionGet identifier = do
  lift $ logDebug $ "Get session info with identifier: " <> T.pack (show identifier)
  dbSession <- Selda.queryOne $ do
    dbSession <- Selda.select tableSession
    Selda.restrict $ dbSession Selda.! #dbSession_id Selda..== Selda.literal (Selda.toId @DbSession $ unIdentifierSession identifier)
    pure dbSession
  lift $ logInfo "Got session info successfully."
  pure
    MkSession
      { sessionId = MkIdentifierSession $ Selda.fromId @DbSession $ dbSession_id dbSession
      , sessionTimeCreated = dbSession_time_created dbSession
      , sessionTimeExpired = dbSession_time_expired dbSession
      }

userSessionCreate ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  -- | session created
  T.UTCTime ->
  -- | session expires
  Maybe T.UTCTime ->
  SeldaTransactionT m IdentifierSession
userSessionCreate userIdentifier timeCreated maybeTimeExpired = do
  lift $ logDebug $ "Creating session for user: " <> T.pack (show userIdentifier)
  lift $ logDebug "Inserting session into database."
  dbSessionId <-
    Selda.insertWithPK
      tableSession
      [ MkDbSession
          { dbSession_id = Selda.def
          , dbSession_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbSession_time_created = timeCreated
          , dbSession_time_expired = maybeTimeExpired
          }
      ]
  lift $ logInfo "Created session successfully."
  pure $ MkIdentifierSession $ Selda.fromId @DbSession dbSessionId
