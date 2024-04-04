{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.User where

import Mensam.API.Aeson
import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time qualified as T
import Database.Selda qualified as Selda
import Deriving.Aeson qualified as A
import GHC.Generics
import System.Random
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
          , dbUser_email_validated = False
          }
  lift $ logDebug "Inserting user into database."
  dbUserId <- Selda.insertWithPK tableUser [dbUser]
  lift $ logInfo "Created user successfully."
  pure $ MkIdentifierUser $ Selda.fromId @DbUser dbUserId

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
      lift $ logInfo "Session validation failed. Session does not exist."
      pure SessionInvalid
    Just dbSession -> do
      -- TODO: Currently the session expiration is checked twice.
      --       Once by servant-auth and once in this following piece of code.
      --       For optimization the following code could be removed.
      lift $ logDebug "Queried session from database for validation. Checking whether session is still valid."
      case dbSession_time_expired dbSession of
        Nothing -> do
          lift $ logInfo "Session validation succeeded. Session does not expire."
          pure SessionValid
        Just expirationTime -> do
          currentTime <- liftIO T.getCurrentTime
          if currentTime >= expirationTime
            then do
              lift $ logInfo "Session validation failed. Session has expired."
              pure SessionInvalid
            else do
              lift $ logInfo "Session validation succeeded. Session has not yet expired."
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

userSessionDelete ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSession ->
  SeldaTransactionT m ()
userSessionDelete identifier = do
  lift $ logDebug $ "Deleting session from database: " <> T.pack (show identifier)
  count <-
    Selda.deleteFrom tableSession $ \dbSession ->
      dbSession Selda.! #dbSession_id Selda..== Selda.literal (Selda.toId @DbSession (unIdentifierSession identifier))
  case count of
    1 -> do
      lift $ logInfo "Deleted session successfully."
      pure ()
    0 -> do
      lift $ logWarn $ "Failed to delete session. There is no matching session: " <> T.pack (show identifier)
      pure ()
    n -> do
      let message = "Critical failure when trying to delete a single session. Multiple sessions have been deleted: " <> T.pack (show n)
      lift $ logError message
      error $ T.unpack message

userConfirmationConfirm ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  ConfirmationSecret ->
  SeldaTransactionT m (Either ConfirmationError ())
userConfirmationConfirm identifier secret = do
  lift $ logDebug $ "Attempting confirmation: " <> T.pack (show (identifier, secret))
  dbConfirmation <- Selda.queryOne $ do
    dbConfirmation <- Selda.select tableConfirmation
    Selda.restrict $
      (dbConfirmation Selda.! #dbConfirmation_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser identifier))
        Selda..&& (dbConfirmation Selda.! #dbConfirmation_secret Selda..== Selda.literal (unConfirmationSecret secret))
    pure dbConfirmation
  currentTime <- lift $ liftIO T.getCurrentTime
  if currentTime >= dbConfirmation_expired dbConfirmation
    then do
      lift $ logInfo $ "Confirmation has already expired: " <> T.pack (show $ dbConfirmation_expired dbConfirmation)
      pure $ Left MkConfirmationErrorExpired
    else do
      lift $ logDebug $ "Parsing confirmation effect: " <> T.pack (show $ dbConfirmation_effect dbConfirmation)
      case A.eitherDecode $ TL.encodeUtf8 $ TL.fromStrict $ dbConfirmation_effect dbConfirmation of
        Left err -> do
          lift $ logError $ "Failed to parse confirmation effect: " <> T.pack (show err)
          pure $ Left MkConfirmationErrorEffectInvalid
        Right effect -> do
          lift $ logInfo "Looked up confirmation. Running effect."
          case effect of
            MkConfirmationEffectEmailValidation emailAddress -> do
              lift $ logDebug "Confirming email address."
              dbUser <- Selda.queryOne $ do
                dbUser <- Selda.select tableUser
                Selda.restrict $ dbUser Selda.! #dbUser_id Selda..== Selda.literal (Selda.toId @DbUser (unIdentifierUser identifier))
                return dbUser
              if fromText (dbUser_email dbUser) == Right emailAddress
                then do
                  count <-
                    Selda.update
                      tableUser
                      ( \dbUserRow ->
                          dbUserRow Selda.! #dbUser_id Selda..== Selda.literal (Selda.toId @DbUser (unIdentifierUser identifier))
                      )
                      ( \dbUserRow ->
                          dbUserRow
                            `Selda.with` [ #dbUser_email_validated
                                            Selda.:= Selda.literal True
                                         ]
                      )
                  case count of
                    1 -> do
                      lift $ logInfo "Validated email address."
                      pure ()
                    0 -> do
                      let message :: T.Text = "Failed to validate email address. User not found."
                      lift $ logError message
                      error $ T.unpack message
                    n -> do
                      let message :: T.Text = "Critical failure when trying to validate a single email address. Multiple users have been affected: " <> T.pack (show n)
                      lift $ logError message
                      error $ T.unpack message
                  lift $ logDebug "Confirmed email address."
                else do
                  let message :: T.Text = "Email address has changed. Can't confirm it now."
                  lift $ logWarn message
                  error $ T.unpack message
          lift $ logDebug "Deleting confirmation."
          count <- Selda.deleteFrom tableConfirmation $ \dbConfirmation' ->
            dbConfirmation' Selda.! #dbConfirmation_id Selda..== Selda.literal (dbConfirmation_id dbConfirmation)
          case count of
            1 -> do
              lift $ logInfo "Deleted confirmation successfully."
              pure $ Right ()
            0 -> do
              let message :: T.Text = "Failed to delete confirmation. No confirmation deleted."
              lift $ logWarn message
              error $ T.unpack message
            n -> do
              let message :: T.Text = "Critical failure when trying to delete a single confirmation. Multiple confirmations have been deleted: " <> T.pack (show n)
              lift $ logError message
              error $ T.unpack message

userConfirmationCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  ConfirmationEffect ->
  T.UTCTime ->
  SeldaTransactionT m ConfirmationSecret
userConfirmationCreate userIdentifier effect expires = do
  lift $ logDebug $ "Creating confirmation for user: " <> T.pack (show userIdentifier)
  lift $ logDebug "Generating secret for confirmation."
  secret <- MkConfirmationSecret . T.pack . show @Word <$> randomIO
  lift $ logDebug "Generating secret for confirmation."
  dbConfirmationId <-
    Selda.insertWithPK
      tableConfirmation
      [ MkDbConfirmation
          { dbConfirmation_id = Selda.def
          , dbConfirmation_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbConfirmation_secret = unConfirmationSecret secret
          , dbConfirmation_expired = expires
          , dbConfirmation_effect = TL.toStrict $ A.encodeToLazyText effect
          }
      ]
  lift $ logInfo $ "Created confirmation successfully: " <> T.pack (show dbConfirmationId)
  pure secret

type ConfirmationError :: Type
data ConfirmationError
  = MkConfirmationErrorExpired
  | MkConfirmationErrorEffectInvalid
  deriving stock (Eq, Generic, Ord, Read, Show)

type ConfirmationEffect :: Type
newtype ConfirmationEffect
  = MkConfirmationEffectEmailValidation EmailAddress
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkConfirmationEffect" "") ConfirmationEffect
