{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Booking where

import Mensam.API.Aeson
import Mensam.API.Desk
import Mensam.API.Space
import Mensam.API.User
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Text qualified as T
import Data.Time qualified as T
import Database.Selda qualified as Selda

spaceLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | name
  T.Text ->
  SeldaTransactionT m (Maybe IdentifierSpace)
spaceLookupId name = do
  lift $ logDebug $ "Looking up space identifier with name: " <> T.pack (show name)
  maybeDbId <- Selda.queryUnique $ do
    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_name Selda..== Selda.literal name
    pure $ dbSpace Selda.! #dbSpace_id
  case maybeDbId of
    Nothing -> do
      lift $ logWarn $ "Failed to look up space. Name doesn't exist: " <> T.pack (show name)
      pure Nothing
    Just dbId -> do
      lift $ logInfo "Looked up space successfully."
      pure $ Just $ MkIdentifierSpace $ Selda.fromId @DbSpace dbId

spaceList ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  SeldaTransactionT m [Space]
spaceList userIdentifier = do
  lift $ logDebug $ "Looking up spaces visible by user: " <> T.pack (show userIdentifier)
  dbSpaces <- Selda.query $ do
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_id Selda..== dbSpaceUser Selda.! #dbSpaceUser_space
    pure dbSpace
  lift $ logInfo "Looked up visible spaces successfully."
  let fromDbSpace space =
        MkSpace
          { spaceId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpace_id space
          , spaceName = dbSpace_name space
          }
  pure $ fromDbSpace <$> dbSpaces

spaceCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | name
  T.Text ->
  -- | visible
  Bool ->
  SeldaTransactionT m ()
spaceCreate name visible = do
  lift $ logDebug $ "Creating space: " <> T.pack (show name)
  let dbSpace =
        MkDbSpace
          { dbSpace_id = Selda.def
          , dbSpace_name = name
          , dbSpace_visible = visible
          }
  Selda.insert_ tableSpace [dbSpace]
  lift $ logInfo "Created space successfully."

spaceUserLookup ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  NameOrIdentifier T.Text IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m (Maybe Bool)
spaceUserLookup space userIdentifier = do
  lift $ logDebug $ "Looking up user " <> T.pack (show userIdentifier) <> " for space " <> T.pack (show space) <> "."
  spaceIdentifier <-
    case space of
      Identifier spaceId -> pure spaceId
      Name name ->
        spaceLookupId name >>= \case
          Just identifier -> pure identifier
          Nothing -> do
            let msg :: T.Text = "No matching space."
            lift $ logWarn msg
            throwM $ Selda.SqlError $ show msg
  lift $ logDebug "Look up space-user connection."
  maybeIsAdmin <- Selda.queryUnique $ do
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    pure $ dbSpaceUser Selda.! #dbSpaceUser_is_admin
  lift $ logInfo "Looked up space-user connection successfully."
  pure maybeIsAdmin

spaceUserAdd ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | space name or identifier
  Either T.Text IdentifierSpace ->
  IdentifierUser ->
  -- | user is admin?
  Bool ->
  SeldaTransactionT m ()
spaceUserAdd space userIdentifier isAdmin = do
  lift $ logDebug $ "Adding user " <> T.pack (show userIdentifier) <> " to space " <> T.pack (show space) <> "."
  spaceIdentifier <-
    case space of
      Right spaceId -> pure spaceId
      Left name ->
        spaceLookupId name >>= \case
          Just identifier -> pure identifier
          Nothing -> do
            let msg :: T.Text = "No matching space."
            lift $ logWarn msg
            throwM $ Selda.SqlError $ show msg
  let dbSpaceUser =
        MkDbSpaceUser
          { dbSpaceUser_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbSpaceUser_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbSpaceUser_is_admin = isAdmin
          }
  lift $ logDebug "Inserting space-user connection."
  Selda.insert_ tableSpaceUser [dbSpaceUser]
  lift $ logInfo "Created space-user connection successfully."

deskList ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m [Desk]
deskList spaceIdentifier userIdentifier = do
  lift $ logDebug $ "Looking up desks visible by user: " <> T.pack (show userIdentifier)
  dbDesks <- Selda.query $ do
    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_id Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $
      (dbSpace Selda.! #dbSpace_visible)
        Selda..|| (dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier))
    dbDesk <- Selda.select tableDesk
    Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== dbSpace Selda.! #dbSpace_id
    pure dbDesk
  lift $ logInfo "Looked up visible desks successfully."
  let fromDbDesk desk =
        MkDesk
          { deskId = MkIdentifierDesk $ Selda.fromId @DbDesk $ dbDesk_id desk
          , deskName = dbDesk_name desk
          }
  pure $ fromDbDesk <$> dbDesks

deskCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | desk name
  T.Text ->
  NameOrIdentifier T.Text IdentifierSpace ->
  SeldaTransactionT m ()
deskCreate deskName space = do
  lift $ logDebug "Creating new desk."
  dbDesk_space <-
    case space of
      Name name -> do
        lift $ logDebug $ "Looking up space identifier with name: " <> T.pack (show name)
        maybeId <- Selda.queryUnique $ do
          dbSpace <- Selda.select tableSpace
          Selda.restrict $ dbSpace Selda.! #dbSpace_name Selda..== Selda.literal name
          pure $ dbSpace Selda.! #dbSpace_id
        case maybeId of
          Nothing -> do
            let msg :: T.Text = "No matching space."
            lift $ logWarn msg
            throwM $ Selda.SqlError $ show msg
          Just spaceId -> do
            lift $ logInfo "Looked up space identifier successfully."
            pure spaceId
      Identifier spaceId -> pure $ Selda.toId $ unIdentifierSpace spaceId
  let dbDesk =
        MkDbDesk
          { dbDesk_id = Selda.def
          , dbDesk_space
          , dbDesk_name = deskName
          }
  lift $ logDebug "Inserting new desk into database."
  Selda.insert_ tableDesk [dbDesk]
  lift $ logInfo "Created new desk successfully."

reservationCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | space name
  T.Text ->
  -- | desk name
  T.Text ->
  -- | user name
  T.Text ->
  -- | timestamp begin
  T.UTCTime ->
  -- | timestamp end
  T.UTCTime ->
  SeldaTransactionT m ()
reservationCreate spaceName deskName userName timestampBegin timestampEnd = do
  lift $ logDebug "Creating new reservation."
  dbSpace_id <- do
    maybeSpaceId <- Selda.queryUnique $ do
      space <- Selda.select tableSpace
      Selda.restrict $ space Selda.! #dbSpace_name Selda..== Selda.literal spaceName
      pure $ space Selda.! #dbSpace_id
    case maybeSpaceId of
      Nothing -> do
        let msg :: T.Text = "No matching space."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      Just spaceId -> pure spaceId
  dbDesk_id <- do
    maybeDeskId <- Selda.queryUnique $ do
      desk <- Selda.select tableDesk
      Selda.restrict $ desk Selda.! #dbDesk_space Selda..== Selda.literal dbSpace_id
      Selda.restrict $ desk Selda.! #dbDesk_name Selda..== Selda.literal deskName
      pure $ desk Selda.! #dbDesk_id
    case maybeDeskId of
      Nothing -> do
        let msg :: T.Text = "No matching desk."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      Just deskId -> pure deskId
  dbUser_id <- do
    maybeUserId <- Selda.queryUnique $ do
      user <- Selda.select tableUser
      Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal userName
      pure $ user Selda.! #dbUser_id
    case maybeUserId of
      Nothing -> do
        let msg :: T.Text = "No matching user."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      Just userId -> pure userId
  let dbReservation =
        MkDbReservation
          { dbReservation_id = Selda.def
          , dbReservation_desk = dbDesk_id
          , dbReservation_user = dbUser_id
          , dbReservation_time_begin = timestampBegin
          , dbReservation_time_end = timestampEnd
          }
  lift $ logDebug "Inserting reservation into database."
  Selda.insert_ tableReservation [dbReservation]
  lift $ logInfo "Created reservation successfully."
  pure ()
