{-# LANGUAGE OverloadedLabels #-}

module Mensam.Booking where

import Mensam.Application.SeldaPool.Class
import Mensam.Database
import Mensam.Database.Extra qualified as Selda

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Time qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

type Space :: Type
data Space = MkSpace
  { spaceId :: Selda.Identifier DbSpace
  , spaceName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

spaceLookup ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | name
  T.Text ->
  SeldaTransactionT m (Maybe Space)
spaceLookup name = do
  lift $ logDebug $ "Looking up space: " <> T.pack (show name)
  dbSpaceMaybe <- Selda.queryUnique $ do
    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_name Selda..== Selda.literal name
    pure dbSpace
  case dbSpaceMaybe of
    Nothing -> do
      lift $ logWarn $ "Failed to look up space. Name doesn't exist: " <> T.pack (show name)
      pure Nothing
    Just dbSpace -> do
      lift $ logInfo "Looked up space successfully."
      pure $
        Just
          MkSpace
            { spaceId = Selda.toIdentifier $ dbSpace_id dbSpace
            , spaceName = dbSpace_name dbSpace
            }

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
  -- | space name or identifier
  Either T.Text (Selda.Identifier DbSpace) ->
  -- | user name or identifier
  Either T.Text (Selda.Identifier DbUser) ->
  SeldaTransactionT m (Maybe Bool)
spaceUserLookup space user = do
  lift $ logDebug $ "Looking up user " <> T.pack (show user) <> " for space " <> T.pack (show space) <> "."
  dbSpaceId <-
    case space of
      Left name -> do
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
      Right spaceId -> pure $ Selda.fromIdentifier spaceId
  dbUserId <-
    case user of
      Left name -> do
        lift $ logDebug $ "Looking up user identifier with name: " <> T.pack (show name)
        maybeId <- Selda.queryUnique $ do
          dbUser <- Selda.select tableUser
          Selda.restrict $ dbUser Selda.! #dbUser_name Selda..== Selda.literal name
          pure $ dbUser Selda.! #dbUser_id
        case maybeId of
          Nothing -> do
            let msg :: T.Text = "No matching user."
            lift $ logWarn msg
            throwM $ Selda.SqlError $ show msg
          Just userId -> do
            lift $ logInfo "Looked up user identifier successfully."
            pure userId
      Right identifier -> pure $ Selda.fromIdentifier identifier
  lift $ logDebug "Look up space-user connection."
  maybeIsAdmin <- Selda.queryUnique $ do
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal dbSpaceId
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal dbUserId
    pure $ dbSpaceUser Selda.! #dbSpaceUser_is_admin
  lift $ logInfo "Looked up space-user connection successfully."
  pure maybeIsAdmin

spaceUserAdd ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | space name or identifier
  Either T.Text (Selda.Identifier DbSpace) ->
  -- | user name or identifier
  Either T.Text (Selda.Identifier DbUser) ->
  -- | user is admin?
  Bool ->
  SeldaTransactionT m ()
spaceUserAdd space user isAdmin = do
  lift $ logDebug $ "Adding user " <> T.pack (show user) <> " to space " <> T.pack (show space) <> "."
  dbSpaceUser_space <-
    case space of
      Left name -> do
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
      Right spaceId -> pure $ Selda.fromIdentifier spaceId
  dbSpaceUser_user <-
    case user of
      Left name -> do
        lift $ logDebug $ "Looking up user identifier with name: " <> T.pack (show name)
        maybeId <- Selda.queryUnique $ do
          dbUser <- Selda.select tableUser
          Selda.restrict $ dbUser Selda.! #dbUser_name Selda..== Selda.literal name
          pure $ dbUser Selda.! #dbUser_id
        case maybeId of
          Nothing -> do
            let msg :: T.Text = "No matching user."
            lift $ logWarn msg
            throwM $ Selda.SqlError $ show msg
          Just userId -> do
            lift $ logInfo "Looked up user identifier successfully."
            pure userId
      Right identifier -> pure $ Selda.fromIdentifier identifier
  let dbSpaceUser =
        MkDbSpaceUser
          { dbSpaceUser_space
          , dbSpaceUser_user
          , dbSpaceUser_is_admin = isAdmin
          }
  lift $ logDebug "Inserting space-user connection into database."
  Selda.insert_ tableSpaceUser [dbSpaceUser]
  lift $ logInfo "Created space-user connection successfully."

type Desk :: Type
data Desk = MkDesk
  { deskId :: Selda.Identifier DbDesk
  , deskName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

deskCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | desk name
  T.Text ->
  -- | space name
  Either T.Text (Selda.Identifier DbSpace) ->
  SeldaTransactionT m ()
deskCreate deskName space = do
  lift $ logDebug "Creating new desk."
  dbDesk_space <-
    case space of
      Left name -> do
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
      Right spaceId -> pure $ Selda.fromIdentifier spaceId
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
  pure undefined
