{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Booking where

import Mensam.API.Desk
import Mensam.API.Order
import Mensam.API.Space
import Mensam.API.User
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

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
  OrderByCategories SpaceOrderCategory ->
  SeldaTransactionT m [Space]
spaceList userIdentifier spaceOrder = do
  lift $ logDebug $ "Looking up spaces visible by user: " <> T.pack (show userIdentifier)
  dbSpaces <- Selda.query $ do
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_id Selda..== dbSpaceUser Selda.! #dbSpaceUser_space
    let categorySelector = \case
          SpaceOrderCategoryId -> Selda.MkSomeCol $ dbSpace Selda.! #dbSpace_id
          SpaceOrderCategoryName -> Selda.MkSomeCol $ dbSpace Selda.! #dbSpace_name
    Selda.orderFlexible categorySelector spaceOrder
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
  IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m (Maybe Bool)
spaceUserLookup spaceIdentifier userIdentifier = do
  lift $ logDebug $ "Looking up user " <> T.pack (show userIdentifier) <> " for space " <> T.pack (show spaceIdentifier) <> "."
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
  IdentifierSpace ->
  IdentifierUser ->
  -- | user is admin?
  Bool ->
  SeldaTransactionT m ()
spaceUserAdd spaceIdentifier userIdentifier isAdmin = do
  lift $ logDebug $ "Adding user " <> T.pack (show userIdentifier) <> " to space " <> T.pack (show spaceIdentifier) <> "."
  let dbSpaceUser =
        MkDbSpaceUser
          { dbSpaceUser_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbSpaceUser_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbSpaceUser_is_admin = isAdmin
          }
  lift $ logDebug "Inserting space-user connection."
  Selda.insert_ tableSpaceUser [dbSpaceUser]
  lift $ logInfo "Created space-user connection successfully."

deskLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | name
  T.Text ->
  SeldaTransactionT m (Maybe IdentifierDesk)
deskLookupId name = do
  lift $ logDebug $ "Looking up desk identifier with name: " <> T.pack (show name)
  maybeDbId <- Selda.queryUnique $ do
    dbDesk <- Selda.select tableDesk
    Selda.restrict $ dbDesk Selda.! #dbDesk_name Selda..== Selda.literal name
    pure $ dbDesk Selda.! #dbDesk_id
  case maybeDbId of
    Nothing -> do
      lift $ logWarn $ "Failed to look up desk. Name doesn't exist: " <> T.pack (show name)
      pure Nothing
    Just dbId -> do
      lift $ logInfo "Looked up desk successfully."
      pure $ Just $ MkIdentifierDesk $ Selda.fromId @DbDesk dbId

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
          , deskSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbDesk_space desk
          , deskName = dbDesk_name desk
          }
  pure $ fromDbDesk <$> dbDesks

deskCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | desk name
  T.Text ->
  IdentifierSpace ->
  SeldaTransactionT m ()
deskCreate deskName spaceIdentifier = do
  lift $ logDebug "Creating desk."
  let dbDesk =
        MkDbDesk
          { dbDesk_id = Selda.def
          , dbDesk_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbDesk_name = deskName
          }
  lift $ logDebug "Inserting desk into database."
  Selda.insert_ tableDesk [dbDesk]
  lift $ logInfo "Created desk successfully."

deskGet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  SeldaTransactionT m Desk
deskGet identifier = do
  lift $ logDebug $ "Get desk info with identifier: " <> T.pack (show identifier)
  dbDesk <- Selda.queryOne $ do
    dbDesk <- Selda.select tableDesk
    Selda.restrict $ dbDesk Selda.! #dbDesk_id Selda..== Selda.literal (Selda.toId @DbDesk $ unIdentifierDesk identifier)
    pure dbDesk
  lift $ logInfo "Got desk info successfully."
  pure
    MkDesk
      { deskId = MkIdentifierDesk $ Selda.fromId $ dbDesk_id dbDesk
      , deskSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbDesk_space dbDesk
      , deskName = dbDesk_name dbDesk
      }

reservationCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  IdentifierUser ->
  -- | timestamp begin
  T.UTCTime ->
  -- | timestamp end
  T.UTCTime ->
  SeldaTransactionT m ()
reservationCreate deskIdentifier userIdentifier timestampBegin timestampEnd = do
  lift $ logDebug "Creating reservation."
  let dbReservation =
        MkDbReservation
          { dbReservation_id = Selda.def
          , dbReservation_desk = Selda.toId @DbDesk $ unIdentifierDesk deskIdentifier
          , dbReservation_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbReservation_time_begin = timestampBegin
          , dbReservation_time_end = timestampEnd
          }
  lift $ logDebug "Inserting reservation into database."
  Selda.insert_ tableReservation [dbReservation]
  lift $ logInfo "Created reservation successfully."
  pure ()
