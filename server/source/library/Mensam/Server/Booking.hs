{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Booking where

import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.User
import Mensam.API.Order
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Kind
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

spaceLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  NameSpace ->
  SeldaTransactionT m (Maybe IdentifierSpace)
spaceLookupId name = do
  lift $ logDebug $ "Looking up space identifier with name: " <> T.pack (show name)
  maybeDbId <- Selda.queryUnique $ do
    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_name Selda..== Selda.literal (unNameSpace name)
    pure $ dbSpace Selda.! #dbSpace_id
  case maybeDbId of
    Nothing -> do
      lift $ logWarn $ "Failed to look up space. Name doesn't exist: " <> T.pack (show name)
      pure Nothing
    Just dbId -> do
      lift $ logInfo "Looked up space successfully."
      pure $ Just $ MkIdentifierSpace $ Selda.fromId @DbSpace dbId

spaceGet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  SeldaTransactionT m Space
spaceGet identifier = do
  lift $ logDebug $ "Get space info with identifier: " <> T.pack (show identifier)
  dbSpace <- Selda.queryOne $ do
    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_id Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
    pure dbSpace
  lift $ logInfo "Got space info successfully."
  pure
    MkSpace
      { spaceId = MkIdentifierSpace $ Selda.fromId $ dbSpace_id dbSpace
      , spaceName = MkNameSpace $ dbSpace_name dbSpace
      }

spaceListVisible ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  OrderByCategories SpaceOrderCategory ->
  SeldaTransactionT m [Space]
spaceListVisible userIdentifier spaceOrder = do
  lift $ logDebug $ "Looking up spaces visible by user: " <> T.pack (show userIdentifier)
  dbSpaces <- Selda.query $ do
    dbSpace <- Selda.select tableSpace
    Selda.restrict $
      dbSpace Selda.! #dbSpace_visibility Selda..== Selda.literal MkDbSpaceVisibility_visible
    let categorySelector = \case
          SpaceOrderCategoryId -> Selda.MkSomeCol $ dbSpace Selda.! #dbSpace_id
          SpaceOrderCategoryName -> Selda.MkSomeCol $ dbSpace Selda.! #dbSpace_name
    Selda.orderFlexible categorySelector spaceOrder
    pure dbSpace
  lift $ logInfo "Looked up visible spaces successfully."
  let fromDbSpace space =
        MkSpace
          { spaceId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpace_id space
          , spaceName = MkNameSpace $ dbSpace_name space
          }
  pure $ fromDbSpace <$> dbSpaces

spaceCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  NameSpace ->
  VisibilitySpace ->
  AccessibilitySpace ->
  SeldaTransactionT m IdentifierSpace
spaceCreate name visibility accessibility = do
  lift $ logDebug $ "Creating space: " <> T.pack (show name)
  let dbSpace =
        MkDbSpace
          { dbSpace_id = Selda.def
          , dbSpace_name = unNameSpace name
          , dbSpace_visibility =
              case visibility of
                MkVisibilitySpaceVisible -> MkDbSpaceVisibility_visible
                MkVisibilitySpaceHidden -> MkDbSpaceVisibility_hidden
          , dbSpace_accessibility =
              case accessibility of
                MkAccessibilitySpaceJoinable -> MkDbSpaceAccessibility_joinable
                MkAccessibilitySpaceInaccessible -> MkDbSpaceAccessibility_inaccessible
          }
  dbSpaceId <- Selda.insertWithPK tableSpace [dbSpace]
  lift $ logInfo "Created space successfully."
  pure $ MkIdentifierSpace $ Selda.fromId @DbSpace dbSpaceId

spaceUserPermissions ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m (S.Set PermissionSpaceUser)
spaceUserPermissions spaceIdentifier userIdentifier = do
  lift $ logDebug $ "Looking up user " <> T.pack (show userIdentifier) <> " for space " <> T.pack (show spaceIdentifier) <> "."
  lift $ logDebug "Looking up space permissions."
  permissions <- Selda.query $ do
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    pure $ dbSpaceUser Selda.! #dbSpaceUser_permission
  lift $ logInfo "Looked up space permissions successfully."
  let translatePermission = \case
        MkDbSpaceUserPermission_edit_desk -> MkPermissionSpaceUserEditDesk
        MkDbSpaceUserPermission_create_reservation -> MkPermissionSpaceUserCreateReservation
        MkDbSpaceUserPermission_cancel_reservation -> MkPermissionSpaceUserCancelReservation
  pure $ S.fromList $ translatePermission <$> permissions

spaceUserPermissionGive ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  PermissionSpaceUser ->
  SeldaTransactionT m ()
spaceUserPermissionGive spaceIdentifier userIdentifier permission = do
  lift $ logDebug $ "Giving user " <> T.pack (show userIdentifier) <> " permission " <> T.pack (show permission) <> " to space " <> T.pack (show spaceIdentifier) <> "."
  let dbSpaceUser =
        MkDbSpaceUser
          { dbSpaceUser_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbSpaceUser_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbSpaceUser_permission =
              case permission of
                MkPermissionSpaceUserEditDesk -> MkDbSpaceUserPermission_edit_desk
                MkPermissionSpaceUserCreateReservation -> MkDbSpaceUserPermission_create_reservation
                MkPermissionSpaceUserCancelReservation -> MkDbSpaceUserPermission_cancel_reservation
          }
  lift $ logDebug "Inserting space-user permission."
  Selda.insert_ tableSpaceUser [dbSpaceUser]
  lift $ logInfo "Gave user a space permission successfully."

spaceUserPermissionRevoke ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  PermissionSpaceUser ->
  SeldaTransactionT m ()
spaceUserPermissionRevoke spaceIdentifier userIdentifier permission = do
  lift $ logDebug $ "Revoking user " <> T.pack (show userIdentifier) <> " permission " <> T.pack (show permission) <> " to space " <> T.pack (show spaceIdentifier) <> "."
  lift $ logDebug "Deleting space-user permission."
  count <- Selda.deleteFrom tableSpaceUser $ \dbSpaceUser ->
    let
      dbPermission =
        case permission of
          MkPermissionSpaceUserEditDesk -> MkDbSpaceUserPermission_edit_desk
          MkPermissionSpaceUserCreateReservation -> MkDbSpaceUserPermission_create_reservation
          MkPermissionSpaceUserCancelReservation -> MkDbSpaceUserPermission_cancel_reservation
      isSpace = dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
      isUser = dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
      isPermission = dbSpaceUser Selda.! #dbSpaceUser_permission Selda..== Selda.literal dbPermission
     in
      isSpace Selda..&& isUser Selda..&& isPermission
  case count of
    0 -> lift $ logWarn "Space permission for user cannot be revoked, because it has not been given."
    1 -> lift $ logInfo "Revoked space permission for user successfully."
    _ -> undefined

deskLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  NameDesk ->
  SeldaTransactionT m (Maybe IdentifierDesk)
deskLookupId spaceIdentifier deskName = do
  lift $ logDebug $ "Looking up desk identifier with name: " <> T.pack (show (spaceIdentifier, deskName))
  maybeDbId <- Selda.queryUnique $ do
    dbDesk <- Selda.select tableDesk
    Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $ dbDesk Selda.! #dbDesk_name Selda..== Selda.literal (unNameDesk deskName)
    pure $ dbDesk Selda.! #dbDesk_id
  case maybeDbId of
    Nothing -> do
      lift $ logWarn $ "Failed to look up desk. Name doesn't exist: " <> T.pack (show deskName)
      pure Nothing
    Just dbId -> do
      lift $ logInfo "Looked up desk successfully."
      pure $ Just $ MkIdentifierDesk $ Selda.fromId @DbDesk dbId

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
      , deskName = MkNameDesk $ dbDesk_name dbDesk
      }

deskList ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m [Desk]
deskList spaceIdentifier userIdentifier = do
  lift $ logDebug $ "Looking up desks visible by user: " <> T.pack (show userIdentifier)
  dbDesks <- Selda.query $ do
    dbSpaceUser <- Selda.distinct $ Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)

    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_id Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $
      (dbSpace Selda.! #dbSpace_visibility Selda..== Selda.literal MkDbSpaceVisibility_visible)
        Selda..|| (dbSpace Selda.! #dbSpace_id Selda..== dbSpaceUser Selda.! #dbSpaceUser_space)

    dbDesk <- Selda.select tableDesk
    Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== dbSpace Selda.! #dbSpace_id
    pure dbDesk
  lift $ logInfo "Looked up visible desks successfully."
  let fromDbDesk desk =
        MkDesk
          { deskId = MkIdentifierDesk $ Selda.fromId @DbDesk $ dbDesk_id desk
          , deskSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbDesk_space desk
          , deskName = MkNameDesk $ dbDesk_name desk
          }
  pure $ fromDbDesk <$> dbDesks

deskCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  NameDesk ->
  IdentifierSpace ->
  SeldaTransactionT m IdentifierDesk
deskCreate deskName spaceIdentifier = do
  lift $ logDebug "Creating desk."
  let dbDesk =
        MkDbDesk
          { dbDesk_id = Selda.def
          , dbDesk_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbDesk_name = unNameDesk deskName
          }
  lift $ logDebug "Inserting desk into database."
  dbDeskId <- Selda.insertWithPK tableDesk [dbDesk]
  lift $ logInfo "Created desk successfully."
  pure $ MkIdentifierDesk $ Selda.fromId @DbDesk dbDeskId

reservationGet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierReservation ->
  SeldaTransactionT m Reservation
reservationGet identifier = do
  lift $ logDebug $ "Get reservation with identifier: " <> T.pack (show identifier)
  dbReservation <- Selda.queryOne $ do
    dbReservation <- Selda.select tableReservation
    Selda.restrict $ dbReservation Selda.! #dbReservation_id Selda..== Selda.literal (Selda.toId @DbReservation $ unIdentifierReservation identifier)
    pure dbReservation
  lift $ logInfo "Got reservation successfully."
  pure
    MkReservation
      { reservationId = MkIdentifierReservation $ Selda.fromId @DbReservation $ dbReservation_id dbReservation
      , reservationDesk = MkIdentifierDesk $ Selda.fromId @DbDesk $ dbReservation_desk dbReservation
      , reservationUser = MkIdentifierUser $ Selda.fromId @DbUser $ dbReservation_user dbReservation
      , reservationTimeBegin = dbReservation_time_begin dbReservation
      , reservationTimeEnd = dbReservation_time_end dbReservation
      , reservationStatus = case dbReservation_status dbReservation of
          MkDbReservationStatus_planned -> MkStatusReservationPlanned
          MkDbReservationStatus_cancelled -> MkStatusReservationCancelled
      }

-- | List all reservations of a desk, that overlap with the given time window.
reservationList ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  -- | minimum timestamp begin
  Maybe T.UTCTime ->
  -- | maximum timestamp end
  Maybe T.UTCTime ->
  SeldaTransactionT m [Reservation]
reservationList deskIdentifier maybeTimestampBegin maybeTimestampEnd = do
  lift $ logDebug $ "Looking up reservations: " <> T.pack (show (deskIdentifier, maybeTimestampBegin, maybeTimestampEnd))
  dbReservations <- Selda.query $ do
    dbReservation <- Selda.select tableReservation
    Selda.restrict $ dbReservation Selda.! #dbReservation_desk Selda..== Selda.literal (Selda.toId @DbDesk $ unIdentifierDesk deskIdentifier)
    case maybeTimestampBegin of
      Nothing -> pure ()
      Just timestampBegin ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_end Selda..> Selda.literal timestampBegin
    case maybeTimestampEnd of
      Nothing -> pure ()
      Just timestampEnd ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_begin Selda..< Selda.literal timestampEnd
    pure dbReservation
  lift $ logInfo "Looked up reservations successfully."
  let toReservation
        MkDbReservation
          { dbReservation_id
          , dbReservation_desk
          , dbReservation_user
          , dbReservation_time_begin
          , dbReservation_time_end
          , dbReservation_status
          } =
          MkReservation
            { reservationId = MkIdentifierReservation $ Selda.fromId @DbReservation dbReservation_id
            , reservationDesk = MkIdentifierDesk $ Selda.fromId @DbDesk dbReservation_desk
            , reservationUser = MkIdentifierUser $ Selda.fromId @DbUser dbReservation_user
            , reservationTimeBegin = dbReservation_time_begin
            , reservationTimeEnd = dbReservation_time_end
            , reservationStatus = case dbReservation_status of
                MkDbReservationStatus_planned -> MkStatusReservationPlanned
                MkDbReservationStatus_cancelled -> MkStatusReservationCancelled
            }
  pure $ toReservation <$> dbReservations

reservationCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  IdentifierUser ->
  Interval T.UTCTime ->
  SeldaTransactionT m IdentifierReservation
reservationCreate deskIdentifier userIdentifier timestampInterval = do
  lift $ logDebug "Creating reservation."
  reservations <-
    reservationList
      deskIdentifier
      (Just $ intervalStart timestampInterval)
      (Just $ intervalEnd timestampInterval)
  case filter ((== MkStatusReservationPlanned) . reservationStatus) reservations of
    _ : _ -> do
      lift $ logWarn "Desk is already reserved."
      throwM MkSqlErrorMensamDeskAlreadyReserved
    [] -> lift $ logDebug "Desk is still free."
  let dbReservation =
        MkDbReservation
          { dbReservation_id = Selda.def
          , dbReservation_desk = Selda.toId @DbDesk $ unIdentifierDesk deskIdentifier
          , dbReservation_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbReservation_time_begin = intervalStart timestampInterval
          , dbReservation_time_end = intervalEnd timestampInterval
          , dbReservation_status = MkDbReservationStatus_planned
          }
  lift $ logDebug "Inserting reservation into database."
  dbReservationId <- Selda.insertWithPK tableReservation [dbReservation]
  lift $ logInfo "Created reservation successfully."
  pure $ MkIdentifierReservation $ Selda.fromId @DbReservation dbReservationId

type SqlErrorMensamDeskAlreadyReserved :: Type
data SqlErrorMensamDeskAlreadyReserved = MkSqlErrorMensamDeskAlreadyReserved
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

reservationCancel ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierReservation ->
  SeldaTransactionT m ()
reservationCancel reservationIdentifier = do
  lift $ logDebug "Cancelling reservation."
  Selda.update_
    tableReservation
    (\dbReservation -> dbReservation Selda.! #dbReservation_id Selda..== Selda.literal (Selda.toId @DbReservation $ unIdentifierReservation reservationIdentifier))
    (\dbReservation -> dbReservation `Selda.with` [#dbReservation_status Selda.:= Selda.literal MkDbReservationStatus_cancelled])
  lift $ logInfo "Cancelled reservation successfully."
