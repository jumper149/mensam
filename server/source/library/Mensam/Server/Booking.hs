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
import Mensam.Server.Database.Space

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Kind
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time qualified as T
import Data.Time.Zones.All qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

spaceLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  NameSpace ->
  SeldaTransactionT m (Maybe IdentifierSpace)
spaceLookupId name = do
  lift $ logDebug $ "Looking up space identifier with name: " <> T.pack (show name)
  maybeDbSpace <- Selda.queryUnique $ spaceLookup $ unNameSpace name
  case maybeDbSpace of
    Nothing -> do
      lift $ logWarn $ "Failed to look up space. Name doesn't exist: " <> T.pack (show name)
      pure Nothing
    Just dbSpace -> do
      lift $ logInfo "Looked up space successfully."
      pure $ Just $ MkIdentifierSpace $ Selda.fromId $ dbSpace_id dbSpace

-- | Already checks permissions.
spaceView ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  IdentifierSpace ->
  SeldaTransactionT m (Maybe SpaceView)
spaceView userIdentifier spaceIdentifier = do
  lift $ logDebug $ "Get space info with identifier: " <> T.pack (show (spaceIdentifier, userIdentifier))
  permissions <- spaceUserPermissions spaceIdentifier userIdentifier
  maybeDbSpace <- Selda.queryUnique $ do
    dbSpace <- spaceGet (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $
      dbSpace Selda.! #dbSpace_visibility Selda..== Selda.literal MkDbSpaceVisibility_visible
        Selda..|| Selda.literal (MkPermissionSpaceViewSpace `S.member` permissions)
    pure dbSpace
  case maybeDbSpace of
    Nothing -> pure Nothing
    Just dbSpace -> do
      lift $ logInfo "Got space successfully."
      lift $ logDebug "Getting space roles."
      spaceRoles <- do
        dbSpaceRoles <- Selda.query $ spaceListRoles $ dbSpace_id dbSpace
        dbSpaceRolesPermissions <- traverse (Selda.query . roleListPermissions . dbSpaceRole_id) dbSpaceRoles
        let spaceRoles =
              zipWith
                ( \dbSpaceRole dbSpaceRolePermissions ->
                    MkSpaceRole
                      { spaceRoleId = MkIdentifierSpaceRole $ Selda.fromId $ dbSpaceRole_id dbSpaceRole
                      , spaceRoleName = MkNameSpaceRole $ dbSpaceRole_name dbSpaceRole
                      , spaceRolePermissions = S.fromList $ spacePermissionDbToApi . dbSpaceRolePermission_permission <$> dbSpaceRolePermissions
                      , spaceRoleAccessibility = spaceRoleAccessibilityDbToApi $ dbSpaceRole_accessibility dbSpaceRole
                      }
                )
                dbSpaceRoles
                dbSpaceRolesPermissions
        pure spaceRoles
      lift $ logInfo "Got space roles successfully."
      lift $ logDebug "Looking up space role for requesting user."
      maybeSpaceRoleIdentifier <- do
        dbMaybeSpaceRoleId <-
          Selda.queryUnique $
            spaceUserGetRole
              (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
              (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
        pure $ MkIdentifierSpaceRole . Selda.fromId @DbSpaceRole <$> dbMaybeSpaceRoleId
      lift $ logInfo "Got requesting user's space role successfully."
      pure $
        Just
          MkSpaceView
            { spaceViewId = MkIdentifierSpace $ Selda.fromId $ dbSpace_id dbSpace
            , spaceViewName = MkNameSpace $ dbSpace_name dbSpace
            , spaceViewTimezone = dbSpace_timezone dbSpace
            , spaceViewVisibility = spaceVisibilityDbToApi $ dbSpace_visibility dbSpace
            , spaceViewRoles = S.fromList spaceRoles
            , spaceViewYourRole = maybeSpaceRoleIdentifier
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

spaceRoleLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  NameSpaceRole ->
  SeldaTransactionT m (Maybe IdentifierSpaceRole)
spaceRoleLookupId spaceIdentifier name = do
  lift $ logDebug $ "Looking up space-role identifier with name: " <> T.pack (show name)
  maybeDbSpaceRole <- Selda.queryUnique $ roleLookup (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier) $ unNameSpaceRole name
  case maybeDbSpaceRole of
    Nothing -> do
      lift $ logWarn $ "Failed to look up space-role. Name doesn't exist: " <> T.pack (show name)
      pure Nothing
    Just dbSpaceRole -> do
      lift $ logInfo "Looked up space successfully."
      pure $ Just $ MkIdentifierSpaceRole $ Selda.fromId $ dbSpaceRole_id dbSpaceRole

spaceRoleGet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  SeldaTransactionT m SpaceRole
spaceRoleGet identifier = do
  lift $ logDebug $ "Get space role info with identifier: " <> T.pack (show identifier)
  dbSpaceRole <- Selda.queryOne $ roleGet $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier
  dbSpaceRolePermissions <- Selda.query $ roleListPermissions $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier
  lift $ logInfo "Got space role info successfully."
  pure
    MkSpaceRole
      { spaceRoleId = MkIdentifierSpaceRole $ Selda.fromId $ dbSpaceRole_id dbSpaceRole
      , spaceRoleName = MkNameSpaceRole $ dbSpaceRole_name dbSpaceRole
      , spaceRolePermissions = S.fromList $ spacePermissionDbToApi . dbSpaceRolePermission_permission <$> dbSpaceRolePermissions
      , spaceRoleAccessibility = spaceRoleAccessibilityDbToApi $ dbSpaceRole_accessibility dbSpaceRole
      }

spaceCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  NameSpace ->
  T.TZLabel ->
  VisibilitySpace ->
  SeldaTransactionT m IdentifierSpace
spaceCreate name timezoneLabel visibility = do
  lift $ logDebug $ "Creating space: " <> T.pack (show name)
  let dbSpace =
        MkDbSpace
          { dbSpace_id = Selda.def
          , dbSpace_name = unNameSpace name
          , dbSpace_timezone = timezoneLabel
          , dbSpace_visibility = spaceVisibilityApiToDb visibility
          }
  dbSpaceId <- Selda.insertWithPK tableSpace [dbSpace]
  lift $ logInfo "Created space successfully."
  pure $ MkIdentifierSpace $ Selda.fromId @DbSpace dbSpaceId

spaceDelete ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  SeldaTransactionT m ()
spaceDelete identifier = do
  lift $ logDebug $ "Deleting space: " <> T.pack (show identifier)
  dbDeskIdentifiers <- Selda.query $ do
    dbDesk <- Selda.select tableDesk
    Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
    pure $ dbDesk Selda.! #dbDesk_id
  lift $ logDebug "Deleting desks."
  traverse_ (deskDelete . MkIdentifierDesk . Selda.fromId @DbDesk) dbDeskIdentifiers
  lift $ logDebug $ "Deleted " <> T.pack (show (length dbDeskIdentifiers)) <> " desks."
  countMembers <- Selda.deleteFrom tableSpaceUser $ \row ->
    row Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
  lift $ logDebug $ "Space had " <> T.pack (show countMembers) <> " memberships."
  dbSpaceRoleIdentifiers <- Selda.query $ do
    dbDesk <- Selda.select tableSpaceRole
    Selda.restrict $ dbDesk Selda.! #dbSpaceRole_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
    pure $ dbDesk Selda.! #dbSpaceRole_id
  lift $ logDebug "Deleting space roles."
  traverse_ (spaceRoleDelete . MkIdentifierSpaceRole . Selda.fromId @DbSpaceRole) dbSpaceRoleIdentifiers
  lift $ logDebug $ "Space had " <> T.pack (show (length dbSpaceRoleIdentifiers)) <> " space roles."
  Selda.deleteOneFrom tableSpace $ \row ->
    row Selda.! #dbSpace_id Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
  lift $ logInfo "Deleted space successfully."
  pure ()

spaceUserAdd ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  IdentifierSpaceRole ->
  SeldaTransactionT m ()
spaceUserAdd spaceIdentifier userIdentifier roleIdentifier = do
  lift $ logDebug $ "Adding user " <> T.pack (show userIdentifier) <> " to space " <> T.pack (show spaceIdentifier) <> " as " <> T.pack (show roleIdentifier) <> "."
  let dbSpaceUser =
        MkDbSpaceUser
          { dbSpaceUser_id = Selda.def
          , dbSpaceUser_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbSpaceUser_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbSpaceUser_role = Selda.toId @DbSpaceRole $ unIdentifierSpaceRole roleIdentifier
          }
  _dbSpaceUserId <- Selda.insertWithPK tableSpaceUser [dbSpaceUser]
  lift $ logInfo "Created space-user successfully."
  pure ()

spaceUserPermissions ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m (S.Set PermissionSpace)
spaceUserPermissions spaceIdentifier userIdentifier = do
  lift $ logDebug $ "Looking up user " <> T.pack (show userIdentifier) <> " permissions for space " <> T.pack (show spaceIdentifier) <> "."
  permissions <-
    Selda.query $
      spaceUserListPermissions
        (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
        (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
  lift $ logInfo "Looked up space permissions successfully."
  pure $ S.fromList $ spacePermissionDbToApi . dbSpaceRolePermission_permission <$> permissions

spaceRoleCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  NameSpaceRole ->
  AccessibilitySpaceRole ->
  SeldaTransactionT m IdentifierSpaceRole
spaceRoleCreate spaceIdentifier roleName accessibility = do
  lift $ logDebug $ "Creating role: " <> T.pack (show (spaceIdentifier, roleName))
  let dbSpaceRole =
        MkDbSpaceRole
          { dbSpaceRole_id = Selda.def
          , dbSpaceRole_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbSpaceRole_name = unNameSpaceRole roleName
          , dbSpaceRole_accessibility = spaceRoleAccessibilityApiToDb accessibility
          }
  lift $ logDebug "Inserting space-role into database."
  dbSpaceRoleId <- Selda.insertWithPK tableSpaceRole [dbSpaceRole]
  lift $ logInfo "Created role successfully."
  pure $ MkIdentifierSpaceRole $ Selda.fromId @DbSpaceRole dbSpaceRoleId

spaceRoleDelete ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  SeldaTransactionT m ()
spaceRoleDelete identifier = do
  lift $ logDebug $ "Deleting space role: " <> T.pack (show identifier)
  countPermissions <- Selda.deleteFrom tableSpaceRolePermission $ \row ->
    row Selda.! #dbSpaceRolePermission_role Selda..== Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier)
  lift $ logDebug $ "Role had " <> T.pack (show countPermissions) <> " permissions."
  Selda.deleteOneFrom tableSpaceRole $ \row ->
    row Selda.! #dbSpaceRole_id Selda..== Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier)
  lift $ logInfo "Deleted space role successfully."
  pure ()

spaceRolePermissionGive ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  PermissionSpace ->
  SeldaTransactionT m ()
spaceRolePermissionGive spaceRoleIdentifier permission = do
  lift $ logDebug $ "Giving role " <> T.pack (show spaceRoleIdentifier) <> " permission " <> T.pack (show permission) <> "."
  let dbSpaceRolePermission =
        MkDbSpaceRolePermission
          { dbSpaceRolePermission_id = Selda.def
          , dbSpaceRolePermission_role = Selda.toId @DbSpaceRole $ unIdentifierSpaceRole spaceRoleIdentifier
          , dbSpaceRolePermission_permission = spacePermissionApiToDb permission
          }
  lift $ logDebug "Inserting space-role permission."
  Selda.insert_ tableSpaceRolePermission [dbSpaceRolePermission]
  lift $ logInfo "Gave space-role a permission successfully."

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

deskDelete ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  SeldaTransactionT m ()
deskDelete identifier = do
  lift $ logDebug $ "Deleting desk: " <> T.pack (show identifier)
  countReservations <- Selda.deleteFrom tableReservation $ \row ->
    row Selda.! #dbReservation_desk Selda..== Selda.literal (Selda.toId @DbDesk $ unIdentifierDesk identifier)
  lift $ logDebug $ "Desk had " <> T.pack (show countReservations) <> " reservations."
  Selda.deleteOneFrom tableDesk $ \row ->
    row Selda.! #dbDesk_id Selda..== Selda.literal (Selda.toId @DbDesk $ unIdentifierDesk identifier)
  lift $ logInfo "Deleted desk successfully."
  pure ()

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
  IntervalNonDegenerate T.UTCTime ->
  SeldaTransactionT m IdentifierReservation
reservationCreate deskIdentifier userIdentifier timestampInterval = do
  lift $ logDebug "Creating reservation."
  let
    start = intervalStart $ unIntervalNonDegenerate timestampInterval
    end = intervalEnd $ unIntervalNonDegenerate timestampInterval
  reservations <- reservationList deskIdentifier (Just start) (Just end)
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
          , dbReservation_time_begin = start
          , dbReservation_time_end = end
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

spaceVisibilityApiToDb :: VisibilitySpace -> DbSpaceVisibility
spaceVisibilityApiToDb = \case
  MkVisibilitySpaceVisible -> MkDbSpaceVisibility_visible
  MkVisibilitySpaceHidden -> MkDbSpaceVisibility_hidden

spaceVisibilityDbToApi :: DbSpaceVisibility -> VisibilitySpace
spaceVisibilityDbToApi = \case
  MkDbSpaceVisibility_visible -> MkVisibilitySpaceVisible
  MkDbSpaceVisibility_hidden -> MkVisibilitySpaceHidden

spaceRoleAccessibilityApiToDb :: AccessibilitySpaceRole -> DbSpaceRoleAccessibility
spaceRoleAccessibilityApiToDb = \case
  MkAccessibilitySpaceRoleJoinable -> MkDbSpaceRoleAccessibility_joinable
  MkAccessibilitySpaceRoleInaccessible -> MkDbSpaceRoleAccessibility_inaccessible

spaceRoleAccessibilityDbToApi :: DbSpaceRoleAccessibility -> AccessibilitySpaceRole
spaceRoleAccessibilityDbToApi = \case
  MkDbSpaceRoleAccessibility_joinable -> MkAccessibilitySpaceRoleJoinable
  MkDbSpaceRoleAccessibility_inaccessible -> MkAccessibilitySpaceRoleInaccessible

spacePermissionApiToDb :: PermissionSpace -> DbSpacePermission
spacePermissionApiToDb = \case
  MkPermissionSpaceViewSpace -> MkDbSpacePermission_view_space
  MkPermissionSpaceEditDesk -> MkDbSpacePermission_edit_desk
  MkPermissionSpaceEditSpace -> MkDbSpacePermission_edit_space
  MkPermissionSpaceCreateReservation -> MkDbSpacePermission_create_reservation
  MkPermissionSpaceCancelReservation -> MkDbSpacePermission_cancel_reservation

spacePermissionDbToApi :: DbSpacePermission -> PermissionSpace
spacePermissionDbToApi = \case
  MkDbSpacePermission_view_space -> MkPermissionSpaceViewSpace
  MkDbSpacePermission_edit_desk -> MkPermissionSpaceEditDesk
  MkDbSpacePermission_edit_space -> MkPermissionSpaceEditSpace
  MkDbSpacePermission_create_reservation -> MkPermissionSpaceCreateReservation
  MkDbSpacePermission_cancel_reservation -> MkPermissionSpaceCancelReservation
