{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Booking where

import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User
import Mensam.API.Order
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema
import Mensam.Server.Database.Space

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Password.Bcrypt
import Data.Set qualified as S
import Data.Singletons
import Data.Text qualified as T
import Data.Time qualified as T
import Data.Time.Zones.All qualified as T
import Data.Typeable
import Database.Selda qualified as Selda
import GHC.Generics

type SqlErrorMensamSpacePermissionNotSatisfied :: PermissionSpace -> Type
data SqlErrorMensamSpacePermissionNotSatisfied permission = MkSqlErrorMensamSpacePermissionNotSatisfied
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

checkPermission ::
  (MonadLogger m, MonadSeldaPool m, Typeable p) =>
  SPermissionSpace p ->
  IdentifierUser ->
  IdentifierSpace ->
  SeldaTransactionT m ()
checkPermission sPermission userIdentifier spaceIdentifier = do
  let permission = fromSing sPermission
  lift $ logDebug $ "Checking permission " <> T.pack (show permission) <> " for user " <> T.pack (show userIdentifier) <> " in space " <> T.pack (show spaceIdentifier) <> "."
  isOwner <- spaceUserIsOwner spaceIdentifier userIdentifier
  if isOwner
    then lift $ logInfo "User is owner. Permission granted."
    else do
      permissions <- spaceUserPermissions spaceIdentifier userIdentifier
      if permission `S.member` permissions
        then lift $ logInfo "Permission satisfied."
        else do
          lift $ logInfo "Permission was not satisfied."
          case sPermission of
            (_ :: SPermissionSpace permission) -> throwM $ MkSqlErrorMensamSpacePermissionNotSatisfied @permission

spaceLookupId ::
  (MonadLogger m, MonadSeldaPool m) =>
  NameSpace ->
  SeldaTransactionT m IdentifierSpace
spaceLookupId name = do
  lift $ logDebug $ "Looking up space identifier with name: " <> T.pack (show name)
  dbSpace <- catch (Selda.queryOne $ spaceLookup $ unNameSpace name) $
    \case exc -> throwM $ MkSqlErrorMensamSpaceNotFound exc
  lift $ logInfo "Looked up space successfully."
  pure $ MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpace_id dbSpace

spaceGetFromId ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  SeldaTransactionT m Space
spaceGetFromId identifier = do
  lift $ logDebug $ "Get space info with identifier: " <> T.pack (show identifier)
  dbSpace <- catch (Selda.queryOne $ spaceGet $ Selda.toId @DbSpace $ unIdentifierSpace identifier) $
    \case exc -> throwM $ MkSqlErrorMensamSpaceNotFound exc
  lift $ logInfo "Got space info successfully."
  pure
    MkSpace
      { spaceId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpace_id dbSpace
      , spaceName = MkNameSpace $ dbSpace_name dbSpace
      , spaceTimezone = dbSpace_timezone dbSpace
      , spaceOwner = MkIdentifierUser $ Selda.fromId @DbUser $ dbSpace_owner dbSpace
      }

spaceInternalGetFromId ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  SeldaTransactionT m SpaceInternal
spaceInternalGetFromId identifier = do
  lift $ logDebug $ "Get space info with identifier: " <> T.pack (show identifier)
  dbSpace <- catch (Selda.queryOne $ spaceGet $ Selda.toId @DbSpace $ unIdentifierSpace identifier) $
    \case exc -> throwM $ MkSqlErrorMensamSpaceNotFound exc
  lift $ logInfo "Got space info successfully."
  pure
    MkSpaceInternal
      { spaceInternalId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpace_id dbSpace
      , spaceInternalName = MkNameSpace $ dbSpace_name dbSpace
      , spaceInternalTimezone = dbSpace_timezone dbSpace
      , spaceInternalVisibility = spaceVisibilityDbToApi $ dbSpace_visibility dbSpace
      , spaceInternalOwner = MkIdentifierUser $ Selda.fromId @DbUser $ dbSpace_owner dbSpace
      }

type SpaceInternal :: Type
data SpaceInternal = MkSpaceInternal
  { spaceInternalId :: IdentifierSpace
  , spaceInternalName :: NameSpace
  , spaceInternalTimezone :: T.TZLabel
  , spaceInternalVisibility :: VisibilitySpace
  , spaceInternalOwner :: IdentifierUser
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

type SqlErrorMensamSpaceNotFound :: Type
newtype SqlErrorMensamSpaceNotFound = MkSqlErrorMensamSpaceNotFound Selda.SqlErrorMensamNotOneQuery
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

-- | Already checks permissions.
spaceView ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  IdentifierSpace ->
  SeldaTransactionT m (Maybe SpaceView)
spaceView userIdentifier spaceIdentifier = do
  lift $ logDebug $ "Get space info with identifier: " <> T.pack (show (spaceIdentifier, userIdentifier))
  permissions <- spaceUserPermissions spaceIdentifier userIdentifier
  maybeDbSpace <- Selda.queryUnique $ do
    dbSpace <- spaceGet (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $
      dbSpace
        Selda.! #dbSpace_visibility
        Selda..== Selda.literal MkDbSpaceVisibility_visible
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
                      { spaceRoleId = MkIdentifierSpaceRole $ Selda.fromId @DbSpaceRole $ dbSpaceRole_id dbSpaceRole
                      , spaceRoleSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpaceRole_space dbSpaceRole
                      , spaceRoleName = MkNameSpaceRole $ dbSpaceRole_name dbSpaceRole
                      , spaceRolePermissions = S.fromList $ spacePermissionDbToApi . dbSpaceRolePermission_permission <$> dbSpaceRolePermissions
                      , spaceRoleAccessibility = spaceRoleAccessibilityDbToApi $ dbSpaceRole_accessibility dbSpaceRole
                      }
                )
                dbSpaceRoles
                dbSpaceRolesPermissions
        pure spaceRoles
      dbSpaceUsers <- Selda.query $ spaceListUsers $ dbSpace_id dbSpace
      let spaceUsers =
            ( \dbSpaceUser ->
                MkSpaceUser
                  { spaceUserUser = MkIdentifierUser $ Selda.fromId @DbUser $ dbSpaceUser_user dbSpaceUser
                  , spaceUserRole = MkIdentifierSpaceRole $ Selda.fromId @DbSpaceRole $ dbSpaceUser_role dbSpaceUser
                  }
            )
              <$> dbSpaceUsers
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
            { spaceViewId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpace_id dbSpace
            , spaceViewName = MkNameSpace $ dbSpace_name dbSpace
            , spaceViewTimezone = dbSpace_timezone dbSpace
            , spaceViewVisibility = spaceVisibilityDbToApi $ dbSpace_visibility dbSpace
            , spaceViewOwner = MkIdentifierUser $ Selda.fromId @DbUser $ dbSpace_owner dbSpace
            , spaceViewRoles = S.fromList spaceRoles
            , spaceViewUsers = S.fromList spaceUsers
            , spaceViewYourRole = maybeSpaceRoleIdentifier
            }

type SpaceView :: Type
data SpaceView = MkSpaceView
  { spaceViewId :: IdentifierSpace
  , spaceViewName :: NameSpace
  , spaceViewTimezone :: T.TZLabel
  , spaceViewVisibility :: VisibilitySpace
  , spaceViewOwner :: IdentifierUser
  , spaceViewRoles :: S.Set SpaceRole
  , spaceViewUsers :: S.Set SpaceUser
  , spaceViewYourRole :: Maybe IdentifierSpaceRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

spaceListVisible ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  OrderByCategories SpaceOrderCategory ->
  -- | Predicate whether the user has to be a member of the space.
  Maybe Bool ->
  SeldaTransactionT m [Space]
spaceListVisible userIdentifier spaceOrder maybeIsMember = do
  lift $ logDebug $ "Looking up spaces visible by user: " <> T.pack (show userIdentifier)
  dbSpaces <- Selda.query $ do
    dbSpace <- Selda.select tableSpace
    dbSpaceUser <-
      Selda.leftJoin
        ( \dbSpaceUser ->
            dbSpaceUser Selda.! #dbSpaceUser_space Selda..== dbSpace Selda.! #dbSpace_id
              Selda..&& dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
        )
        (Selda.select tableSpaceUser)
    case maybeIsMember of
      Nothing -> pure ()
      Just True -> Selda.restrict $ Selda.not_ $ Selda.isNull $ dbSpaceUser Selda.? #dbSpaceUser_id
      Just False -> Selda.restrict $ Selda.isNull $ dbSpaceUser Selda.? #dbSpaceUser_id
    Selda.restrict $
      dbSpace Selda.! #dbSpace_visibility Selda..== Selda.literal MkDbSpaceVisibility_visible
        Selda..|| Selda.not_ (Selda.isNull $ dbSpaceUser Selda.? #dbSpaceUser_id)
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
          , spaceTimezone = dbSpace_timezone space
          , spaceOwner = MkIdentifierUser $ Selda.fromId @DbUser $ dbSpace_owner space
          }
  pure $ fromDbSpace <$> dbSpaces

spaceRoleLookupId ::
  (MonadLogger m, MonadSeldaPool m) =>
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
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  SeldaTransactionT m SpaceRole
spaceRoleGet identifier = do
  lift $ logDebug $ "Get space role info with identifier: " <> T.pack (show identifier)
  dbSpaceRole <- Selda.queryOne $ roleGet $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier
  dbSpaceRolePermissions <- Selda.query $ roleListPermissions $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier
  lift $ logInfo "Got space role info successfully."
  pure
    MkSpaceRole
      { spaceRoleId = MkIdentifierSpaceRole $ Selda.fromId @DbSpaceRole $ dbSpaceRole_id dbSpaceRole
      , spaceRoleSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpaceRole_space dbSpaceRole
      , spaceRoleName = MkNameSpaceRole $ dbSpaceRole_name dbSpaceRole
      , spaceRolePermissions = S.fromList $ spacePermissionDbToApi . dbSpaceRolePermission_permission <$> dbSpaceRolePermissions
      , spaceRoleAccessibility = spaceRoleAccessibilityDbToApi $ dbSpaceRole_accessibility dbSpaceRole
      }

spaceCreate ::
  (MonadLogger m, MonadSeldaPool m) =>
  NameSpace ->
  IdentifierUser ->
  T.TZLabel ->
  VisibilitySpace ->
  SeldaTransactionT m IdentifierSpace
spaceCreate name owner timezoneLabel visibility = do
  lift $ logDebug $ "Creating space: " <> T.pack (show name)
  let dbSpace =
        MkDbSpace
          { dbSpace_id = Selda.def
          , dbSpace_name = unNameSpace name
          , dbSpace_timezone = timezoneLabel
          , dbSpace_visibility = spaceVisibilityApiToDb visibility
          , dbSpace_owner = Selda.toId @DbUser $ unIdentifierUser owner
          }
  dbSpaceId <- Selda.insertWithPK tableSpace [dbSpace]
  lift $ logInfo "Created space successfully."
  pure $ MkIdentifierSpace $ Selda.fromId @DbSpace dbSpaceId

spaceDelete ::
  (MonadLogger m, MonadSeldaPool m) =>
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
  traverse_ (spaceRoleDeleteUnsafe . MkIdentifierSpaceRole . Selda.fromId @DbSpaceRole) dbSpaceRoleIdentifiers
  lift $ logDebug $ "Space had " <> T.pack (show (length dbSpaceRoleIdentifiers)) <> " space roles."
  Selda.deleteOneFrom tableSpace $ \row ->
    row Selda.! #dbSpace_id Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
  lift $ logInfo "Deleted space successfully."
  pure ()

spaceNameSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  NameSpace ->
  SeldaTransactionT m ()
spaceNameSet identifier name = do
  lift $ logDebug $ "Setting name " <> T.pack (show name) <> " of space " <> T.pack (show identifier) <> "."
  Selda.updateOne
    tableSpace
    (#dbSpace_id `Selda.is` Selda.toId @DbSpace (unIdentifierSpace identifier))
    (\rowSpace -> rowSpace `Selda.with` [#dbSpace_name Selda.:= Selda.literal (unNameSpace name)])
  lift $ logInfo "Set name successfully."

spaceTimezoneSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  T.TZLabel ->
  SeldaTransactionT m ()
spaceTimezoneSet identifier timezone = do
  lift $ logDebug $ "Setting timezone " <> T.pack (show timezone) <> " of space " <> T.pack (show identifier) <> "."
  Selda.updateOne
    tableSpace
    (#dbSpace_id `Selda.is` Selda.toId @DbSpace (unIdentifierSpace identifier))
    (\rowSpace -> rowSpace `Selda.with` [#dbSpace_timezone Selda.:= Selda.literal timezone])
  lift $ logInfo "Set timezone successfully."

spaceVisibilitySet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  VisibilitySpace ->
  SeldaTransactionT m ()
spaceVisibilitySet identifier visibility = do
  lift $ logDebug $ "Setting visibility " <> T.pack (show visibility) <> " of space " <> T.pack (show identifier) <> "."
  Selda.updateOne
    tableSpace
    (#dbSpace_id `Selda.is` Selda.toId @DbSpace (unIdentifierSpace identifier))
    (\rowSpace -> rowSpace `Selda.with` [#dbSpace_visibility Selda.:= Selda.literal (spaceVisibilityApiToDb visibility)])
  lift $ logInfo "Set visibility successfully."

spaceUserAdd ::
  (MonadLogger m, MonadSeldaPool m) =>
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

spaceUserRemove ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m ()
spaceUserRemove spaceIdentifier userIdentifier = do
  lift $ logDebug $ "Removing user " <> T.pack (show userIdentifier) <> " from space " <> T.pack (show spaceIdentifier) <> "."
  dbSpaceUser <- Selda.queryOne $ do
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    pure dbSpaceUser
  Selda.deleteOneFrom tableSpaceUser $ \row -> row Selda.! #dbSpaceUser_id Selda..== Selda.literal (dbSpaceUser_id dbSpaceUser)
  lift $ logInfo "Removed space-user successfully."
  pure ()

spaceUserRoleEdit ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  IdentifierSpaceRole ->
  SeldaTransactionT m ()
spaceUserRoleEdit spaceIdentifier userIdentifier roleIdentifier = do
  lift $ logDebug $ "Setting new role " <> T.pack (show roleIdentifier) <> " for user " <> T.pack (show userIdentifier) <> " from space " <> T.pack (show spaceIdentifier) <> "."
  lift $ logDebug "Ensuring that the role is of the right space."
  roleBelongsToSpace <- do
    dbSpaceRole <- Selda.queryOne $ do
      roleGet $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole roleIdentifier
    let spaceRoleSpaceId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpaceRole_space dbSpaceRole
    pure $ spaceRoleSpaceId == spaceIdentifier
  unless roleBelongsToSpace undefined -- TODO: Use an actual Exception type.
  lift $ logDebug "Updating the role."
  dbSpaceUser <- Selda.queryOne $ do
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    pure dbSpaceUser
  Selda.updateOne
    tableSpaceUser
    (#dbSpaceUser_id `Selda.is` dbSpaceUser_id dbSpaceUser)
    (\rowSpaceUser -> rowSpaceUser `Selda.with` [#dbSpaceUser_role Selda.:= Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole roleIdentifier)])
  lift $ logInfo "Set new role for space-user successfully."
  pure ()

spaceUserIsOwner ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  SeldaTransactionT m Bool
spaceUserIsOwner spaceIdentifier userIdentifier = do
  lift $ logDebug $ "Looking up if user " <> T.pack (show userIdentifier) <> " is owner of space " <> T.pack (show spaceIdentifier) <> "."
  space <- spaceGetFromId spaceIdentifier
  lift $ logInfo "Looked up space ownership successfully."
  pure $ spaceOwner space == userIdentifier

spaceUserPermissions ::
  (MonadLogger m, MonadSeldaPool m) =>
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
  Maybe Password ->
  SeldaTransactionT m IdentifierSpaceRole
spaceRoleCreate spaceIdentifier roleName accessibility password = do
  lift $ logDebug $ "Creating role: " <> T.pack (show (spaceIdentifier, roleName))
  maybePasswordHash :: Maybe (PasswordHash Bcrypt) <- do
    lift $ logDebug "Confirming that accessibility and password_hash match."
    let accessibilityMatchesPassword =
          case accessibility of
            MkAccessibilitySpaceRoleJoinable -> isNothing password
            MkAccessibilitySpaceRoleJoinableWithPassword -> isJust password
            MkAccessibilitySpaceRoleInaccessible -> isNothing password
    if accessibilityMatchesPassword
      then lift $ traverse hashPassword password
      else throwM MkSqlErrorMensamSpaceRoleAccessibilityAndPasswordDontMatch
  let dbSpaceRole =
        MkDbSpaceRole
          { dbSpaceRole_id = Selda.def
          , dbSpaceRole_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbSpaceRole_name = unNameSpaceRole roleName
          , dbSpaceRole_accessibility = spaceRoleAccessibilityApiToDb accessibility
          , dbSpaceRole_password_hash = unPasswordHash <$> maybePasswordHash
          }
  lift $ logDebug "Inserting space-role into database."
  dbSpaceRoleId <- Selda.insertWithPK tableSpaceRole [dbSpaceRole]
  lift $ logInfo "Created role successfully."
  pure $ MkIdentifierSpaceRole $ Selda.fromId @DbSpaceRole dbSpaceRoleId

type SqlErrorMensamSpaceRoleAccessibilityAndPasswordDontMatch :: Type
data SqlErrorMensamSpaceRoleAccessibilityAndPasswordDontMatch = MkSqlErrorMensamSpaceRoleAccessibilityAndPasswordDontMatch
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

spaceRoleDeleteUnsafe ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  SeldaTransactionT m ()
spaceRoleDeleteUnsafe identifier = do
  lift $ logDebug $ "Deleting space role: " <> T.pack (show identifier)
  countPermissions <- Selda.deleteFrom tableSpaceRolePermission $ \row ->
    row Selda.! #dbSpaceRolePermission_role Selda..== Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier)
  lift $ logDebug $ "Role had " <> T.pack (show countPermissions) <> " permissions."
  Selda.deleteOneFrom tableSpaceRole $ \row ->
    row Selda.! #dbSpaceRole_id Selda..== Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier)
  lift $ logInfo "Deleted space role successfully."
  pure ()

spaceRoleDeleteWithFallback ::
  (MonadLogger m, MonadSeldaPool m) =>
  -- | to be deleted
  IdentifierSpaceRole ->
  -- | fallback
  IdentifierSpaceRole ->
  SeldaTransactionT m ()
spaceRoleDeleteWithFallback identifierToDelete identifierFallback = do
  lift $ logDebug $ "Deleting space role " <> T.pack (show identifierToDelete) <> " with fallback " <> T.pack (show identifierFallback) <> "."
  lift $ logInfo "Making sure that the fallback role is of the same space."
  if identifierToDelete /= identifierFallback
    then lift $ logInfo "The fallback role is different from the role that will be deleted."
    else error "Fallback is the same."
  dbSpaceRoleToDelete <- Selda.queryOne $ roleGet $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifierToDelete
  dbSpaceRoleFallback <- Selda.queryOne $ roleGet $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifierFallback
  if dbSpaceRole_space dbSpaceRoleToDelete == dbSpaceRole_space dbSpaceRoleFallback
    then lift $ logInfo "The fallback role is of the same space."
    else error "Fallback role is not of the same space"
  countFallbackSpaceUsers <-
    Selda.update
      tableSpaceUser
      (\dbSpaceUser -> dbSpaceUser Selda.! #dbSpaceUser_role Selda..== Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifierToDelete))
      (\dbSpaceUser -> dbSpaceUser `Selda.with` [#dbSpaceUser_role Selda.:= Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifierFallback)])
  lift $ logDebug $ "The amount of users that now use the fallback role: " <> T.pack (show countFallbackSpaceUsers)
  spaceRoleDeleteUnsafe identifierToDelete
  pure ()

spaceRolePermissionGive ::
  (MonadLogger m, MonadSeldaPool m) =>
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

-- | Just checks that the password matches.
-- Does not check the accessibility of the role.
spaceRolePasswordCheck ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  Maybe Password ->
  SeldaTransactionT m PasswordCheck
spaceRolePasswordCheck identifier maybePassword = do
  lift $ logDebug $ "Querying space_role " <> T.pack (show identifier) <> " from database to check password."
  dbSpaceRole <- Selda.queryOne $ roleGet $ Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier
  case PasswordHash <$> dbSpaceRole_password_hash dbSpaceRole of
    Nothing -> do
      case maybePassword of
        Nothing -> do
          lift $ logInfo "No password has been set. Nothing to check."
          pure PasswordCheckSuccess
        Just _ -> do
          lift $ logWarn "Tried to enter a password even though there is no password set up."
          throwM MkSqlErrorMensamSpaceRoleNoPasswordSetCannotCheck
    Just passwordHash -> do
      case maybePassword of
        Nothing -> do
          lift $ logInfo "Didn't enter a password even though a password is required."
          pure PasswordCheckFail
        Just password -> do
          lift $ logDebug "Comparing password hashes."
          let passwordCheck = checkPassword password passwordHash
          case passwordCheck of
            PasswordCheckSuccess ->
              lift $ logInfo "Password matches. Check successful."
            PasswordCheckFail ->
              lift $ logInfo "Password does not matches. Check failed."
          pure passwordCheck

type SqlErrorMensamSpaceRoleNoPasswordSetCannotCheck :: Type
data SqlErrorMensamSpaceRoleNoPasswordSetCannotCheck = MkSqlErrorMensamSpaceRoleNoPasswordSetCannotCheck
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

-- | Fails the transaction when the password check fails.
spaceRolePasswordCheck' ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  Maybe Password ->
  SeldaTransactionT m ()
spaceRolePasswordCheck' identifier maybePassword =
  spaceRolePasswordCheck identifier maybePassword >>= \case
    PasswordCheckSuccess -> pure ()
    PasswordCheckFail -> do
      lift $ logDebug "Abort transaction after failed space_role password check."
      throwM MkSqlErrorMensamSpaceRolePasswordCheckFail

type SqlErrorMensamSpaceRolePasswordCheckFail :: Type
data SqlErrorMensamSpaceRolePasswordCheckFail = MkSqlErrorMensamSpaceRolePasswordCheckFail
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

type SqlErrorMensamSpaceRoleInaccessible :: Type
data SqlErrorMensamSpaceRoleInaccessible = MkSqlErrorMensamSpaceRoleInaccessible
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

spaceRoleNameSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  NameSpaceRole ->
  SeldaTransactionT m ()
spaceRoleNameSet identifier name = do
  lift $ logDebug $ "Setting name " <> T.pack (show name) <> " of role " <> T.pack (show identifier) <> "."
  Selda.updateOne
    tableSpaceRole
    (#dbSpaceRole_id `Selda.is` Selda.toId @DbSpaceRole (unIdentifierSpaceRole identifier))
    (\rowSpaceRole -> rowSpaceRole `Selda.with` [#dbSpaceRole_name Selda.:= Selda.literal (unNameSpaceRole name)])
  lift $ logInfo "Set new role name successfully."

spaceRoleAccessibilityAndPasswordSet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  AccessibilitySpaceRole ->
  Maybe Password ->
  SeldaTransactionT m ()
spaceRoleAccessibilityAndPasswordSet identifier accessibility password = do
  lift $ logDebug $ "Setting accessibility " <> T.pack (show accessibility) <> " of role " <> T.pack (show identifier) <> "."
  maybePasswordHash :: Maybe (PasswordHash Bcrypt) <- do
    lift $ logDebug "Confirming that accessibility and password_hash match."
    let accessibilityMatchesPassword =
          case accessibility of
            MkAccessibilitySpaceRoleJoinable -> isNothing password
            MkAccessibilitySpaceRoleJoinableWithPassword -> isJust password
            MkAccessibilitySpaceRoleInaccessible -> isNothing password
    if accessibilityMatchesPassword
      then lift $ traverse hashPassword password
      else throwM MkSqlErrorMensamSpaceRoleAccessibilityAndPasswordDontMatch
  Selda.updateOne
    tableSpaceRole
    (#dbSpaceRole_id `Selda.is` Selda.toId @DbSpaceRole (unIdentifierSpaceRole identifier))
    (\rowSpaceRole -> rowSpaceRole `Selda.with` [#dbSpaceRole_accessibility Selda.:= Selda.literal (spaceRoleAccessibilityApiToDb accessibility)])
  Selda.updateOne
    tableSpaceRole
    (#dbSpaceRole_id `Selda.is` Selda.toId @DbSpaceRole (unIdentifierSpaceRole identifier))
    (\rowSpaceRole -> rowSpaceRole `Selda.with` [#dbSpaceRole_password_hash Selda.:= Selda.literal (unPasswordHash <$> maybePasswordHash)])
  lift $ logInfo "Set new role accessibility successfully."

spaceRolePermissionsSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpaceRole ->
  S.Set PermissionSpace ->
  SeldaTransactionT m ()
spaceRolePermissionsSet identifier permissions = do
  lift $ logDebug $ "Setting permissions " <> T.pack (show permissions) <> " of role " <> T.pack (show identifier) <> "."
  countPermissionsDeleted <- Selda.deleteFrom tableSpaceRolePermission $ \row ->
    row Selda.! #dbSpaceRolePermission_role Selda..== Selda.literal (Selda.toId @DbSpaceRole $ unIdentifierSpaceRole identifier)
  lift $ logDebug $ "Deleted old permissions: " <> T.pack (show countPermissionsDeleted)
  countPermissionsGiven <- L.length <$> traverse (spaceRolePermissionGive identifier) (S.toList permissions)
  lift $ logDebug $ "Gave new permissions: " <> T.pack (show countPermissionsGiven)
  lift $ logInfo "Set new role permissions successfully."

deskLookupId ::
  (MonadLogger m, MonadSeldaPool m) =>
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

deskGetFromId ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  SeldaTransactionT m Desk
deskGetFromId identifier = do
  lift $ logDebug $ "Get desk info with identifier: " <> T.pack (show identifier)
  dbDesk <- catch
    ( Selda.queryOne $ do
        dbDesk <- Selda.select tableDesk
        Selda.restrict $ dbDesk Selda.! #dbDesk_id Selda..== Selda.literal (Selda.toId @DbDesk $ unIdentifierDesk identifier)
        pure dbDesk
    )
    $ \case exc -> throwM $ MkSqlErrorMensamDeskNotFound exc
  lift $ logInfo "Got desk info successfully."
  pure
    MkDesk
      { deskId = MkIdentifierDesk $ Selda.fromId $ dbDesk_id dbDesk
      , deskSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbDesk_space dbDesk
      , deskName = MkNameDesk $ dbDesk_name dbDesk
      }

type SqlErrorMensamDeskNotFound :: Type
newtype SqlErrorMensamDeskNotFound = MkSqlErrorMensamDeskNotFound Selda.SqlErrorMensamNotOneQuery
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

deskList ::
  (MonadLogger m, MonadSeldaPool m) =>
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
  (MonadLogger m, MonadSeldaPool m) =>
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
  (MonadLogger m, MonadSeldaPool m) =>
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

deskNameSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  NameDesk ->
  SeldaTransactionT m ()
deskNameSet identifier name = do
  lift $ logDebug $ "Setting name " <> T.pack (show name) <> " of desk " <> T.pack (show identifier) <> "."
  Selda.updateOne
    tableDesk
    (#dbDesk_id `Selda.is` Selda.toId @DbDesk (unIdentifierDesk identifier))
    (\rowDesk -> rowDesk `Selda.with` [#dbDesk_name Selda.:= Selda.literal (unNameDesk name)])
  lift $ logInfo "Set desk name successfully."

reservationGet ::
  (MonadLogger m, MonadSeldaPool m) =>
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
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  -- | minimum timestamp begin
  Maybe T.UTCTime ->
  -- | maximum timestamp end
  Maybe T.UTCTime ->
  SeldaTransactionT m [Reservation]
reservationList deskIdentifier maybeTimestampBegin maybeTimestampEnd = do
  lift $ logDebug $ "Looking up desk's reservations: " <> T.pack (show (deskIdentifier, maybeTimestampBegin, maybeTimestampEnd))
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
  lift $ logInfo "Looked up desk's reservations successfully."
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

-- | List all reservations of a user, that overlap with the given time window.
reservationListUser ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierUser ->
  -- | minimum timestamp begin
  Maybe T.UTCTime ->
  -- | maximum timestamp end
  Maybe T.UTCTime ->
  SeldaTransactionT m [Reservation]
reservationListUser userIdentifier maybeTimestampBegin maybeTimestampEnd = do
  lift $ logDebug $ "Looking up user's reservations: " <> T.pack (show (userIdentifier, maybeTimestampBegin, maybeTimestampEnd))
  dbReservations <- Selda.query $ do
    dbReservation <- Selda.select tableReservation
    Selda.restrict $ dbReservation Selda.! #dbReservation_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    -- TODO: Improve overlapping behaviour (OR instead of AND?).
    case maybeTimestampBegin of
      Nothing -> pure ()
      Just timestampBegin ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_end Selda..> Selda.literal timestampBegin
    case maybeTimestampEnd of
      Nothing -> pure ()
      Just timestampEnd ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_begin Selda..< Selda.literal timestampEnd
    pure dbReservation
  lift $ logInfo "Looked up user's reservations successfully."
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
  (MonadLogger m, MonadSeldaPool m) =>
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
  reservation <- reservationGet reservationIdentifier
  case reservationStatus reservation of
    MkStatusReservationCancelled -> do
      lift $ logDebug "Reservation is already cancelled."
      throwM MkSqlErrorMensamReservationAlreadyCancelled
    MkStatusReservationPlanned ->
      lift $ logDebug "Reservation is currently still planned."
  timeCurrent <- lift $ liftIO T.getCurrentTime
  if timeCurrent >= reservationTimeBegin reservation
    then do
      lift $ logDebug "Reservation has already started."
      throwM MkSqlErrorMensamReservationIsInThePast
    else lift $ logDebug "Reservation is set for a time in the future."
  Selda.update_
    tableReservation
    (\dbReservation -> dbReservation Selda.! #dbReservation_id Selda..== Selda.literal (Selda.toId @DbReservation $ unIdentifierReservation reservationIdentifier))
    (\dbReservation -> dbReservation `Selda.with` [#dbReservation_status Selda.:= Selda.literal MkDbReservationStatus_cancelled])
  lift $ logInfo "Cancelled reservation successfully."

type SqlErrorMensamReservationAlreadyCancelled :: Type
data SqlErrorMensamReservationAlreadyCancelled = MkSqlErrorMensamReservationAlreadyCancelled
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

type SqlErrorMensamReservationIsInThePast :: Type
data SqlErrorMensamReservationIsInThePast = MkSqlErrorMensamReservationIsInThePast
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

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
  MkAccessibilitySpaceRoleJoinableWithPassword -> MkDbSpaceRoleAccessibility_joinable_with_password
  MkAccessibilitySpaceRoleInaccessible -> MkDbSpaceRoleAccessibility_inaccessible

spaceRoleAccessibilityDbToApi :: DbSpaceRoleAccessibility -> AccessibilitySpaceRole
spaceRoleAccessibilityDbToApi = \case
  MkDbSpaceRoleAccessibility_joinable -> MkAccessibilitySpaceRoleJoinable
  MkDbSpaceRoleAccessibility_joinable_with_password -> MkAccessibilitySpaceRoleJoinableWithPassword
  MkDbSpaceRoleAccessibility_inaccessible -> MkAccessibilitySpaceRoleInaccessible

spacePermissionApiToDb :: PermissionSpace -> DbSpacePermission
spacePermissionApiToDb = \case
  MkPermissionSpaceViewSpace -> MkDbSpacePermission_view_space
  MkPermissionSpaceEditDesk -> MkDbSpacePermission_edit_desk
  MkPermissionSpaceEditRole -> MkDbSpacePermission_edit_role
  MkPermissionSpaceEditSpace -> MkDbSpacePermission_edit_space
  MkPermissionSpaceCreateReservation -> MkDbSpacePermission_create_reservation
  MkPermissionSpaceCancelReservation -> MkDbSpacePermission_cancel_reservation

spacePermissionDbToApi :: DbSpacePermission -> PermissionSpace
spacePermissionDbToApi = \case
  MkDbSpacePermission_view_space -> MkPermissionSpaceViewSpace
  MkDbSpacePermission_edit_desk -> MkPermissionSpaceEditDesk
  MkDbSpacePermission_edit_role -> MkPermissionSpaceEditRole
  MkDbSpacePermission_edit_space -> MkPermissionSpaceEditSpace
  MkDbSpacePermission_create_reservation -> MkPermissionSpaceCreateReservation
  MkDbSpacePermission_cancel_reservation -> MkPermissionSpaceCancelReservation
