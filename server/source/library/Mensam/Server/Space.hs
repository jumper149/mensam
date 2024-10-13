{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Space where

import Mensam.API.Data.Desk
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
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.Kind
import Data.List qualified as L
import Data.Maybe
import Data.Password.Bcrypt
import Data.Set qualified as S
import Data.Singletons
import Data.Text qualified as T
import Data.Time.Zones.All qualified as T
import Data.Typeable
import Database.Selda qualified as Selda
import GHC.Generics
import Mensam.Server.Jpeg
import Numeric.Natural

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
      lift $ logDebug "Getting roles."
      roles <- do
        dbRoles <- Selda.query $ spaceListRoles $ dbSpace_id dbSpace
        dbRolesPermissions <- traverse (Selda.query . spaceRoleListPermissions . dbRole_id) dbRoles
        let roles =
              zipWith
                ( \dbRole dbRolePermissions ->
                    MkRole
                      { roleId = MkIdentifierRole $ Selda.fromId @DbRole $ dbRole_id dbRole
                      , roleSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbRole_space dbRole
                      , roleName = MkNameRole $ dbRole_name dbRole
                      , rolePermissions = S.fromList $ spacePermissionDbToApi . dbRolePermission_permission <$> dbRolePermissions
                      , roleAccessibility = roleAccessibilityDbToApi $ dbRole_accessibility dbRole
                      }
                )
                dbRoles
                dbRolesPermissions
        pure roles
      dbSpaceUsers <- Selda.query $ spaceListUsers $ dbSpace_id dbSpace
      let spaceUsers =
            ( \dbSpaceUser ->
                MkSpaceUser
                  { spaceUserUser = MkIdentifierUser $ Selda.fromId @DbUser $ dbSpaceUser_user dbSpaceUser
                  , spaceUserRole = MkIdentifierRole $ Selda.fromId @DbRole $ dbSpaceUser_role dbSpaceUser
                  }
            )
              <$> dbSpaceUsers
      lift $ logInfo "Got roles successfully."
      lift $ logDebug "Looking up role for requesting user."
      maybeRoleIdentifier <- do
        dbMaybeRoleId <-
          Selda.queryUnique $
            spaceUserGetRole
              (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
              (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
        pure $ MkIdentifierRole . Selda.fromId @DbRole <$> dbMaybeRoleId
      lift $ logInfo "Got requesting user's role successfully."
      pure $
        Just
          MkSpaceView
            { spaceViewId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbSpace_id dbSpace
            , spaceViewName = MkNameSpace $ dbSpace_name dbSpace
            , spaceViewTimezone = dbSpace_timezone dbSpace
            , spaceViewVisibility = spaceVisibilityDbToApi $ dbSpace_visibility dbSpace
            , spaceViewOwner = MkIdentifierUser $ Selda.fromId @DbUser $ dbSpace_owner dbSpace
            , spaceViewRoles = S.fromList roles
            , spaceViewUsers = S.fromList spaceUsers
            , spaceViewYourRole = maybeRoleIdentifier
            }

type SpaceView :: Type
data SpaceView = MkSpaceView
  { spaceViewId :: IdentifierSpace
  , spaceViewName :: NameSpace
  , spaceViewTimezone :: T.TZLabel
  , spaceViewVisibility :: VisibilitySpace
  , spaceViewOwner :: IdentifierUser
  , spaceViewRoles :: S.Set Role
  , spaceViewUsers :: S.Set SpaceUser
  , spaceViewYourRole :: Maybe IdentifierRole
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

spaceCountUsers ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  SeldaTransactionT m Natural
spaceCountUsers identifier = do
  lift $ logDebug $ "Counting space users: " <> T.pack (show identifier)
  dbUserCount <- Selda.queryOne $ do
    Selda.aggregate $ do
      dbSpaceUser <- Selda.select tableSpaceUser
      Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
      pure $ Selda.count $ dbSpaceUser Selda.! #dbSpaceUser_id
  let intToNatural :: Int -> Natural = toEnum
  pure $ intToNatural dbUserCount

spaceCountDesks ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  SeldaTransactionT m Natural
spaceCountDesks identifier = do
  lift $ logDebug $ "Counting space desks: " <> T.pack (show identifier)
  dbDeskCount <- Selda.queryOne $ do
    Selda.aggregate $ do
      dbDesk <- Selda.select tableDesk
      Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
      pure $ Selda.count $ dbDesk Selda.! #dbDesk_id
  let intToNatural :: Int -> Natural = toEnum
  pure $ intToNatural dbDeskCount

roleLookupId ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  NameRole ->
  SeldaTransactionT m (Maybe IdentifierRole)
roleLookupId spaceIdentifier name = do
  lift $ logDebug $ "Looking up space-role identifier with name: " <> T.pack (show name)
  maybeDbRole <- Selda.queryUnique $ spaceRoleLookup (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier) $ unNameRole name
  case maybeDbRole of
    Nothing -> do
      lift $ logWarn $ "Failed to look up space-role. Name doesn't exist: " <> T.pack (show name)
      pure Nothing
    Just dbRole -> do
      lift $ logInfo "Looked up space successfully."
      pure $ Just $ MkIdentifierRole $ Selda.fromId $ dbRole_id dbRole

roleGet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  SeldaTransactionT m Role
roleGet identifier = do
  lift $ logDebug $ "Get role info with identifier: " <> T.pack (show identifier)
  dbRole <- Selda.queryOne $ spaceRoleGet $ Selda.toId @DbRole $ unIdentifierRole identifier
  dbRolePermissions <- Selda.query $ spaceRoleListPermissions $ Selda.toId @DbRole $ unIdentifierRole identifier
  lift $ logInfo "Got role info successfully."
  pure
    MkRole
      { roleId = MkIdentifierRole $ Selda.fromId @DbRole $ dbRole_id dbRole
      , roleSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbRole_space dbRole
      , roleName = MkNameRole $ dbRole_name dbRole
      , rolePermissions = S.fromList $ spacePermissionDbToApi . dbRolePermission_permission <$> dbRolePermissions
      , roleAccessibility = roleAccessibilityDbToApi $ dbRole_accessibility dbRole
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
          , dbSpace_picture_jpeg = Nothing
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
  dbRoleIdentifiers <- Selda.query $ do
    dbDesk <- Selda.select tableRole
    Selda.restrict $ dbDesk Selda.! #dbRole_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
    pure $ dbDesk Selda.! #dbRole_id
  lift $ logDebug "Deleting roles."
  traverse_ (roleDeleteUnsafe . MkIdentifierRole . Selda.fromId @DbRole) dbRoleIdentifiers
  lift $ logDebug $ "Space had " <> T.pack (show (length dbRoleIdentifiers)) <> " roles."
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

spaceSetPicture ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  Maybe ByteStringJpeg ->
  SeldaTransactionT m ()
spaceSetPicture identifier picture = do
  lift $ logDebug "Set new space picture."
  Selda.updateOne
    tableSpace
    (#dbSpace_id `Selda.is` Selda.toId @DbSpace (unIdentifierSpace identifier))
    (`Selda.with` [#dbSpace_picture_jpeg Selda.:= Selda.literal (BL.toStrict . unByteStringJpeg <$> picture)])
  lift $ logInfo "Set new space picture successfully."

spaceGetPicture ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  SeldaTransactionT m (Maybe ByteStringJpeg)
spaceGetPicture identifier = do
  lift $ logDebug "Get space picture."
  picture <- Selda.queryOne $ do
    space <- Selda.select tableSpace
    Selda.restrict $ space Selda.! #dbSpace_id Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace identifier)
    pure $ space Selda.! #dbSpace_picture_jpeg
  lift $ logInfo "Got space picture successfully."
  pure $ MkByteStringJpegUnsafe . BL.fromStrict <$> picture

spaceUserAdd ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  IdentifierUser ->
  IdentifierRole ->
  SeldaTransactionT m ()
spaceUserAdd spaceIdentifier userIdentifier roleIdentifier = do
  lift $ logDebug $ "Adding user " <> T.pack (show userIdentifier) <> " to space " <> T.pack (show spaceIdentifier) <> " as " <> T.pack (show roleIdentifier) <> "."
  lift $ logDebug "Ensuring that the role is of the right space."
  roleBelongsToSpace <- do
    dbRole <- Selda.queryOne $ do
      spaceRoleGet $ Selda.toId @DbRole $ unIdentifierRole roleIdentifier
    let roleSpaceId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbRole_space dbRole
    pure $ roleSpaceId == spaceIdentifier
  unless roleBelongsToSpace undefined -- TODO: Use an actual Exception type.
  let dbSpaceUser =
        MkDbSpaceUser
          { dbSpaceUser_id = Selda.def
          , dbSpaceUser_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbSpaceUser_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbSpaceUser_role = Selda.toId @DbRole $ unIdentifierRole roleIdentifier
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
  IdentifierRole ->
  SeldaTransactionT m ()
spaceUserRoleEdit spaceIdentifier userIdentifier roleIdentifier = do
  lift $ logDebug $ "Setting new role " <> T.pack (show roleIdentifier) <> " for user " <> T.pack (show userIdentifier) <> " from space " <> T.pack (show spaceIdentifier) <> "."
  lift $ logDebug "Ensuring that the role is of the right space."
  roleBelongsToSpace <- do
    dbRole <- Selda.queryOne $ do
      spaceRoleGet $ Selda.toId @DbRole $ unIdentifierRole roleIdentifier
    let roleSpaceId = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbRole_space dbRole
    pure $ roleSpaceId == spaceIdentifier
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
    (\rowSpaceUser -> rowSpaceUser `Selda.with` [#dbSpaceUser_role Selda.:= Selda.literal (Selda.toId @DbRole $ unIdentifierRole roleIdentifier)])
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
  pure $ S.fromList $ spacePermissionDbToApi . dbRolePermission_permission <$> permissions

roleCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierSpace ->
  NameRole ->
  AccessibilityRole ->
  Maybe Password ->
  SeldaTransactionT m IdentifierRole
roleCreate spaceIdentifier roleName accessibility password = do
  lift $ logDebug $ "Creating role: " <> T.pack (show (spaceIdentifier, roleName))
  maybePasswordHash :: Maybe (PasswordHash Bcrypt) <- do
    lift $ logDebug "Confirming that accessibility and password_hash match."
    let accessibilityMatchesPassword =
          case accessibility of
            MkAccessibilityRoleJoinable -> isNothing password
            MkAccessibilityRoleJoinableWithPassword -> isJust password
            MkAccessibilityRoleInaccessible -> isNothing password
    if accessibilityMatchesPassword
      then lift $ traverse hashPassword password
      else throwM MkSqlErrorMensamRoleAccessibilityAndPasswordDontMatch
  let dbRole =
        MkDbRole
          { dbRole_id = Selda.def
          , dbRole_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbRole_name = unNameRole roleName
          , dbRole_accessibility = roleAccessibilityApiToDb accessibility
          , dbRole_password_hash = unPasswordHash <$> maybePasswordHash
          }
  lift $ logDebug "Inserting space-role into database."
  dbRoleId <- Selda.insertWithPK tableRole [dbRole]
  lift $ logInfo "Created role successfully."
  pure $ MkIdentifierRole $ Selda.fromId @DbRole dbRoleId

type SqlErrorMensamRoleAccessibilityAndPasswordDontMatch :: Type
data SqlErrorMensamRoleAccessibilityAndPasswordDontMatch = MkSqlErrorMensamRoleAccessibilityAndPasswordDontMatch
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

roleDeleteUnsafe ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  SeldaTransactionT m ()
roleDeleteUnsafe identifier = do
  lift $ logDebug $ "Deleting role: " <> T.pack (show identifier)
  countPermissions <- Selda.deleteFrom tableRolePermission $ \row ->
    row Selda.! #dbRolePermission_role Selda..== Selda.literal (Selda.toId @DbRole $ unIdentifierRole identifier)
  lift $ logDebug $ "Role had " <> T.pack (show countPermissions) <> " permissions."
  Selda.deleteOneFrom tableRole $ \row ->
    row Selda.! #dbRole_id Selda..== Selda.literal (Selda.toId @DbRole $ unIdentifierRole identifier)
  lift $ logInfo "Deleted role successfully."
  pure ()

roleDeleteWithFallback ::
  (MonadLogger m, MonadSeldaPool m) =>
  -- | to be deleted
  IdentifierRole ->
  -- | fallback
  IdentifierRole ->
  SeldaTransactionT m ()
roleDeleteWithFallback identifierToDelete identifierFallback = do
  lift $ logDebug $ "Deleting role " <> T.pack (show identifierToDelete) <> " with fallback " <> T.pack (show identifierFallback) <> "."
  lift $ logInfo "Making sure that the fallback role is of the same space."
  if identifierToDelete /= identifierFallback
    then lift $ logInfo "The fallback role is different from the role that will be deleted."
    else error "Fallback is the same."
  dbRoleToDelete <- Selda.queryOne $ spaceRoleGet $ Selda.toId @DbRole $ unIdentifierRole identifierToDelete
  dbRoleFallback <- Selda.queryOne $ spaceRoleGet $ Selda.toId @DbRole $ unIdentifierRole identifierFallback
  if dbRole_space dbRoleToDelete == dbRole_space dbRoleFallback
    then lift $ logInfo "The fallback role is of the same space."
    else error "Fallback role is not of the same space"
  countFallbackSpaceUsers <-
    Selda.update
      tableSpaceUser
      (\dbSpaceUser -> dbSpaceUser Selda.! #dbSpaceUser_role Selda..== Selda.literal (Selda.toId @DbRole $ unIdentifierRole identifierToDelete))
      (\dbSpaceUser -> dbSpaceUser `Selda.with` [#dbSpaceUser_role Selda.:= Selda.literal (Selda.toId @DbRole $ unIdentifierRole identifierFallback)])
  lift $ logDebug $ "The amount of users that now use the fallback role: " <> T.pack (show countFallbackSpaceUsers)
  roleDeleteUnsafe identifierToDelete
  pure ()

rolePermissionGive ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  PermissionSpace ->
  SeldaTransactionT m ()
rolePermissionGive roleIdentifier permission = do
  lift $ logDebug $ "Giving role " <> T.pack (show roleIdentifier) <> " permission " <> T.pack (show permission) <> "."
  let dbRolePermission =
        MkDbRolePermission
          { dbRolePermission_id = Selda.def
          , dbRolePermission_role = Selda.toId @DbRole $ unIdentifierRole roleIdentifier
          , dbRolePermission_permission = spacePermissionApiToDb permission
          }
  lift $ logDebug "Inserting space-role permission."
  Selda.insert_ tableRolePermission [dbRolePermission]
  lift $ logInfo "Gave space-role a permission successfully."

-- | Just checks that the password matches.
-- Does not check the accessibility of the role.
rolePasswordCheck ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  Maybe Password ->
  SeldaTransactionT m PasswordCheck
rolePasswordCheck identifier maybePassword = do
  lift $ logDebug $ "Querying space_role " <> T.pack (show identifier) <> " from database to check password."
  dbRole <- Selda.queryOne $ spaceRoleGet $ Selda.toId @DbRole $ unIdentifierRole identifier
  case PasswordHash <$> dbRole_password_hash dbRole of
    Nothing -> do
      case maybePassword of
        Nothing -> do
          lift $ logInfo "No password has been set. Nothing to check."
          pure PasswordCheckSuccess
        Just _ -> do
          lift $ logWarn "Tried to enter a password even though there is no password set up."
          throwM MkSqlErrorMensamRoleNoPasswordSetCannotCheck
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

type SqlErrorMensamRoleNoPasswordSetCannotCheck :: Type
data SqlErrorMensamRoleNoPasswordSetCannotCheck = MkSqlErrorMensamRoleNoPasswordSetCannotCheck
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

-- | Fails the transaction when the password check fails.
rolePasswordCheck' ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  Maybe Password ->
  SeldaTransactionT m ()
rolePasswordCheck' identifier maybePassword =
  rolePasswordCheck identifier maybePassword >>= \case
    PasswordCheckSuccess -> pure ()
    PasswordCheckFail -> do
      lift $ logDebug "Abort transaction after failed space_role password check."
      throwM MkSqlErrorMensamRolePasswordCheckFail

type SqlErrorMensamRolePasswordCheckFail :: Type
data SqlErrorMensamRolePasswordCheckFail = MkSqlErrorMensamRolePasswordCheckFail
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

type SqlErrorMensamRoleInaccessible :: Type
data SqlErrorMensamRoleInaccessible = MkSqlErrorMensamRoleInaccessible
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)

roleNameSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  NameRole ->
  SeldaTransactionT m ()
roleNameSet identifier name = do
  lift $ logDebug $ "Setting name " <> T.pack (show name) <> " of role " <> T.pack (show identifier) <> "."
  Selda.updateOne
    tableRole
    (#dbRole_id `Selda.is` Selda.toId @DbRole (unIdentifierRole identifier))
    (\rowRole -> rowRole `Selda.with` [#dbRole_name Selda.:= Selda.literal (unNameRole name)])
  lift $ logInfo "Set new role name successfully."

roleAccessibilityAndPasswordSet ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  AccessibilityRole ->
  Maybe Password ->
  SeldaTransactionT m ()
roleAccessibilityAndPasswordSet identifier accessibility password = do
  lift $ logDebug $ "Setting accessibility " <> T.pack (show accessibility) <> " of role " <> T.pack (show identifier) <> "."
  maybePasswordHash :: Maybe (PasswordHash Bcrypt) <- do
    lift $ logDebug "Confirming that accessibility and password_hash match."
    let accessibilityMatchesPassword =
          case accessibility of
            MkAccessibilityRoleJoinable -> isNothing password
            MkAccessibilityRoleJoinableWithPassword -> isJust password
            MkAccessibilityRoleInaccessible -> isNothing password
    if accessibilityMatchesPassword
      then lift $ traverse hashPassword password
      else throwM MkSqlErrorMensamRoleAccessibilityAndPasswordDontMatch
  Selda.updateOne
    tableRole
    (#dbRole_id `Selda.is` Selda.toId @DbRole (unIdentifierRole identifier))
    (\rowRole -> rowRole `Selda.with` [#dbRole_accessibility Selda.:= Selda.literal (roleAccessibilityApiToDb accessibility)])
  Selda.updateOne
    tableRole
    (#dbRole_id `Selda.is` Selda.toId @DbRole (unIdentifierRole identifier))
    (\rowRole -> rowRole `Selda.with` [#dbRole_password_hash Selda.:= Selda.literal (unPasswordHash <$> maybePasswordHash)])
  lift $ logInfo "Set new role accessibility successfully."

rolePermissionsSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierRole ->
  S.Set PermissionSpace ->
  SeldaTransactionT m ()
rolePermissionsSet identifier permissions = do
  lift $ logDebug $ "Setting permissions " <> T.pack (show permissions) <> " of role " <> T.pack (show identifier) <> "."
  countPermissionsDeleted <- Selda.deleteFrom tableRolePermission $ \row ->
    row Selda.! #dbRolePermission_role Selda..== Selda.literal (Selda.toId @DbRole $ unIdentifierRole identifier)
  lift $ logDebug $ "Deleted old permissions: " <> T.pack (show countPermissionsDeleted)
  countPermissionsGiven <- L.length <$> traverse (rolePermissionGive identifier) (S.toList permissions)
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
      , deskLocation = do
          x <- dbDesk_position_x dbDesk
          y <- dbDesk_position_y dbDesk
          direction <- dbDesk_direction dbDesk
          width <- dbDesk_size_width dbDesk
          depth <- dbDesk_size_depth dbDesk
          pure
            MkLocationDesk
              { locationDeskPosition =
                  MkPositionDesk
                    { positionDeskX = MkConstrainedDoubleUnsafe x
                    , positionDeskY = MkConstrainedDoubleUnsafe y
                    }
              , locationDeskDirection =
                  MkDirectionDesk
                    { unDirectionDesk = MkDirectionDegrees $ MkConstrainedDoubleUnsafe direction
                    }
              , locationDeskSize =
                  MkSizeDesk
                    { sizeDeskWidth = MkConstrainedDoubleUnsafe width
                    , sizeDeskDepth = MkConstrainedDoubleUnsafe depth
                    }
              }
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
  let fromDbDesk dbDesk =
        MkDesk
          { deskId = MkIdentifierDesk $ Selda.fromId @DbDesk $ dbDesk_id dbDesk
          , deskSpace = MkIdentifierSpace $ Selda.fromId @DbSpace $ dbDesk_space dbDesk
          , deskName = MkNameDesk $ dbDesk_name dbDesk
          , deskLocation = do
              x <- dbDesk_position_x dbDesk
              y <- dbDesk_position_y dbDesk
              direction <- dbDesk_direction dbDesk
              width <- dbDesk_size_width dbDesk
              depth <- dbDesk_size_depth dbDesk
              pure
                MkLocationDesk
                  { locationDeskPosition =
                      MkPositionDesk
                        { positionDeskX = MkConstrainedDoubleUnsafe x
                        , positionDeskY = MkConstrainedDoubleUnsafe y
                        }
                  , locationDeskDirection =
                      MkDirectionDesk
                        { unDirectionDesk = MkDirectionDegrees $ MkConstrainedDoubleUnsafe direction
                        }
                  , locationDeskSize =
                      MkSizeDesk
                        { sizeDeskWidth = MkConstrainedDoubleUnsafe width
                        , sizeDeskDepth = MkConstrainedDoubleUnsafe depth
                        }
                  }
          }
  pure $ fromDbDesk <$> dbDesks

deskCreate ::
  (MonadLogger m, MonadSeldaPool m) =>
  NameDesk ->
  IdentifierSpace ->
  Maybe LocationDesk ->
  SeldaTransactionT m IdentifierDesk
deskCreate deskName spaceIdentifier deskLocation = do
  lift $ logDebug "Creating desk."
  let dbDesk =
        MkDbDesk
          { dbDesk_id = Selda.def
          , dbDesk_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbDesk_name = unNameDesk deskName
          , dbDesk_position_x = unConstrainedDouble . positionDeskX . locationDeskPosition <$> deskLocation
          , dbDesk_position_y = unConstrainedDouble . positionDeskY . locationDeskPosition <$> deskLocation
          , dbDesk_direction = unConstrainedDouble . unDirectionDegrees . unDirectionDesk . locationDeskDirection <$> deskLocation
          , dbDesk_size_width = unConstrainedDouble . sizeDeskWidth . locationDeskSize <$> deskLocation
          , dbDesk_size_depth = unConstrainedDouble . sizeDeskDepth . locationDeskSize <$> deskLocation
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

deskLocationSet ::
  (MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  Maybe LocationDesk ->
  SeldaTransactionT m ()
deskLocationSet identifier location = do
  lift $ logDebug $ "Setting location " <> T.pack (show location) <> " of desk " <> T.pack (show identifier) <> "."
  Selda.updateOne
    tableDesk
    (#dbDesk_id `Selda.is` Selda.toId @DbDesk (unIdentifierDesk identifier))
    ( \rowDesk ->
        rowDesk
          `Selda.with` [ #dbDesk_position_x Selda.:= Selda.literal (unConstrainedDouble . positionDeskX . locationDeskPosition <$> location)
                       , #dbDesk_position_y Selda.:= Selda.literal (unConstrainedDouble . positionDeskY . locationDeskPosition <$> location)
                       , #dbDesk_direction Selda.:= Selda.literal (unConstrainedDouble . unDirectionDegrees . unDirectionDesk . locationDeskDirection <$> location)
                       , #dbDesk_size_width Selda.:= Selda.literal (unConstrainedDouble . sizeDeskWidth . locationDeskSize <$> location)
                       , #dbDesk_size_depth Selda.:= Selda.literal (unConstrainedDouble . sizeDeskDepth . locationDeskSize <$> location)
                       ]
    )
  lift $ logInfo "Set desk location successfully."

spaceVisibilityApiToDb :: VisibilitySpace -> DbSpaceVisibility
spaceVisibilityApiToDb = \case
  MkVisibilitySpaceVisible -> MkDbSpaceVisibility_visible
  MkVisibilitySpaceHidden -> MkDbSpaceVisibility_hidden

spaceVisibilityDbToApi :: DbSpaceVisibility -> VisibilitySpace
spaceVisibilityDbToApi = \case
  MkDbSpaceVisibility_visible -> MkVisibilitySpaceVisible
  MkDbSpaceVisibility_hidden -> MkVisibilitySpaceHidden

roleAccessibilityApiToDb :: AccessibilityRole -> DbRoleAccessibility
roleAccessibilityApiToDb = \case
  MkAccessibilityRoleJoinable -> MkDbRoleAccessibility_joinable
  MkAccessibilityRoleJoinableWithPassword -> MkDbRoleAccessibility_joinable_with_password
  MkAccessibilityRoleInaccessible -> MkDbRoleAccessibility_inaccessible

roleAccessibilityDbToApi :: DbRoleAccessibility -> AccessibilityRole
roleAccessibilityDbToApi = \case
  MkDbRoleAccessibility_joinable -> MkAccessibilityRoleJoinable
  MkDbRoleAccessibility_joinable_with_password -> MkAccessibilityRoleJoinableWithPassword
  MkDbRoleAccessibility_inaccessible -> MkAccessibilityRoleInaccessible

spacePermissionApiToDb :: PermissionSpace -> DbSpacePermission
spacePermissionApiToDb = \case
  MkPermissionSpaceViewSpace -> MkDbSpacePermission_view_space
  MkPermissionSpaceEditDesk -> MkDbSpacePermission_edit_desk
  MkPermissionSpaceEditUser -> MkDbSpacePermission_edit_user
  MkPermissionSpaceEditRole -> MkDbSpacePermission_edit_role
  MkPermissionSpaceEditSpace -> MkDbSpacePermission_edit_space
  MkPermissionSpaceCreateReservation -> MkDbSpacePermission_create_reservation
  MkPermissionSpaceCancelReservation -> MkDbSpacePermission_cancel_reservation

spacePermissionDbToApi :: DbSpacePermission -> PermissionSpace
spacePermissionDbToApi = \case
  MkDbSpacePermission_view_space -> MkPermissionSpaceViewSpace
  MkDbSpacePermission_edit_desk -> MkPermissionSpaceEditDesk
  MkDbSpacePermission_edit_user -> MkPermissionSpaceEditUser
  MkDbSpacePermission_edit_role -> MkPermissionSpaceEditRole
  MkDbSpacePermission_edit_space -> MkPermissionSpaceEditSpace
  MkDbSpacePermission_create_reservation -> MkPermissionSpaceCreateReservation
  MkDbSpacePermission_cancel_reservation -> MkPermissionSpaceCancelReservation
