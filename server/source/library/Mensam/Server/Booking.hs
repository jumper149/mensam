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
import Data.Text qualified as T
import Data.Time qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

spaceLookupId ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | name
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
          , spaceName = MkNameSpace $ dbSpace_name space
          }
  pure $ fromDbSpace <$> dbSpaces

spaceCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  NameSpace ->
  -- | visible
  Bool ->
  SeldaTransactionT m IdentifierSpace
spaceCreate name visible = do
  lift $ logDebug $ "Creating space: " <> T.pack (show name)
  let dbSpace =
        MkDbSpace
          { dbSpace_id = Selda.def
          , dbSpace_name = unNameSpace name
          , dbSpace_visible = visible
          }
  dbSpaceId <- Selda.insertWithPK tableSpace [dbSpace]
  lift $ logInfo "Created space successfully."
  pure $ MkIdentifierSpace $ Selda.fromId @DbSpace dbSpaceId

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
  -- | space
  IdentifierSpace ->
  -- | desk name
  T.Text ->
  SeldaTransactionT m (Maybe IdentifierDesk)
deskLookupId spaceIdentifier deskName = do
  lift $ logDebug $ "Looking up desk identifier with name: " <> T.pack (show (spaceIdentifier, deskName))
  maybeDbId <- Selda.queryUnique $ do
    dbDesk <- Selda.select tableDesk
    Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $ dbDesk Selda.! #dbDesk_name Selda..== Selda.literal deskName
    pure $ dbDesk Selda.! #dbDesk_id
  case maybeDbId of
    Nothing -> do
      lift $ logWarn $ "Failed to look up desk. Name doesn't exist: " <> T.pack (show deskName)
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
    dbSpaceUser <- Selda.select tableSpaceUser
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)

    dbSpace <- Selda.select tableSpace
    Selda.restrict $ dbSpace Selda.! #dbSpace_id Selda..== Selda.literal (Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier)
    Selda.restrict $
      (dbSpace Selda.! #dbSpace_visible)
        Selda..|| (dbSpace Selda.! #dbSpace_id Selda..== dbSpaceUser Selda.! #dbSpaceUser_space)

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
  SeldaTransactionT m IdentifierDesk
deskCreate deskName spaceIdentifier = do
  lift $ logDebug "Creating desk."
  let dbDesk =
        MkDbDesk
          { dbDesk_id = Selda.def
          , dbDesk_space = Selda.toId @DbSpace $ unIdentifierSpace spaceIdentifier
          , dbDesk_name = deskName
          }
  lift $ logDebug "Inserting desk into database."
  dbDeskId <- Selda.insertWithPK tableDesk [dbDesk]
  lift $ logInfo "Created desk successfully."
  pure $ MkIdentifierDesk $ Selda.fromId @DbDesk dbDeskId

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

-- | List all reservations of a desk, that overlap with the given time window.
reservationList ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  -- | minimum timestamp begin
  Maybe T.UTCTime ->
  -- | maximum timestamp end
  Maybe T.UTCTime ->
  -- | cancelled
  Maybe Bool ->
  SeldaTransactionT m [Reservation]
reservationList deskIdentifier maybeTimestampBegin maybeTimestampEnd maybeCancelled = do
  lift $ logDebug $ "Looking up reservations: " <> T.pack (show (deskIdentifier, maybeTimestampBegin, maybeTimestampEnd))
  dbReservations <- Selda.query $ do
    dbReservation <- Selda.select tableReservation
    Selda.restrict $ dbReservation Selda.! #dbReservation_desk Selda..== Selda.literal (Selda.toId @DbDesk $ unIdentifierDesk deskIdentifier)
    case maybeTimestampBegin of
      Nothing -> pure ()
      Just timestampBegin ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_begin Selda..> Selda.literal timestampBegin
    case maybeTimestampEnd of
      Nothing -> pure ()
      Just timestampBegin ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_begin Selda..< Selda.literal timestampBegin
    case maybeCancelled of
      Nothing -> pure ()
      Just cancelled ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_cancelled Selda..== Selda.literal cancelled
    pure dbReservation
  lift $ logInfo "Looked up reservations successfully."
  let toReservation
        MkDbReservation
          { dbReservation_id
          , dbReservation_desk
          , dbReservation_user
          , dbReservation_time_begin
          , dbReservation_time_end
          , dbReservation_cancelled
          } =
          MkReservation
            { reservationId = MkIdentifierReservation $ Selda.fromId @DbReservation dbReservation_id
            , reservationDesk = MkIdentifierDesk $ Selda.fromId @DbDesk dbReservation_desk
            , reservationUser = MkIdentifierUser $ Selda.fromId @DbUser dbReservation_user
            , reservationTimeBegin = dbReservation_time_begin
            , reservationTimeEnd = dbReservation_time_end
            , reservationCancelled = dbReservation_cancelled
            }
  pure $ toReservation <$> dbReservations

reservationCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  IdentifierDesk ->
  IdentifierUser ->
  -- | timestamp begin
  T.UTCTime ->
  -- | timestamp end
  T.UTCTime ->
  SeldaTransactionT m IdentifierReservation
reservationCreate deskIdentifier userIdentifier timestampBegin timestampEnd = do
  lift $ logDebug "Creating reservation."
  reservations <- reservationList deskIdentifier (Just timestampBegin) (Just timestampEnd) (Just False)
  case reservations of
    _ : _ -> do
      lift $ logWarn "Desk is already reserved."
      throwM MkSqlErrorMensamDeskAlreadyReserved
    [] -> lift $ logDebug "Desk is still free."
  let dbReservation =
        MkDbReservation
          { dbReservation_id = Selda.def
          , dbReservation_desk = Selda.toId @DbDesk $ unIdentifierDesk deskIdentifier
          , dbReservation_user = Selda.toId @DbUser $ unIdentifierUser userIdentifier
          , dbReservation_time_begin = timestampBegin
          , dbReservation_time_end = timestampEnd
          , dbReservation_cancelled = False
          }
  lift $ logDebug "Inserting reservation into database."
  dbReservationId <- Selda.insertWithPK tableReservation [dbReservation]
  lift $ logInfo "Created reservation successfully."
  pure $ MkIdentifierReservation $ Selda.fromId @DbReservation dbReservationId

type SqlErrorMensamDeskAlreadyReserved :: Type
data SqlErrorMensamDeskAlreadyReserved = MkSqlErrorMensamDeskAlreadyReserved
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (Exception)
