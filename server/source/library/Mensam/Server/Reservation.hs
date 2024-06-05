{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Reservation where

import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.User
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
  IntervalUnbounded T.UTCTime ->
  SeldaTransactionT m [Reservation]
reservationList deskIdentifier timestampIntervalUnbounded = do
  lift $ logDebug $ "Looking up desk's reservations: " <> T.pack (show (deskIdentifier, timestampIntervalUnbounded))
  dbReservations <- Selda.query $ do
    dbReservation <- Selda.select tableReservation
    Selda.restrict $ dbReservation Selda.! #dbReservation_desk Selda..== Selda.literal (Selda.toId @DbDesk $ unIdentifierDesk deskIdentifier)
    case intervalUnboundedStart timestampIntervalUnbounded of
      NothingUnboundedLow -> pure ()
      JustUnboundedLow timestampBegin ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_end Selda..> Selda.literal timestampBegin
    case intervalUnboundedEnd timestampIntervalUnbounded of
      NothingUnboundedHigh -> pure ()
      JustUnboundedHigh timestampEnd ->
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
  IntervalUnbounded T.UTCTime ->
  SeldaTransactionT m [Reservation]
reservationListUser userIdentifier timestampIntervalUnbounded = do
  lift $ logDebug $ "Looking up user's reservations: " <> T.pack (show (userIdentifier, timestampIntervalUnbounded))
  dbReservations <- Selda.query $ do
    dbReservation <- Selda.select tableReservation
    Selda.restrict $ dbReservation Selda.! #dbReservation_user Selda..== Selda.literal (Selda.toId @DbUser $ unIdentifierUser userIdentifier)
    -- TODO: Improve overlapping behaviour (OR instead of AND?).
    case intervalUnboundedStart timestampIntervalUnbounded of
      NothingUnboundedLow -> pure ()
      JustUnboundedLow timestampBegin ->
        Selda.restrict $ dbReservation Selda.! #dbReservation_time_end Selda..> Selda.literal timestampBegin
    case intervalUnboundedEnd timestampIntervalUnbounded of
      NothingUnboundedHigh -> pure ()
      JustUnboundedHigh timestampEnd ->
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
  reservations <- reservationList deskIdentifier $ mkIntervalUnboundedFromBounded $ mkIntervalFromNonDegenerate timestampInterval
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
