{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Database where

import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Schema

import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Text qualified as T
import Database.Selda qualified as Selda

createDatabase ::
  ( MonadSeldaPool m
  , MonadLogger m
  ) =>
  m ()
createDatabase = do
  logDebug "Creating database."
  result <- runSeldaTransactionT $ do
    lift $ logDebug "Creating table 'user'."
    Selda.createTable tableUser

    lift $ logDebug "Creating table 'session'."
    Selda.createTable tableSession

    lift $ logDebug "Creating table 'space'."
    Selda.createTable tableSpace

    lift $ logDebug "Creating table 'space_user'."
    Selda.createTable tableSpaceUser

    lift $ logDebug "Creating table 'desk'."
    Selda.createTable tableDesk

    lift $ logDebug "Creating table 'reservation'."
    Selda.createTable tableReservation

    lift $ logInfo "Committing transaction."

  case result of
    SeldaSuccess () -> logInfo "Created database."
    SeldaFailure err -> logError $ "Failed to create database: " <> T.pack (show err)

checkDatabase ::
  ( MonadLogger m
  , MonadSeldaPool m
  ) =>
  m ()
checkDatabase = void $ runSeldaTransactionT $ do
  lift $ logInfo "Checking for overlapping reservations."
  reservationsOverlapping <- Selda.query $ do
    dbReservationX <- Selda.select tableReservation
    dbReservationY <- Selda.select tableReservation
    Selda.restrict $ dbReservationX Selda.! #dbReservation_id Selda../= dbReservationY Selda.! #dbReservation_id
    Selda.restrict $ dbReservationX Selda.! #dbReservation_desk Selda..== dbReservationY Selda.! #dbReservation_desk
    Selda.restrict $ dbReservationX Selda.! #dbReservation_time_begin Selda..< dbReservationY Selda.! #dbReservation_time_end
    Selda.restrict $ dbReservationX Selda.! #dbReservation_time_end Selda..> dbReservationY Selda.! #dbReservation_time_begin
    pure dbReservationX
  case reservationsOverlapping of
    [] -> lift $ logInfo "No overlapping reservations found."
    _ : _ -> lift $ logError $ "There are overlapping reservations: " <> T.pack (show reservationsOverlapping)

  pure ()
