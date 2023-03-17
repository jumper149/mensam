{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Database where

import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Schema

import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Text qualified as T
import Database.Selda qualified as Selda

initDatabase :: MonadSeldaPool m => m ()
initDatabase = void $ runSeldaTransactionT $ do
  Selda.createTable tableUser
  Selda.createTable tableSpace
  Selda.createTable tableSpaceUser
  Selda.createTable tableDesk
  Selda.createTable tableReservation

checkDatabase :: (MonadLogger m, MonadSeldaPool m) => m ()
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
