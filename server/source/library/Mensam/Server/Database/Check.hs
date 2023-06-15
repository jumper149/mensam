{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Database.Check where

import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Schema

import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Text qualified as T
import Database.Selda qualified as Selda

checkDatabase ::
  ( MonadLogger m
  , MonadSeldaPool m
  ) =>
  m ()
checkDatabase = void $ runSeldaTransactionT $ do
  --  -- TODO: The validator internally makes a mistake, thinking that INTEGER means Int32.
  --  -- https://github.com/valderman/selda/blob/ab9619db13b93867d1a244441bb4de03d3e1dadb/selda-sqlite/src/Database/Selda/SQLite.hs#L129
  --  lift $ logDebug "Validating table 'user'."
  --  Selda.validateTable tableUser
  --
  --  lift $ logDebug "Validating table 'session'."
  --  Selda.validateTable tableSession
  --
  --  lift $ logDebug "Validating table 'space'."
  --  Selda.validateTable tableSpace
  --
  --  lift $ logDebug "Validating table 'space_user'."
  --  Selda.validateTable tableSpaceUser
  --
  --  lift $ logDebug "Validating table 'desk'."
  --  Selda.validateTable tableDesk
  --
  --  lift $ logDebug "Validating table 'reservation'."
  --  Selda.validateTable tableReservation

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
