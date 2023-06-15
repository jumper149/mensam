{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Database where

import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Schema

import Control.Monad
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Text qualified as T
import Database.Selda qualified as Selda
import Database.Selda.Unsafe qualified as Selda.Unsafe

createDatabase ::
  ( MonadSeldaPool m
  , MonadLogger m
  ) =>
  m ()
createDatabase = do
  logDebug "Creating database."
  result <- runSeldaTransactionT $ do
    lift $ logDebug "Creating table 'migration'."
    Selda.Unsafe.rawStm "CREATE TABLE \"migration\"(\"id\" INTEGER  NOT NULL, \"name\" TEXT NOT NULL UNIQUE, \"time_applied\" DATETIME NOT NULL, UNIQUE(\"name\"), PRIMARY KEY(\"id\"))"

    lift $ logDebug "Creating table 'jwk'."
    Selda.Unsafe.rawStm "CREATE TABLE \"jwk\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"jwk\" BLOB NOT NULL UNIQUE, \"created\" DATETIME NOT NULL, UNIQUE(\"jwk\"))"

    lift $ logDebug "Creating table 'user'."
    Selda.Unsafe.rawStm "CREATE TABLE \"user\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"name\" TEXT NOT NULL UNIQUE, \"password_hash\" TEXT NOT NULL, \"email\" TEXT NOT NULL, \"email_visibility\" TEXT NOT NULL, \"email_validated\" BOOLEAN NOT NULL, UNIQUE(\"name\"))"

    lift $ logDebug "Creating table 'confirmation'."
    Selda.Unsafe.rawStm "CREATE TABLE \"confirmation\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"user\" INTEGER NOT NULL, \"secret\" TEXT NOT NULL, \"expired\" DATETIME NOT NULL, \"effect\" TEXT NOT NULL, UNIQUE(\"secret\", \"user\"), CONSTRAINT \"fk0_user\" FOREIGN KEY (\"user\") REFERENCES \"user\"(\"id\"))"

    lift $ logDebug "Creating table 'session'."
    Selda.Unsafe.rawStm "CREATE TABLE \"session\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"user\" INTEGER NOT NULL, \"time_created\" DATETIME NOT NULL, \"time_expired\" DATETIME NULL, CONSTRAINT \"fk0_user\" FOREIGN KEY (\"user\") REFERENCES \"user\"(\"id\"))"

    lift $ logDebug "Creating table 'space'."
    Selda.Unsafe.rawStm "CREATE TABLE \"space\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"name\" TEXT NOT NULL UNIQUE, \"timezone\" TEXT NOT NULL, \"visibility\" TEXT NOT NULL, UNIQUE(\"name\"))"

    lift $ logDebug "Creating table 'space_role'."
    Selda.Unsafe.rawStm "CREATE TABLE \"space_role\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"space\" INTEGER NOT NULL, \"name\" TEXT NOT NULL, \"accessibility\" TEXT NOT NULL, UNIQUE(\"space\", \"name\"), CONSTRAINT \"fk0_space\" FOREIGN KEY (\"space\") REFERENCES \"space\"(\"id\"))"

    lift $ logDebug "Creating table 'space_role_permission'."
    Selda.Unsafe.rawStm "CREATE TABLE \"space_role_permission\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"role\" INTEGER NOT NULL, \"permission\" TEXT NOT NULL, UNIQUE(\"role\", \"permission\"), CONSTRAINT \"fk0_role\" FOREIGN KEY (\"role\") REFERENCES \"space_role\"(\"id\"))"

    lift $ logDebug "Creating table 'space_user'."
    Selda.Unsafe.rawStm "CREATE TABLE \"space_user\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"space\" INTEGER NOT NULL, \"user\" INTEGER NOT NULL, \"role\" INTEGER NOT NULL, UNIQUE(\"space\", \"user\"), CONSTRAINT \"fk0_space\" FOREIGN KEY (\"space\") REFERENCES \"space\"(\"id\"), CONSTRAINT \"fk1_user\" FOREIGN KEY (\"user\") REFERENCES \"user\"(\"id\"))"

    lift $ logDebug "Creating table 'desk'."
    Selda.Unsafe.rawStm "CREATE TABLE \"desk\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"space\" INTEGER NOT NULL, \"name\" TEXT NOT NULL, UNIQUE(\"space\", \"name\"), CONSTRAINT \"fk0_space\" FOREIGN KEY (\"space\") REFERENCES \"space\"(\"id\"))"

    lift $ logDebug "Creating table 'reservation'."
    Selda.Unsafe.rawStm "CREATE TABLE \"reservation\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"desk\" INTEGER NOT NULL, \"user\" INTEGER NOT NULL, \"time_begin\" DATETIME NOT NULL, \"time_end\" DATETIME NOT NULL, \"status\" TEXT NOT NULL, CONSTRAINT \"fk0_desk\" FOREIGN KEY (\"desk\") REFERENCES \"desk\"(\"id\"), CONSTRAINT \"fk1_user\" FOREIGN KEY (\"user\") REFERENCES \"user\"(\"id\"))"

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
