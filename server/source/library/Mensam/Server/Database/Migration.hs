{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Database.Migration where

import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Kind
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time qualified as T
import Database.Selda qualified as Selda
import Database.Selda.Unsafe qualified as Selda.Unsafe

migrateDatabase ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  ) =>
  SeldaTransactionT m ()
migrateDatabase = do
  appliedMigrationKeys <- do
    lift $ logDebug "Looking up migrations that have already been applied."
    dbMigrations <- Selda.query $ do
      dbMigration <- Selda.select tableMigration
      Selda.order (dbMigration Selda.! #dbMigration_time_applied) Selda.Asc
      pure dbMigration
    lift $ logInfo $ "These migrations have already been applied: " <> T.pack (show dbMigrations)
    let toKey dbMigration = (dbMigration_id dbMigration, dbMigration_name dbMigration)
    pure $ map toKey dbMigrations
  let
    migrationMap ::
      ( MonadIO m
      , MonadLogger m
      , MonadSeldaPool m
      ) =>
      M.Map (Selda.ID DbMigration, T.Text) (SeldaTransactionT m ())
    migrationMap =
      M.fromListWith (error "Multiple migrations have the same identifier and name.") $
        map (\migration -> ((migrationId migration, migrationName migration), migrationWork migration)) migrations
    migrationsToApply =
      M.toAscList $
        foldr M.delete migrationMap appliedMigrationKeys
  migrationsToApply `for_` \((identifier, name), work) -> do
    lift $ logDebug $ "Applying migration: " <> T.pack (show (identifier, name))
    work
    currentTime <- liftIO T.getCurrentTime
    lift $ logDebug "Setting migration as done."
    Selda.insertOne tableMigration $
      MkDbMigration
        { dbMigration_id = identifier
        , dbMigration_name = name
        , dbMigration_time_applied = currentTime
        }
    pure ()
  pure ()

type Migration :: Type
data Migration = MkMigration
  { migrationId :: Selda.ID DbMigration
  , migrationName :: T.Text
  , migrationWork ::
      forall m.
      ( MonadIO m
      , MonadLogger m
      , MonadSeldaPool m
      ) =>
      SeldaTransactionT m ()
  }

migrations :: [Migration]
migrations =
  [ MkMigration
      { migrationId = Selda.toId 0
      , migrationName = "exampleJustLog"
      , migrationWork = do
          lift $ logInfo "I am logging."
      }
  , MkMigration
      { migrationId = Selda.toId 1
      , migrationName = "exampleDoNothing"
      , migrationWork = pure ()
      }
  , MkMigration
      { migrationId = Selda.toId 2
      , migrationName = "exampleAddColumn"
      , migrationWork =
          Selda.Unsafe.rawStm
            "ALTER TABLE user\n\
            \ADD COLUMN address TEXT"
      }
  , MkMigration
      { migrationId = Selda.toId 3
      , migrationName = "exampleDropColumn"
      , migrationWork =
          Selda.Unsafe.rawStm
            "ALTER TABLE user\n\
            \DROP COLUMN address"
      }
  , MkMigration
      { migrationId = Selda.toId 4
      , migrationName = "addSpacePassword"
      , migrationWork =
          Selda.Unsafe.rawStm
            "ALTER TABLE space\n\
            \ADD COLUMN password_hash TEXT"
      }
  ]

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

  case result of
    SeldaSuccess () -> logInfo "Created database."
    SeldaFailure err -> logError $ "Failed to create database: " <> T.pack (show err)
