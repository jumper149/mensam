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
  forall m.
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
      forall n.
      ( MonadIO n
      , MonadLogger n
      , MonadSeldaPool n
      ) =>
      M.Map (Selda.ID DbMigration, T.Text) (SeldaTransactionT n ())
    migrationMap =
      M.fromListWith (error "Multiple migrations have the same identifier and name.") $
        map (\migration -> ((migrationId migration, migrationName migration), migrationWork migration)) migrations
    migrationsToApply =
      M.toAscList $
        foldr M.delete migrationMap appliedMigrationKeys
    unknownMigrationKeysAlreadyApplied =
      map fst $
        M.toAscList $
          foldr M.delete (M.fromList $ (,()) <$> appliedMigrationKeys) (M.keys (migrationMap @m))
  case unknownMigrationKeysAlreadyApplied of
    [] -> do
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
    _ -> do
      lift $ logWarn $ "Unknown migrations have already been applied to the database: " <> T.pack (show unknownMigrationKeysAlreadyApplied)
      lift $ logError "Database is in an unknown state. Let's stop here to prevent undefined behaviour."
      error "Unknown migrations have been applied before. Database is in an unknown state."
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
  , MkMigration
      { migrationId = Selda.toId 5
      , migrationName = "addSpaceOwner"
      , migrationWork = do
          lift $ logDebug "First add a column without the `NOT NULL` constraint."
          Selda.Unsafe.rawStm
            "ALTER TABLE space\n\
            \ADD COLUMN owner REFERENCES \"user\"(\"id\")"

          lift $ logDebug "Set the correct values to the new owner column."
          Selda.Unsafe.rawStm
            "UPDATE space\n\
            \SET owner = user\n\
            \FROM (SELECT * FROM space_user ORDER BY id)\n\
            \WHERE space = space.id"

          lift $ logDebug "Clean up orphaned spaces without owner."
          lift $ logInfo "Spaces without members will now be deleted permanently."
          Selda.Unsafe.rawStm
            "DELETE FROM space_role_permission\n\
            \WHERE id IN\n\
            \(\n\
            \    SELECT space_role_permission.id\n\
            \    FROM space_role_permission\n\
            \    JOIN space_role ON space_role_permission.role = space_role.id\n\
            \    JOIN space ON space_role.space = space.id\n\
            \    WHERE space.owner IS NULL\n\
            \)"
          Selda.Unsafe.rawStm
            "DELETE FROM space_role\n\
            \WHERE id IN\n\
            \(\n\
            \    SELECT space_role.id\n\
            \    FROM space_role\n\
            \    JOIN space ON space_role.space = space.id\n\
            \    WHERE space.owner IS NULL\n\
            \)"
          -- There should be no `space_user`s because otherwise the space would have an `owner` now.
          Selda.Unsafe.rawStm
            "DELETE FROM space_user\n\
            \WHERE id IN\n\
            \(\n\
            \    SELECT space_user.id\n\
            \    FROM space_user\n\
            \    JOIN space ON space_user.space = space.id\n\
            \    WHERE space.owner IS NULL\n\
            \)"
          Selda.Unsafe.rawStm
            "DELETE FROM reservation\n\
            \WHERE id IN\n\
            \(\n\
            \    SELECT reservation.id\n\
            \    FROM reservation\n\
            \    JOIN desk ON reservation.desk = desk.id\n\
            \    JOIN space ON desk.space = space.id\n\
            \    WHERE space.owner IS NULL\n\
            \)"
          Selda.Unsafe.rawStm
            "DELETE FROM desk\n\
            \WHERE id IN\n\
            \(\n\
            \    SELECT desk.id\n\
            \    FROM desk\n\
            \    JOIN space ON desk.space = space.id\n\
            \    WHERE space.owner IS NULL\n\
            \)"
          Selda.Unsafe.rawStm
            "DELETE FROM space\n\
            \WHERE owner IS NULL"

          lift $ logDebug "The complicated logic is done. Now we have to redo most of the schema to get a `FOREIGN KEY` constraint on the owner column."

          lift $ logDebug "Declare a bunch of tables as temporary."
          Selda.Unsafe.rawStm
            "ALTER TABLE space\n\
            \RENAME TO space_temp"
          Selda.Unsafe.rawStm
            "ALTER TABLE space_user\n\
            \RENAME TO space_user_temp"
          Selda.Unsafe.rawStm
            "ALTER TABLE space_role\n\
            \RENAME TO space_role_temp"
          Selda.Unsafe.rawStm
            "ALTER TABLE space_role_permission\n\
            \RENAME TO space_role_permission_temp"
          Selda.Unsafe.rawStm
            "ALTER TABLE desk\n\
            \RENAME TO desk_temp"
          Selda.Unsafe.rawStm
            "ALTER TABLE reservation\n\
            \RENAME TO reservation_temp"

          lift $ logDebug "And now recreate all tables that depend on each other."
          Selda.Unsafe.rawStm
            "CREATE TABLE \"space\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"name\" TEXT NOT NULL UNIQUE, \"timezone\" TEXT NOT NULL, \"visibility\" TEXT NOT NULL, \"password_hash\" TEXT, \"owner\" NOT NULL REFERENCES \"user\"(\"id\"), UNIQUE(\"name\"))"
          Selda.Unsafe.rawStm
            "CREATE TABLE \"space_user\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"space\" INTEGER NOT NULL, \"user\" INTEGER NOT NULL, \"role\" INTEGER NOT NULL, UNIQUE(\"space\", \"user\"), CONSTRAINT \"fk0_space\" FOREIGN KEY (\"space\") REFERENCES \"space\"(\"id\"), CONSTRAINT \"fk1_user\" FOREIGN KEY (\"user\") REFERENCES \"user\"(\"id\"))"
          Selda.Unsafe.rawStm
            "CREATE TABLE \"space_role\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"space\" INTEGER NOT NULL, \"name\" TEXT NOT NULL, \"accessibility\" TEXT NOT NULL, UNIQUE(\"space\", \"name\"), CONSTRAINT \"fk0_space\" FOREIGN KEY (\"space\") REFERENCES \"space\"(\"id\"))"
          Selda.Unsafe.rawStm
            "CREATE TABLE \"space_role_permission\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"role\" INTEGER NOT NULL, \"permission\" TEXT NOT NULL, UNIQUE(\"role\", \"permission\"), CONSTRAINT \"fk0_role\" FOREIGN KEY (\"role\") REFERENCES \"space_role\"(\"id\"))"
          Selda.Unsafe.rawStm
            "CREATE TABLE \"desk\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"space\" INTEGER NOT NULL, \"name\" TEXT NOT NULL, UNIQUE(\"space\", \"name\"), CONSTRAINT \"fk0_space\" FOREIGN KEY (\"space\") REFERENCES \"space\"(\"id\"))"
          Selda.Unsafe.rawStm
            "CREATE TABLE \"reservation\"(\"id\" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, \"desk\" INTEGER NOT NULL, \"user\" INTEGER NOT NULL, \"time_begin\" DATETIME NOT NULL, \"time_end\" DATETIME NOT NULL, \"status\" TEXT NOT NULL, CONSTRAINT \"fk0_desk\" FOREIGN KEY (\"desk\") REFERENCES \"desk\"(\"id\"), CONSTRAINT \"fk1_user\" FOREIGN KEY (\"user\") REFERENCES \"user\"(\"id\"))"

          lift $ logDebug "Insert all rows from the old tables into the new tables."
          Selda.Unsafe.rawStm
            "INSERT INTO space\n\
            \SELECT * FROM space_temp"
          Selda.Unsafe.rawStm
            "INSERT INTO space_user\n\
            \SELECT * FROM space_user_temp"
          Selda.Unsafe.rawStm
            "INSERT INTO space_role\n\
            \SELECT * FROM space_role_temp"
          Selda.Unsafe.rawStm
            "INSERT INTO space_role_permission\n\
            \SELECT * FROM space_role_permission_temp"
          Selda.Unsafe.rawStm
            "INSERT INTO desk\n\
            \SELECT * FROM desk_temp"
          Selda.Unsafe.rawStm
            "INSERT INTO reservation\n\
            \SELECT * FROM reservation_temp"

          lift $ logDebug "Drop the temporary tables."
          Selda.Unsafe.rawStm "DROP TABLE reservation_temp"
          Selda.Unsafe.rawStm "DROP TABLE desk_temp"
          Selda.Unsafe.rawStm "DROP TABLE space_role_permission_temp"
          Selda.Unsafe.rawStm "DROP TABLE space_role_temp"
          Selda.Unsafe.rawStm "DROP TABLE space_user_temp"
          Selda.Unsafe.rawStm "DROP TABLE space_temp"

          lift $ logDebug "The `owner` column has been successfully added to the `space` table."
      }
  , MkMigration
      { migrationId = Selda.toId 6
      , migrationName = "addRoleAccessibilityJoinableWithPassword"
      , migrationWork =
          Selda.Unsafe.rawStm
            "UPDATE space_role\n\
            \SET accessibility = 'joinable_with_password'\n\
            \FROM (SELECT id AS space_id, password_hash AS space_password_hash FROM space)\n\
            \WHERE space_role.space = space_id\n\
            \AND space_password_hash IS NOT NULL\n\
            \AND space_role.accessibility = 'joinable'"
      }
  , MkMigration
      { migrationId = Selda.toId 7
      , migrationName = "addRolePasswordHash"
      , migrationWork = do
          lift $ logDebug "Create new `password_hash` column for `space_role`."
          Selda.Unsafe.rawStm
            "ALTER TABLE space_role\n\
            \ADD COLUMN password_hash TEXT"

          lift $ logDebug "Set the correct values to the new `password_hash` column."
          Selda.Unsafe.rawStm
            "UPDATE space_role\n\
            \SET password_hash = space.password_hash\n\
            \FROM space\n\
            \WHERE space_role.space = space.id\n\
            \AND space_role.accessibility = 'joinable_with_password'"

          lift $ logDebug "Delete old `password_hash` column from `space`."
          Selda.Unsafe.rawStm
            "ALTER TABLE space\n\
            \DROP COLUMN password_hash"
      }
  , MkMigration
      { migrationId = Selda.toId 8
      , migrationName = "fixSpaceOwner"
      , migrationWork = do
          lift $ logDebug "Fixing the space owner column. One of the previous migrations set regular members as owners. We are setting the owner column to be an admin if possible."
          Selda.Unsafe.rawStm
            "UPDATE space\n\
            \SET owner = user\n\
            \FROM (\n\
            \    SELECT space_user.space AS space, space_user.user AS user\n\
            \    FROM space_user\n\
            \    JOIN space_role\n\
            \    WHERE space_user.role = space_role.id\n\
            \    AND space_role.name = 'Admin'\n\
            \    ORDER BY space_user.id DESC\n\
            \)\n\
            \WHERE space = space.id"
      }
  , MkMigration
      { migrationId = Selda.toId 9
      , migrationName = "addSpaceEditPermission"
      , migrationWork = do
          lift $ logDebug "Space admins should have the 'edit_space' permission."
          Selda.Unsafe.rawStm
            "INSERT INTO space_role_permission (role, permission)\n\
            \SELECT role AS role, 'edit_space' AS permission\n\
            \FROM space_role_permission\n\
            \WHERE space_role_permission.permission = 'edit_desk'\n\
            \ON CONFLICT DO NOTHING"
      }
  , MkMigration
      { migrationId = Selda.toId 10
      , migrationName = "addRoleEditPermission"
      , migrationWork = do
          lift $ logDebug "Space admins should have the 'edit_role' permission."
          Selda.Unsafe.rawStm
            "INSERT INTO space_role_permission (role, permission)\n\
            \SELECT role AS role, 'edit_role' AS permission\n\
            \FROM space_role_permission\n\
            \WHERE space_role_permission.permission = 'edit_space'\n\
            \ON CONFLICT DO NOTHING"
      }
  , MkMigration
      { migrationId = Selda.toId 11
      , migrationName = "deleteSpaceUsersWithForeignRole"
      , migrationWork = do
          lift $ logDebug "Due to an oversight it was possible to join a space with the role of a different space."
          lift $ logInfo "All space users with foreign roles will be removed permanently."
          Selda.Unsafe.rawStm
            "DELETE FROM space_user\n\
            \WHERE space_user.space NOT IN (\n\
            \  SELECT space_role.space\n\
            \  FROM space_role\n\
            \  WHERE id = space_user.role\n\
            \)"
      }
  , MkMigration
      { migrationId = Selda.toId 12
      , migrationName = "addUserEditPermission"
      , migrationWork = do
          lift $ logDebug "Space admins should have the 'edit_user' permission."
          Selda.Unsafe.rawStm
            "INSERT INTO space_role_permission (role, permission)\n\
            \SELECT role AS role, 'edit_user' AS permission\n\
            \FROM space_role_permission\n\
            \WHERE space_role_permission.permission = 'edit_space'\n\
            \ON CONFLICT DO NOTHING"
      }
  , MkMigration
      { migrationId = Selda.toId 13
      , migrationName = "addUserProfilePicture"
      , migrationWork = do
          Selda.Unsafe.rawStm
            "ALTER TABLE user\n\
            \ADD COLUMN picture_jpeg BLOB"
      }
  , MkMigration
      { migrationId = Selda.toId 14
      , migrationName = "addDeskLocation"
      , migrationWork = do
          Selda.Unsafe.rawStm
            "ALTER TABLE desk\n\
            \ADD COLUMN position_x DOUBLE PRECISION"
          Selda.Unsafe.rawStm
            "ALTER TABLE desk\n\
            \ADD COLUMN position_y DOUBLE PRECISION"
          Selda.Unsafe.rawStm
            "ALTER TABLE desk\n\
            \ADD COLUMN direction DOUBLE PRECISION"
          Selda.Unsafe.rawStm
            "ALTER TABLE desk\n\
            \ADD COLUMN size_width DOUBLE PRECISION"
          Selda.Unsafe.rawStm
            "ALTER TABLE desk\n\
            \ADD COLUMN size_depth DOUBLE PRECISION"
      }
  , MkMigration
      { migrationId = Selda.toId 15
      , migrationName = "addUserEmailNotificationOption"
      , migrationWork = do
          Selda.Unsafe.rawStm
            "ALTER TABLE user\n\
            \ADD COLUMN email_notifications BOOLEAN NOT NULL DEFAULT FALSE"
      }
  , MkMigration
      { migrationId = Selda.toId 16
      , migrationName = "addSpacePicture"
      , migrationWork = do
          Selda.Unsafe.rawStm
            "ALTER TABLE space\n\
            \ADD COLUMN picture_jpeg BLOB"
      }
  , MkMigration
      { migrationId = Selda.toId 17
      , migrationName = "renameSpaceRoleToRole"
      , migrationWork = do
          Selda.Unsafe.rawStm
            "ALTER TABLE space_role\n\
            \RENAME TO role"
          Selda.Unsafe.rawStm
            "ALTER TABLE space_role_permission\n\
            \RENAME TO role_permission"
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
