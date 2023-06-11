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
import Database.Selda.Unsafe qualified as Selda

migrate ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  ) =>
  SeldaTransactionT m ()
migrate = do
  appliedMigrationKeys <- do
    lift $ logDebug "Looking up migrations that have already been applied."
    dbMigrations <- Selda.query $ do
      dbMigration <- Selda.select tableMigration
      Selda.order (dbMigration Selda.! #dbMigration_time_applied) Selda.Asc
      pure dbMigration
    lift $ logInfo $ "These migrations have already been applied: " <> T.pack (show dbMigrations)
    let toKey dbMigration = (dbMigration_id dbMigration, dbMigration_name dbMigration)
    pure $ map toKey dbMigrations
  let migrationsToApply =
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

migrationMap ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  ) =>
  M.Map (Selda.ID DbMigration, T.Text) (SeldaTransactionT m ())
migrationMap =
  M.fromListWith (error "Multiple migrations have the same identifier and name.") $
    map (\migration -> ((migrationId migration, migrationName migration), migrationWork migration)) migrations

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
          Selda.rawStm
            "ALTER TABLE user\n\
            \ADD COLUMN address TEXT"
      }
  , MkMigration
      { migrationId = Selda.toId 3
      , migrationName = "exampleDropColumn"
      , migrationWork =
          Selda.rawStm
            "ALTER TABLE user\n\
            \DROP COLUMN address"
      }
  ]
