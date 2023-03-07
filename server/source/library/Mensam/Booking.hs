{-# LANGUAGE OverloadedLabels #-}

module Mensam.Booking where

import Mensam.Application.SeldaPool.Class
import Mensam.Database

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Time qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

type Space :: Type
newtype Space = MkSpace
  { spaceName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

spaceCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Space ->
  SeldaTransactionT m ()
spaceCreate space@MkSpace {spaceName} = do
  lift $ logDebug $ "Creating new space: " <> T.pack (show space)
  let dbSpace =
        MkDbSpace
          { dbSpace_id = Selda.def
          , dbSpace_name = spaceName
          }
  lift $ logDebug "Inserting new space into database."
  Selda.insert_ tableSpace [dbSpace]
  lift $ logInfo "Created new space successfully."

spaceAddUser ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  T.Text ->
  T.Text ->
  Bool ->
  SeldaTransactionT m ()
spaceAddUser spaceName userName isAdmin = do
  lift $ logDebug $ "Adding user " <> T.pack (show userName) <> " to space " <> T.pack (show spaceName) <> "."
  dbSpaceUser_space <- do
    spaceIds <- Selda.query $ do
      space <- Selda.select tableSpace
      Selda.restrict $ space Selda.! #dbSpace_name Selda..== Selda.literal spaceName
      pure $ space Selda.! #dbSpace_id
    case spaceIds of
      [] -> do
        let msg :: T.Text = "No matching space."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      [spaceId] -> pure spaceId
      _ : _ : _ -> do
        let msg :: T.Text = "Multiple matching spaces."
        lift $ logError msg
        throwM $ Selda.SqlError $ show msg
  dbSpaceUser_user <- do
    userIds <- Selda.query $ do
      user <- Selda.select tableUser
      Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal userName
      pure $ user Selda.! #dbUser_id
    case userIds of
      [] -> do
        let msg :: T.Text = "No matching user."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      [userId] -> pure userId
      _ : _ : _ -> do
        let msg :: T.Text = "Multiple matching users."
        lift $ logError msg
        throwM $ Selda.SqlError $ show msg
  let dbSpaceUser =
        MkDbSpaceUser
          { dbSpaceUser_space
          , dbSpaceUser_user
          , dbSpaceUser_is_admin = isAdmin
          }
  lift $ logDebug "Inserting new space-user connection into database."
  Selda.insert_ tableSpaceUser [dbSpaceUser]
  lift $ logInfo "Created new space successfully."

type Desk :: Type
newtype Desk = MkDesk
  { deskName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

deskCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Desk ->
  T.Text ->
  SeldaTransactionT m ()
deskCreate desk@MkDesk {deskName} spaceName = do
  lift $ logDebug $ "Creating new desk: " <> T.pack (show (desk, spaceName))
  dbDesk_space <- do
    spaceIds <- Selda.query $ do
      space <- Selda.select tableSpace
      Selda.restrict $ space Selda.! #dbSpace_name Selda..== Selda.literal spaceName
      pure $ space Selda.! #dbSpace_id
    case spaceIds of
      [] -> do
        let msg :: T.Text = "No matching space."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      [spaceId] -> pure spaceId
      _ : _ : _ -> do
        let msg :: T.Text = "Multiple matching spaces."
        lift $ logError msg
        throwM $ Selda.SqlError $ show msg
  let dbDesk =
        MkDbDesk
          { dbDesk_id = Selda.def
          , dbDesk_space
          , dbDesk_name = deskName
          }
  lift $ logDebug "Inserting new desk into database."
  Selda.insert_ tableDesk [dbDesk]
  lift $ logInfo "Created new desk successfully."

reservationCreate ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  -- | space name
  T.Text ->
  -- | desk name
  T.Text ->
  -- | user name
  T.Text ->
  -- | timestamp begin
  T.UTCTime ->
  -- | timestamp end
  T.UTCTime ->
  SeldaTransactionT m ()
reservationCreate spaceName deskName userName timestampBegin timestampEnd = do
  lift $ logDebug "Creating new reservation."
  dbSpace_id <- do
    spaceIds <- Selda.query $ do
      space <- Selda.select tableSpace
      Selda.restrict $ space Selda.! #dbSpace_name Selda..== Selda.literal spaceName
      pure $ space Selda.! #dbSpace_id
    case spaceIds of
      [] -> do
        let msg :: T.Text = "No matching space."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      [spaceId] -> pure spaceId
      _ : _ : _ -> do
        let msg :: T.Text = "Multiple matching spaces."
        lift $ logError msg
        throwM $ Selda.SqlError $ show msg
  dbDesk_id <- do
    deskIds <- Selda.query $ do
      desk <- Selda.select tableDesk
      Selda.restrict $ desk Selda.! #dbDesk_space Selda..== Selda.literal dbSpace_id
      Selda.restrict $ desk Selda.! #dbDesk_name Selda..== Selda.literal deskName
      pure $ desk Selda.! #dbDesk_id
    case deskIds of
      [] -> do
        let msg :: T.Text = "No matching desk."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      [deskId] -> pure deskId
      _ : _ : _ -> do
        let msg :: T.Text = "Multiple matching desks."
        lift $ logError msg
        throwM $ Selda.SqlError $ show msg
  dbUser_id <- do
    userIds <- Selda.query $ do
      user <- Selda.select tableUser
      Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal userName
      pure $ user Selda.! #dbUser_id
    case userIds of
      [] -> do
        let msg :: T.Text = "No matching user."
        lift $ logWarn msg
        throwM $ Selda.SqlError $ show msg
      [userId] -> pure userId
      _ : _ : _ -> do
        let msg :: T.Text = "Multiple matching users."
        lift $ logError msg
        throwM $ Selda.SqlError $ show msg
  let dbReservation =
        MkDbReservation
          { dbReservation_id = Selda.def
          , dbReservation_desk = dbDesk_id
          , dbReservation_user = dbUser_id
          , dbReservation_time_begin = timestampBegin
          , dbReservation_time_end = timestampEnd
          }
  lift $ logDebug "Inserting reservation into database."
  Selda.insert_ tableReservation [dbReservation]
  lift $ logInfo "Created reservation successfully."
  pure undefined
