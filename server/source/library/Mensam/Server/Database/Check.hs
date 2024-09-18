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
  --
  --  lift $ logDebug "Validating table 'migration'."
  --  Selda.validateTable tableMigration
  --
  --  lift $ logDebug "Validating table 'jwk'."
  --  Selda.validateTable tableJwk
  --
  --  lift $ logDebug "Validating table 'user'."
  --  Selda.validateTable tableUser
  --
  --  lift $ logDebug "Validating table 'confirmation'."
  --  Selda.validateTable tableConfirmation
  --
  --  lift $ logDebug "Validating table 'session'."
  --  Selda.validateTable tableSession
  --
  --  lift $ logDebug "Validating table 'space'."
  --  Selda.validateTable tableSpace
  --
  --  lift $ logDebug "Validating table 'space_role'."
  --  Selda.validateTable tableSpaceRole
  --
  --  lift $ logDebug "Validating table 'space_role_permission'."
  --  Selda.validateTable tableSpaceRolePermission
  --
  --  lift $ logDebug "Validating table 'space_user'."
  --  Selda.validateTable tableSpaceUser
  --
  --  lift $ logDebug "Validating table 'desk'."
  --  Selda.validateTable tableDesk
  --
  --  lift $ logDebug "Validating table 'reservation'."
  --  Selda.validateTable tableReservation

  do
    lift $ logInfo "Checking for overlapping reservations."
    reservationsOverlapping <- Selda.query $ do
      dbReservationX <- Selda.select tableReservation
      Selda.restrict $ dbReservationX Selda.! #dbReservation_status Selda../= Selda.literal MkDbReservationStatus_cancelled
      dbReservationY <- Selda.select tableReservation
      Selda.restrict $ dbReservationY Selda.! #dbReservation_status Selda../= Selda.literal MkDbReservationStatus_cancelled
      Selda.restrict $ dbReservationX Selda.! #dbReservation_id Selda../= dbReservationY Selda.! #dbReservation_id
      Selda.restrict $ dbReservationX Selda.! #dbReservation_desk Selda..== dbReservationY Selda.! #dbReservation_desk
      Selda.restrict $ dbReservationX Selda.! #dbReservation_time_begin Selda..< dbReservationY Selda.! #dbReservation_time_end
      Selda.restrict $ dbReservationX Selda.! #dbReservation_time_end Selda..> dbReservationY Selda.! #dbReservation_time_begin
      pure dbReservationX
    case reservationsOverlapping of
      [] -> lift $ logInfo "No overlapping reservations found."
      _ : _ -> lift $ logError $ "There are overlapping reservations: " <> T.pack (show reservationsOverlapping)

  do
    lift $ logInfo "Checking that setting a password coincides with the `joinable_with_password` accessibility."
    do
      dbSpaceRolesJoinableWithPasswordButPasswordNull <- Selda.query $ do
        dbSpaceRole <- Selda.select tableSpaceRole
        Selda.restrict $ dbSpaceRole Selda.! #dbSpaceRole_accessibility Selda..== Selda.literal MkDbSpaceRoleAccessibility_joinable_with_password
        Selda.restrict $ Selda.isNull $ dbSpaceRole Selda.! #dbSpaceRole_password_hash
        pure dbSpaceRole
      case dbSpaceRolesJoinableWithPasswordButPasswordNull of
        [] -> lift $ logInfo "No space roles with missing passwords found."
        _ : _ -> lift $ logError $ "There are missing passwords in space roles: " <> T.pack (show dbSpaceRolesJoinableWithPasswordButPasswordNull)
    do
      dbSpaceRolesNotJoinableWithPasswordButPasswordNotNull <- Selda.query $ do
        dbSpaceRole <- Selda.select tableSpaceRole
        Selda.restrict $ dbSpaceRole Selda.! #dbSpaceRole_accessibility Selda../= Selda.literal MkDbSpaceRoleAccessibility_joinable_with_password
        Selda.restrict $ Selda.not_ $ Selda.isNull $ dbSpaceRole Selda.! #dbSpaceRole_password_hash
        pure dbSpaceRole
      case dbSpaceRolesNotJoinableWithPasswordButPasswordNotNull of
        [] -> lift $ logInfo "No space roles with passwords and unexpected accessibility found."
        _ : _ -> lift $ logError $ "There are unexpected accessibilities in space roles: " <> T.pack (show dbSpaceRolesNotJoinableWithPasswordButPasswordNotNull)

  do
    lift $ logInfo "Checking that the roles used for `space_user`s actually belong to the right space."
    dbSpaceUsersWithWrongRoles <- Selda.query $ do
      dbSpaceUser <- Selda.select tableSpaceUser
      dbSpaceRole <-
        Selda.innerJoin
          ( \dbSpaceRole ->
              dbSpaceRole Selda.! #dbSpaceRole_id Selda..== dbSpaceUser Selda.! #dbSpaceUser_role
                Selda..&& dbSpaceRole Selda.! #dbSpaceRole_space Selda../= dbSpaceUser Selda.! #dbSpaceUser_space
          )
          (Selda.select tableSpaceRole)
      pure (dbSpaceUser Selda.:*: dbSpaceRole)
    case dbSpaceUsersWithWrongRoles of
      [] -> lift $ logInfo "No users have roles from other spaces."
      _ : _ -> lift $ logError $ "There are unexpected role/space combinations in space users: " <> T.pack (show dbSpaceUsersWithWrongRoles)

  do
    lift $ logInfo "Checking that desk locations cover all parameters (position, direction and size)"
    dbDesksWithBrokenLocations <- Selda.query $ do
      dbDesk <- Selda.select tableDesk
      dbDeskIdJoined <-
        Selda.leftJoin
          ( \dbDeskWithOrWithoutLocationId ->
              dbDesk Selda.! #dbDesk_id Selda..== dbDeskWithOrWithoutLocationId
          )
          ( do
              dbDeskWithOrWithoutLocation <- Selda.select tableDesk
              Selda.restrict $
                ( Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_position_x)
                    Selda..&& Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_position_y)
                    Selda..&& Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_direction)
                    Selda..&& Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_size_width)
                    Selda..&& Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_size_depth)
                )
                  Selda..|| ( Selda.not_ (Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_position_x))
                                Selda..&& Selda.not_ (Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_position_y))
                                Selda..&& Selda.not_ (Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_direction))
                                Selda..&& Selda.not_ (Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_size_width))
                                Selda..&& Selda.not_ (Selda.isNull (dbDeskWithOrWithoutLocation Selda.! #dbDesk_size_depth))
                            )
              pure $ dbDeskWithOrWithoutLocation Selda.! #dbDesk_id
          )
      Selda.restrict $ Selda.isNull dbDeskIdJoined
      pure dbDesk
    case dbDesksWithBrokenLocations of
      [] -> lift $ logInfo "No desks have broken locations."
      _ : _ -> lift $ logError $ "There are desks with broken locations: " <> T.pack (show dbDesksWithBrokenLocations)

  pure ()
