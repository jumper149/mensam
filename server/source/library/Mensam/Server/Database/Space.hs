{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Database.Space where

import Mensam.Server.Database.Schema

import Database.Selda qualified as Selda

spaceLookup ::
  -- | name
  Selda.Text ->
  Selda.Query backend (Selda.Row backend DbSpace)
spaceLookup name = do
  dbSpace <- Selda.select tableSpace
  Selda.restrict $ dbSpace Selda.! #dbSpace_name Selda..== Selda.literal name
  pure dbSpace

spaceGet ::
  Selda.ID DbSpace ->
  Selda.Query backend (Selda.Row backend DbSpace)
spaceGet identifier = do
  dbSpace <- Selda.select tableSpace
  Selda.restrict $ dbSpace Selda.! #dbSpace_id Selda..== Selda.literal identifier
  pure dbSpace

spaceListRoles ::
  Selda.ID DbSpace ->
  Selda.Query backend (Selda.Row backend DbRole)
spaceListRoles space = do
  dbRole <- Selda.select tableRole
  Selda.restrict $ dbRole Selda.! #dbRole_space Selda..== Selda.literal space
  pure dbRole

spaceRoleLookup ::
  Selda.ID DbSpace ->
  -- | name
  Selda.Text ->
  Selda.Query backend (Selda.Row backend DbRole)
spaceRoleLookup space name = do
  dbRole <- spaceListRoles space
  Selda.restrict $ dbRole Selda.! #dbRole_name Selda..== Selda.literal name
  pure dbRole

spaceRoleGet ::
  Selda.ID DbRole ->
  Selda.Query backend (Selda.Row backend DbRole)
spaceRoleGet identifier = do
  dbRole <- Selda.select tableRole
  Selda.restrict $ dbRole Selda.! #dbRole_id Selda..== Selda.literal identifier
  pure dbRole

spaceRoleListPermissions ::
  Selda.ID DbRole ->
  Selda.Query backend (Selda.Row backend DbRolePermission)
spaceRoleListPermissions role = do
  dbRolePermission <- Selda.select tableRolePermission
  Selda.restrict $ dbRolePermission Selda.! #dbRolePermission_role Selda..== Selda.literal role
  pure dbRolePermission

spaceUserGetRole ::
  Selda.ID DbSpace ->
  Selda.ID DbUser ->
  Selda.Query backend (Selda.Col backend (Selda.ID DbRole))
spaceUserGetRole space user = do
  dbSpaceUser <- Selda.select tableSpaceUser
  Selda.restrict $
    dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal space
      Selda..&& dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal user
  pure $ dbSpaceUser Selda.! #dbSpaceUser_role

spaceUserListPermissions ::
  Selda.ID DbSpace ->
  Selda.ID DbUser ->
  Selda.Query backend (Selda.Row backend DbRolePermission)
spaceUserListPermissions space user = do
  dbRoleId <- spaceUserGetRole space user
  dbRolePermission <- Selda.select tableRolePermission
  Selda.restrict $ dbRolePermission Selda.! #dbRolePermission_role Selda..== dbRoleId
  pure dbRolePermission

spaceListUsers ::
  Selda.ID DbSpace ->
  Selda.Query backend (Selda.Row backend DbSpaceUser)
spaceListUsers space = do
  dbSpaceUser <- Selda.select tableSpaceUser
  Selda.restrict $ dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal space
  pure dbSpaceUser

spaceListDesks ::
  Selda.ID DbSpace ->
  Selda.Query backend (Selda.Row backend DbDesk)
spaceListDesks space = do
  dbDesk <- Selda.select tableDesk
  Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== Selda.literal space
  pure dbDesk
