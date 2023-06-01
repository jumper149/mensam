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
  Selda.Query backend (Selda.Row backend DbSpaceRole)
spaceListRoles space = do
  dbSpaceRole <- Selda.select tableSpaceRole
  Selda.restrict $ dbSpaceRole Selda.! #dbSpaceRole_space Selda..== Selda.literal space
  pure dbSpaceRole

roleLookup ::
  Selda.ID DbSpace ->
  -- | name
  Selda.Text ->
  Selda.Query backend (Selda.Row backend DbSpaceRole)
roleLookup space name = do
  dbSpaceRole <- spaceListRoles space
  Selda.restrict $ dbSpaceRole Selda.! #dbSpaceRole_name Selda..== Selda.literal name
  pure dbSpaceRole

roleGet ::
  Selda.ID DbSpaceRole ->
  Selda.Query backend (Selda.Row backend DbSpaceRole)
roleGet identifier = do
  dbSpaceRole <- Selda.select tableSpaceRole
  Selda.restrict $ dbSpaceRole Selda.! #dbSpaceRole_id Selda..== Selda.literal identifier
  pure dbSpaceRole

roleListPermissions ::
  Selda.ID DbSpaceRole ->
  Selda.Query backend (Selda.Row backend DbSpaceRolePermission)
roleListPermissions role = do
  dbSpaceRolePermission <- Selda.select tableSpaceRolePermission
  Selda.restrict $ dbSpaceRolePermission Selda.! #dbSpaceRolePermission_role Selda..== Selda.literal role
  pure dbSpaceRolePermission

spaceUserGetRole ::
  Selda.ID DbSpace ->
  Selda.ID DbUser ->
  Selda.Query backend (Selda.Col backend (Selda.ID DbSpaceRole))
spaceUserGetRole space user = do
  dbSpaceUser <- Selda.select tableSpaceUser
  Selda.restrict $
    dbSpaceUser Selda.! #dbSpaceUser_space Selda..== Selda.literal space
      Selda..&& dbSpaceUser Selda.! #dbSpaceUser_user Selda..== Selda.literal user
  pure $ dbSpaceUser Selda.! #dbSpaceUser_role

spaceUserListPermissions ::
  Selda.ID DbSpace ->
  Selda.ID DbUser ->
  Selda.Query backend (Selda.Row backend DbSpaceRolePermission)
spaceUserListPermissions space user = do
  dbRoleId <- spaceUserGetRole space user
  dbSpaceRolePermission <- Selda.select tableSpaceRolePermission
  Selda.restrict $ dbSpaceRolePermission Selda.! #dbSpaceRolePermission_role Selda..== dbRoleId
  pure dbSpaceRolePermission

spaceListDesks ::
  Selda.ID DbSpace ->
  Selda.Query backend (Selda.Row backend DbDesk)
spaceListDesks space = do
  dbDesk <- Selda.select tableDesk
  Selda.restrict $ dbDesk Selda.! #dbDesk_space Selda..== Selda.literal space
  pure dbDesk
