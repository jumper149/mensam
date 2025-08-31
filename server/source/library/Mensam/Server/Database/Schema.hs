{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

{- HLINT ignore "Use camelCase" -}

module Mensam.Server.Database.Schema where

import Mensam.Server.Database.Extra

import Data.ByteString qualified as BS
import Data.Kind
import Data.Maybe
import Data.Text qualified as T
import Data.Time.Zones.All qualified as Time
import Data.Time.Zones.All.OrphanInstances ()
import Database.Selda qualified as Selda
import GHC.Generics

type DbMigration :: Type
data DbMigration = MkDbMigration
  { dbMigration_id :: Selda.ID DbMigration
  , dbMigration_name :: Selda.Text
  , dbMigration_time_applied :: Selda.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

tableMigration :: Selda.Table DbMigration
tableMigration =
  Selda.tableFieldMod
    "migration"
    [ #dbMigration_id Selda.:- Selda.primary
    , #dbMigration_name Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbMigration_")

type DbJwk :: Type
data DbJwk = MkDbJwk
  { dbJwk_id :: ~(Selda.ID DbJwk)
  , dbJwk_jwk :: BS.ByteString
  , dbJwk_created :: Selda.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

tableJwk :: Selda.Table DbJwk
tableJwk =
  Selda.tableFieldMod
    "jwk"
    [ #dbJwk_id Selda.:- Selda.autoPrimary
    , #dbJwk_jwk Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbJwk_")

type DbUser :: Type
data DbUser = MkDbUser
  { dbUser_id :: ~(Selda.ID DbUser)
  , dbUser_name :: Selda.Text
  , dbUser_password_hash :: Selda.Text
  , dbUser_email :: Selda.Text
  , dbUser_email_visibility :: DbEmailVisibility
  , dbUser_email_validated :: Bool
  , dbUser_email_notifications :: Bool
  , dbUser_picture_jpeg :: Maybe BS.ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

type DbEmailVisibility :: Type
data DbEmailVisibility
  = MkDbEmailVisibility_visible
  | MkDbEmailVisibility_hidden
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbEmailVisibility_" DbEmailVisibility)
  deriving anyclass (Selda.SqlType)

tableUser :: Selda.Table DbUser
tableUser =
  Selda.tableFieldMod
    "user"
    [ #dbUser_id Selda.:- Selda.autoPrimary
    , #dbUser_name Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbUser_")

type DbConfirmation :: Type
data DbConfirmation = MkDbConfirmation
  { dbConfirmation_id :: ~(Selda.ID DbConfirmation)
  , dbConfirmation_user :: Selda.ID DbUser
  , dbConfirmation_secret :: Selda.Text
  , dbConfirmation_expired :: Selda.UTCTime
  , dbConfirmation_effect :: Selda.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

tableConfirmation :: Selda.Table DbConfirmation
tableConfirmation =
  Selda.tableFieldMod
    "confirmation"
    [ #dbConfirmation_id Selda.:- Selda.autoPrimary
    , #dbConfirmation_user Selda.:- Selda.foreignKey tableUser #dbUser_id
    , #dbConfirmation_secret Selda.:+ #dbConfirmation_user Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbConfirmation_")

type DbSession :: Type
data DbSession = MkDbSession
  { dbSession_id :: ~(Selda.ID DbSession)
  , dbSession_user :: Selda.ID DbUser
  , dbSession_time_created :: Selda.UTCTime
  , dbSession_time_expired :: Maybe Selda.UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

tableSession :: Selda.Table DbSession
tableSession =
  Selda.tableFieldMod
    "session"
    [ #dbSession_id Selda.:- Selda.autoPrimary
    , #dbSession_user Selda.:- Selda.foreignKey tableUser #dbUser_id
    ]
    (fromJust . T.stripPrefix "dbSession_")

type DbSpace :: Type
data DbSpace = MkDbSpace
  { dbSpace_id :: ~(Selda.ID DbSpace)
  , dbSpace_name :: Selda.Text
  , dbSpace_timezone :: Time.TZLabel
  , dbSpace_discoverability :: DbSpaceDiscoverability
  , dbSpace_owner :: Selda.ID DbUser
  , dbSpace_picture_jpeg :: Maybe BS.ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

type DbSpaceDiscoverability :: Type
data DbSpaceDiscoverability
  = MkDbSpaceDiscoverability_public
  | MkDbSpaceDiscoverability_private
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbSpaceDiscoverability_" DbSpaceDiscoverability)
  deriving anyclass (Selda.SqlType)

tableSpace :: Selda.Table DbSpace
tableSpace =
  Selda.tableFieldMod
    "space"
    [ #dbSpace_id Selda.:- Selda.autoPrimary
    , #dbSpace_name Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbSpace_")

-- TODO: Some assumptions are currently not checked by the database:
--   - When `accessibility = 'joinable_with_password'` we also have `password_hash IS NOT NULL`
--   - When `accessibility != 'joinable_with_password'` we also have `password_hash IS NULL`
type DbRole :: Type
data DbRole = MkDbRole
  { dbRole_id :: ~(Selda.ID DbRole)
  , dbRole_space :: Selda.ID DbSpace
  , dbRole_name :: Selda.Text
  , dbRole_accessibility :: DbRoleAccessibility
  , dbRole_password_hash :: Maybe Selda.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

type DbRoleAccessibility :: Type
data DbRoleAccessibility
  = MkDbRoleAccessibility_joinable
  | MkDbRoleAccessibility_joinable_with_password
  | MkDbRoleAccessibility_inaccessible
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbRoleAccessibility_" DbRoleAccessibility)
  deriving anyclass (Selda.SqlType)

tableRole :: Selda.Table DbRole
tableRole =
  Selda.tableFieldMod
    "role"
    [ #dbRole_id Selda.:- Selda.autoPrimary
    , #dbRole_space Selda.:+ #dbRole_name Selda.:- Selda.unique
    , #dbRole_space Selda.:- Selda.foreignKey tableSpace #dbSpace_id
    ]
    (fromJust . T.stripPrefix "dbRole_")

type DbRolePermission :: Type
data DbRolePermission = MkDbRolePermission
  { dbRolePermission_id :: ~(Selda.ID DbRolePermission)
  , dbRolePermission_role :: Selda.ID DbRole
  , dbRolePermission_permission :: DbPermission
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

tableRolePermission :: Selda.Table DbRolePermission
tableRolePermission =
  Selda.tableFieldMod
    "role_permission"
    [ #dbRolePermission_id Selda.:- Selda.autoPrimary
    , #dbRolePermission_role Selda.:+ #dbRolePermission_permission Selda.:- Selda.unique
    , #dbRolePermission_role Selda.:- Selda.foreignKey tableRole #dbRole_id
    ]
    (fromJust . T.stripPrefix "dbRolePermission_")

type DbPermission :: Type
data DbPermission
  = MkDbPermission_view_space
  | MkDbPermission_edit_desk
  | MkDbPermission_edit_user
  | MkDbPermission_edit_role
  | MkDbPermission_edit_space
  | MkDbPermission_create_reservation
  | MkDbPermission_cancel_reservation
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbPermission_" DbPermission)
  deriving anyclass (Selda.SqlType)

type DbSpaceUser :: Type
data DbSpaceUser = MkDbSpaceUser
  { dbSpaceUser_id :: ~(Selda.ID DbSpaceUser)
  , dbSpaceUser_space :: Selda.ID DbSpace
  , dbSpaceUser_user :: Selda.ID DbUser
  , dbSpaceUser_role :: Selda.ID DbRole
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

tableSpaceUser :: Selda.Table DbSpaceUser
tableSpaceUser =
  Selda.tableFieldMod
    "space_user"
    [ #dbSpaceUser_id Selda.:- Selda.autoPrimary
    , #dbSpaceUser_space Selda.:+ #dbSpaceUser_user Selda.:- Selda.unique
    , #dbSpaceUser_space Selda.:- Selda.foreignKey tableSpace #dbSpace_id
    , #dbSpaceUser_user Selda.:- Selda.foreignKey tableUser #dbUser_id
    ]
    (fromJust . T.stripPrefix "dbSpaceUser_")

type DbDesk :: Type
data DbDesk = MkDbDesk
  { dbDesk_id :: ~(Selda.ID DbDesk)
  , dbDesk_space :: Selda.ID DbSpace
  , dbDesk_name :: Selda.Text
  , dbDesk_position_x :: Maybe Double
  , dbDesk_position_y :: Maybe Double
  , dbDesk_direction :: Maybe Double
  , dbDesk_size_width :: Maybe Double
  , dbDesk_size_depth :: Maybe Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

tableDesk :: Selda.Table DbDesk
tableDesk =
  Selda.tableFieldMod
    "desk"
    [ #dbDesk_id Selda.:- Selda.autoPrimary
    , #dbDesk_space Selda.:+ #dbDesk_name Selda.:- Selda.unique
    , #dbDesk_space Selda.:- Selda.foreignKey tableSpace #dbSpace_id
    ]
    (fromJust . T.stripPrefix "dbDesk_")

type DbReservation :: Type
data DbReservation = MkDbReservation
  { dbReservation_id :: ~(Selda.ID DbReservation)
  , dbReservation_desk :: Selda.ID DbDesk
  , dbReservation_user :: Selda.ID DbUser
  , dbReservation_time_begin :: Selda.UTCTime
  , dbReservation_time_end :: Selda.UTCTime
  , dbReservation_status :: DbReservationStatus
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.Relational, Selda.SqlRow)

type DbReservationStatus :: Type
data DbReservationStatus
  = MkDbReservationStatus_planned
  | MkDbReservationStatus_cancelled
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbReservationStatus_" DbReservationStatus)
  deriving anyclass (Selda.SqlType)

-- TODO: Some assumptions are currently not checked by the database:
--   - Start and end are ordered:
--       `CHECK (time_begin < time_end)`
--   - Two active reservations (x and y) cannot overlap at any time.
tableReservation :: Selda.Table DbReservation
tableReservation =
  Selda.tableFieldMod
    "reservation"
    [ #dbReservation_id Selda.:- Selda.autoPrimary
    , #dbReservation_desk Selda.:- Selda.foreignKey tableDesk #dbDesk_id
    , #dbReservation_user Selda.:- Selda.foreignKey tableUser #dbUser_id
    ]
    (fromJust . T.stripPrefix "dbReservation_")
