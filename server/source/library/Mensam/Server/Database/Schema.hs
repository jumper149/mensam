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
  deriving anyclass (Selda.SqlRow)

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
  deriving anyclass (Selda.SqlRow)

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
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

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
  deriving anyclass (Selda.SqlRow)

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
  deriving anyclass (Selda.SqlRow)

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
  , dbSpace_visibility :: DbSpaceVisibility
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

type DbSpaceVisibility :: Type
data DbSpaceVisibility
  = MkDbSpaceVisibility_visible
  | MkDbSpaceVisibility_hidden
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbSpaceVisibility_" DbSpaceVisibility)
  deriving anyclass (Selda.SqlType)

tableSpace :: Selda.Table DbSpace
tableSpace =
  Selda.tableFieldMod
    "space"
    [ #dbSpace_id Selda.:- Selda.autoPrimary
    , #dbSpace_name Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbSpace_")

type DbSpaceRole :: Type
data DbSpaceRole = MkDbSpaceRole
  { dbSpaceRole_id :: ~(Selda.ID DbSpaceRole)
  , dbSpaceRole_space :: Selda.ID DbSpace
  , dbSpaceRole_name :: Selda.Text
  , dbSpaceRole_accessibility :: DbSpaceRoleAccessibility
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

type DbSpaceRoleAccessibility :: Type
data DbSpaceRoleAccessibility
  = MkDbSpaceRoleAccessibility_joinable
  | MkDbSpaceRoleAccessibility_inaccessible
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbSpaceRoleAccessibility_" DbSpaceRoleAccessibility)
  deriving anyclass (Selda.SqlType)

tableSpaceRole :: Selda.Table DbSpaceRole
tableSpaceRole =
  Selda.tableFieldMod
    "space_role"
    [ #dbSpaceRole_id Selda.:- Selda.autoPrimary
    , #dbSpaceRole_space Selda.:+ #dbSpaceRole_name Selda.:- Selda.unique
    , #dbSpaceRole_space Selda.:- Selda.foreignKey tableSpace #dbSpace_id
    ]
    (fromJust . T.stripPrefix "dbSpaceRole_")

type DbSpaceRolePermission :: Type
data DbSpaceRolePermission = MkDbSpaceRolePermission
  { dbSpaceRolePermission_id :: ~(Selda.ID DbSpaceRolePermission)
  , dbSpaceRolePermission_role :: Selda.ID DbSpaceRole
  , dbSpaceRolePermission_permission :: DbSpacePermission
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

tableSpaceRolePermission :: Selda.Table DbSpaceRolePermission
tableSpaceRolePermission =
  Selda.tableFieldMod
    "space_role_permission"
    [ #dbSpaceRolePermission_id Selda.:- Selda.autoPrimary
    , #dbSpaceRolePermission_role Selda.:+ #dbSpaceRolePermission_permission Selda.:- Selda.unique
    , #dbSpaceRolePermission_role Selda.:- Selda.foreignKey tableSpaceRole #dbSpaceRole_id
    ]
    (fromJust . T.stripPrefix "dbSpaceRolePermission_")

type DbSpacePermission :: Type
data DbSpacePermission
  = MkDbSpacePermission_view_space
  | MkDbSpacePermission_edit_desk
  | MkDbSpacePermission_edit_space
  | MkDbSpacePermission_create_reservation
  | MkDbSpacePermission_cancel_reservation
  deriving stock (Bounded, Enum, Read, Show)
  deriving (Selda.SqlEnum) via (SqlEnumStripPrefix "MkDbSpacePermission_" DbSpacePermission)
  deriving anyclass (Selda.SqlType)

type DbSpaceUser :: Type
data DbSpaceUser = MkDbSpaceUser
  { dbSpaceUser_id :: ~(Selda.ID DbSpaceUser)
  , dbSpaceUser_space :: Selda.ID DbSpace
  , dbSpaceUser_user :: Selda.ID DbUser
  , dbSpaceUser_role :: Selda.ID DbSpaceRole
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

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
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

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
  deriving anyclass (Selda.SqlRow)

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
