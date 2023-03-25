{-# LANGUAGE OverloadedLabels #-}

{- HLINT ignore "Use camelCase" -}

module Mensam.Server.Database.Schema where

import Mensam.Server.Database.Extra

import Data.Kind
import Data.Maybe
import Data.Text qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

type DbUser :: Type
data DbUser = MkDbUser
  { dbUser_id :: Selda.ID DbUser
  , dbUser_name :: Selda.Text
  , dbUser_password_hash :: Selda.Text
  , dbUser_email :: Selda.Text
  , dbUser_email_visibility :: DbEmailVisibility
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

type DbSpace :: Type
data DbSpace = MkDbSpace
  { dbSpace_id :: Selda.ID DbSpace
  , dbSpace_name :: Selda.Text
  , dbSpace_visible :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

tableSpace :: Selda.Table DbSpace
tableSpace =
  Selda.tableFieldMod
    "space"
    [ #dbSpace_id Selda.:- Selda.autoPrimary
    , #dbSpace_name Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbSpace_")

type DbSpaceUser :: Type
data DbSpaceUser = MkDbSpaceUser
  { dbSpaceUser_space :: Selda.ID DbSpace
  , dbSpaceUser_user :: Selda.ID DbUser
  , dbSpaceUser_is_admin :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

tableSpaceUser :: Selda.Table DbSpaceUser
tableSpaceUser =
  Selda.tableFieldMod
    "space_user"
    [ #dbSpaceUser_space Selda.:+ #dbSpaceUser_user Selda.:- Selda.primary
    , #dbSpaceUser_space Selda.:- Selda.foreignKey tableSpace #dbSpace_id
    , #dbSpaceUser_user Selda.:- Selda.foreignKey tableUser #dbUser_id
    ]
    (fromJust . T.stripPrefix "dbSpaceUser_")

type DbDesk :: Type
data DbDesk = MkDbDesk
  { dbDesk_id :: Selda.ID DbDesk
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
  { dbReservation_id :: Selda.ID DbReservation
  , dbReservation_desk :: Selda.ID DbDesk
  , dbReservation_user :: Selda.ID DbUser
  , dbReservation_time_begin :: Selda.UTCTime
  , dbReservation_time_end :: Selda.UTCTime
  , dbReservation_cancelled :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

tableReservation :: Selda.Table DbReservation
tableReservation =
  Selda.tableFieldMod
    "reservation"
    [ #dbReservation_id Selda.:- Selda.autoPrimary
    , #dbReservation_desk Selda.:- Selda.foreignKey tableDesk #dbDesk_id
    , #dbReservation_user Selda.:- Selda.foreignKey tableUser #dbUser_id
    ]
    (fromJust . T.stripPrefix "dbReservation_")
