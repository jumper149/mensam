{-# LANGUAGE OverloadedLabels #-}

module Mensam.Database where

import Mensam.Application.SeldaConnection.Class

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
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

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
  { dbSpaceUser_id :: Selda.ID DbSpaceUser
  , dbSpaceUser_space :: Selda.ID DbSpace
  , dbSpaceUser_user :: Selda.ID DbUser
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

initDatabase :: MonadSeldaConnection m => m ()
initDatabase = runSeldaTransaction $ do
  Selda.createTable tableUser
  Selda.createTable tableSpace
  Selda.createTable tableSpaceUser
