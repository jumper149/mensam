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

userTable :: Selda.Table DbUser
userTable =
  Selda.tableFieldMod
    "user"
    [ #dbUser_id Selda.:- Selda.autoPrimary
    , #dbUser_name Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbUser_")

initDatabase :: MonadSeldaConnection m => m ()
initDatabase = runSeldaTransaction $ do
  Selda.createTable userTable
