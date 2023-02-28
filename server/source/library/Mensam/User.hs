{-# LANGUAGE OverloadedLabels #-}

module Mensam.User where

import Data.Kind
import Data.Maybe
import Data.Text qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

type DbUser :: Type
data DbUser = MkDbUser
  { dbUser_id :: Selda.ID DbUser
  , dbUser_name :: Selda.Text
  , dbUser_password :: Selda.Text
  , dbUser_email :: Selda.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Selda.SqlRow)

usersTable :: Selda.Table DbUser
usersTable =
  Selda.tableFieldMod
    "users"
    [ #dbUser_id Selda.:- Selda.autoPrimary
    , #dbUser_name Selda.:- Selda.unique
    ]
    (fromJust . T.stripPrefix "dbUser_")

initDatabase :: (Selda.MonadMask m, Selda.MonadSelda m) => m ()
initDatabase = Selda.transaction $ do
  Selda.createTable usersTable
  Selda.insert_ usersTable [userMaxMustermann, userMartaMustermann]

userMaxMustermann :: DbUser
userMaxMustermann =
  MkDbUser
    { dbUser_id = Selda.def
    , dbUser_name = "Max Mustermann"
    , dbUser_password = "asdfg"
    , dbUser_email = "max-mustermann@gmail.com"
    }

userMartaMustermann :: DbUser
userMartaMustermann =
  MkDbUser
    { dbUser_id = Selda.def
    , dbUser_name = "Marta Mustermann"
    , dbUser_password = "abc"
    , dbUser_email = "marta-mustermann@gmx.com"
    }
