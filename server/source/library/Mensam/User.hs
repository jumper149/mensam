{-# LANGUAGE OverloadedLabels #-}

module Mensam.User where

import Mensam.Application.SeldaConnection.Class

import Control.Monad.IO.Class
import Data.Kind
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text qualified as T
import Database.Selda qualified as Selda
import GHC.Generics

type User :: Type
data User = MkUser
  { userName :: T.Text
  , userPasswordHash :: PasswordHash Bcrypt
  , userEmail :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

type DbUser :: Type
data DbUser = MkDbUser
  { dbUser_id :: Selda.ID DbUser
  , dbUser_name :: Selda.Text
  , dbUser_password_hash :: Selda.Text
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

initDatabase :: MonadSeldaConnection m => m ()
initDatabase = runSeldaTransaction $ do
  Selda.createTable usersTable
  Selda.insert_ usersTable [userMaxMustermann, userMartaMustermann]

userMaxMustermann :: DbUser
userMaxMustermann =
  MkDbUser
    { dbUser_id = Selda.def
    , dbUser_name = "Max Mustermann"
    , dbUser_password_hash = "asdfg"
    , dbUser_email = "max-mustermann@gmail.com"
    }

userMartaMustermann :: DbUser
userMartaMustermann =
  MkDbUser
    { dbUser_id = Selda.def
    , dbUser_name = "Marta Mustermann"
    , dbUser_password_hash = "abc"
    , dbUser_email = "marta-mustermann@gmx.com"
    }

type RequestUserCreate :: Type
data RequestUserCreate = MkRequestUserCreate
  { requestUserCreateName :: T.Text
  , requestUserCreatePassword :: Password
  , requestUserCreateEmail :: T.Text
  }

userCreate :: (MonadIO m, MonadSeldaConnection m) => RequestUserCreate -> m ()
userCreate request = do
  passwordHash :: PasswordHash Bcrypt <- hashPassword $ requestUserCreatePassword request
  let user =
        MkUser
          { userName = requestUserCreateName request
          , userPasswordHash = passwordHash
          , userEmail = requestUserCreateEmail request
          }
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = requestUserCreateName request
          , dbUser_password_hash = unPasswordHash $ userPasswordHash user
          , dbUser_email = requestUserCreateEmail request
          }
  runSeldaTransaction $
    Selda.insert_ usersTable [dbUser]
  pure ()

type RequestUserLogin :: Type
data RequestUserLogin = MkRequestUserLogin
  { requestUserLoginName :: T.Text
  , requestUserLoginPassword :: Password
  }

userLogin :: MonadSeldaConnection m => RequestUserLogin -> m User
userLogin request = do
  matchingUsers :: [DbUser] <- runSeldaTransaction $ Selda.query $ do
    user <- Selda.select usersTable
    Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal (requestUserLoginName request)
    return user
  user <- case matchingUsers of
    [] -> error "No user found."
    [dbUser] ->
      pure $
        MkUser
          { userName = dbUser_name dbUser
          , userPasswordHash = PasswordHash $ dbUser_password_hash dbUser
          , userEmail = dbUser_email dbUser
          }
    _ : _ -> error "Multiple users found."
  case checkPassword (requestUserLoginPassword request) (userPasswordHash user) of
    PasswordCheckFail -> error "Wrong password."
    PasswordCheckSuccess -> pure user
