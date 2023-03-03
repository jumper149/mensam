{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Mensam.User where

import Mensam.Application.SeldaConnection.Class

import Data.Aeson qualified as A
import Data.Kind
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text qualified as T
import Database.Selda qualified as Selda
import GHC.Generics
import Servant.Auth.JWT
import Servant.Auth.Server

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

type User :: Type
data User = MkUser
  { userName :: T.Text
  , userPasswordHash :: PasswordHash Bcrypt
  , userEmail :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

instance ToJWT User
instance A.ToJSON User where
  toJSON = undefined

instance FromBasicAuthData User where
  fromBasicAuthData BasicAuthData {basicAuthUsername, basicAuthPassword} (MkRunLoginInIO runLoginInIO) = runLoginInIO $ do
    let
      username = undefined basicAuthUsername
      password = undefined basicAuthPassword
    userLogin username password

type instance BasicAuthCfg = RunLoginInIO

type RunLoginInIO :: Type
data RunLoginInIO = forall m. MonadSeldaConnection m => MkRunLoginInIO {runLoginInIO :: m (AuthResult User) -> IO (AuthResult User)}

userLogin ::
  MonadSeldaConnection m =>
  -- | username
  T.Text ->
  Password ->
  m (AuthResult User)
userLogin username password = do
  matchingUsers :: [DbUser] <- runSeldaTransaction $ Selda.query $ do
    user <- Selda.select usersTable
    Selda.restrict $ user Selda.! #dbUser_name Selda..== Selda.literal username
    return user
  case matchingUsers of
    [] -> pure NoSuchUser
    [dbUser] -> do
      let user =
            MkUser
              { userName = dbUser_name dbUser
              , userPasswordHash = PasswordHash $ dbUser_password_hash dbUser
              , userEmail = dbUser_email dbUser
              }
      case checkPassword password (userPasswordHash user) of
        PasswordCheckFail -> pure BadPassword
        PasswordCheckSuccess -> pure $ Authenticated user
    _ : _ -> pure Indefinite -- error "Multiple users found."
