{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Type where

import Brick
import Brick.Forms
import Data.Kind
import Data.Text qualified as T
import Lens.Micro.Platform

type ClientName :: Type
data ClientName
  = ClientNameLoginUsername
  | ClientNameLoginPassword
  deriving stock (Eq, Ord, Show)

type LoginInfo :: Type
data LoginInfo = MkLoginInfo
  { _loginInfoUsername :: T.Text
  , _loginInfoPassword :: T.Text
  }
makeLenses ''LoginInfo

loginFormInitial :: Form LoginInfo e ClientName
loginFormInitial =
  newForm
    [ (str "Username: " <+>) @@= editTextField loginInfoUsername ClientNameLoginUsername (Just 1)
    , (str "Password: " <+>) @@= editTextField loginInfoPassword ClientNameLoginPassword (Just 1)
    ]
    MkLoginInfo
      { _loginInfoUsername = ""
      , _loginInfoPassword = ""
      }

type ClientState :: Type -> Type
data ClientState e
  = ClientStateLogin {_clientStateLoginForm :: Form LoginInfo e ClientName}
  | ClientStateLoggedIn {_clientStateJwt :: T.Text}
makeLenses ''ClientState

type ClientEvent :: Type
data ClientEvent
  = ClientEventLoginRequire
  | ClientEventLoginSend
  deriving stock (Eq, Ord, Show)
