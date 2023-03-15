{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Type where

import Mensam.API.Space

import Brick
import Brick.Forms
import Data.Kind
import Data.Text qualified as T
import Lens.Micro.Platform

type ClientName :: Type
data ClientName
  = ClientNameLoginUsername
  | ClientNameLoginPassword
  | ClientNameRegisterUsername
  | ClientNameRegisterPassword
  | ClientNameRegisterEmail
  | ClientNameRegisterEmailVisible
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

type RegisterInfo :: Type
data RegisterInfo = MkRegisterInfo
  { _registerInfoUsername :: T.Text
  , _registerInfoPassword :: T.Text
  , _registerInfoEmail :: T.Text
  , _registerInfoEmailVisible :: Bool
  }
makeLenses ''RegisterInfo

registerFormInitial :: Form RegisterInfo e ClientName
registerFormInitial =
  newForm
    [ (str "Username: " <+>) @@= editTextField registerInfoUsername ClientNameRegisterUsername (Just 1)
    , (str "Password: " <+>) @@= editTextField registerInfoPassword ClientNameRegisterPassword (Just 1)
    , (str "Email: " <+>) @@= editTextField registerInfoEmail ClientNameRegisterEmail (Just 1)
    , checkboxField registerInfoEmailVisible ClientNameRegisterEmailVisible "Email visible"
    ]
    MkRegisterInfo
      { _registerInfoUsername = ""
      , _registerInfoPassword = ""
      , _registerInfoEmail = ""
      , _registerInfoEmailVisible = False
      }

type ClientState :: Type
data ClientState
  = ClientStateLogin {_clientStateLoginForm :: Form LoginInfo () ClientName}
  | ClientStateRegister {_clientStateRegisterForm :: Form RegisterInfo () ClientName}
  | ClientStateLoggedIn {_clientStateJwt :: T.Text, _clientStateSpaces :: [Space]}
makeLenses ''ClientState