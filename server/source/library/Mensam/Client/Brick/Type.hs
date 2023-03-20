{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Type where

import Mensam.API.Data.Desk
import Mensam.API.Data.Space

import Brick
import Brick.Forms
import Brick.Widgets.List
import Data.Kind
import Data.Sequence qualified as Seq
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
  | ClientNameSpacesList
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

type ScreenLoginState :: Type
newtype ScreenLoginState = MkScreenLoginState
  { _screenStateLoginForm :: Form LoginInfo () ClientName
  }
makeLenses ''ScreenLoginState

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

type ScreenRegisterState :: Type
newtype ScreenRegisterState = MkScreenRegisterState
  { _screenStateRegisterForm :: Form RegisterInfo () ClientName
  }
makeLenses ''ScreenRegisterState

spacesListInitial :: GenericList ClientName Seq.Seq Space
spacesListInitial =
  list
    ClientNameSpacesList
    mempty
    1

type ScreenSpacesState :: Type
newtype ScreenSpacesState = MkScreenSpacesState
  { _screenStateSpacesList :: GenericList ClientName Seq.Seq Space
  }
makeLenses ''ScreenSpacesState

desksListInitial :: GenericList ClientName Seq.Seq Desk
desksListInitial =
  list
    ClientNameSpacesList
    mempty
    1

type ScreenDesksState :: Type
data ScreenDesksState = MkScreenDesksState
  { _screenStateDesksSpace :: Space
  , _screenStateDesksList :: GenericList ClientName Seq.Seq Desk
  }
makeLenses ''ScreenDesksState

type ClientScreenState :: Type
data ClientScreenState
  = ClientScreenStateLogin {_clientScreenStateLogin :: ScreenLoginState}
  | ClientScreenStateRegister {_clientScreenStateRegister :: ScreenRegisterState}
  | ClientScreenStateSpaces {_clientScreenStateSpaces :: ScreenSpacesState}
  | ClientScreenStateDesks {_clientScreenStateDesks :: ScreenDesksState}
makeLenses ''ClientScreenState

type ClientState :: Type
data ClientState = MkClientState
  { _clientStateScreenState :: ClientScreenState
  , _clientStatePopup :: Maybe T.Text
  , _clientStateJwt :: Maybe T.Text
  }
makeLenses ''ClientState
