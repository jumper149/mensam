{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Brick.State where

import Mensam.API.Route.Api.User
import Mensam.Client.UI.Desks
import Mensam.Client.UI.Login
import Mensam.Client.UI.Register
import Mensam.Client.UI.Spaces

import Data.Kind
import Data.Text qualified as T
import Lens.Micro.Platform

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
  , _clientStateJwt :: Maybe Jwt
  }
makeLenses ''ClientState
