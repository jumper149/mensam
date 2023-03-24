{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Type where

import Mensam.Client.Brick.Desks
import Mensam.Client.Brick.Login
import Mensam.Client.Brick.Register
import Mensam.Client.Brick.Spaces

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
  , _clientStateJwt :: Maybe T.Text
  }
makeLenses ''ClientState
