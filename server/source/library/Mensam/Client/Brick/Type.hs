{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Type where

import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.Client.Brick.Login
import Mensam.Client.Brick.Names
import Mensam.Client.Brick.Register

import Brick.Widgets.List
import Data.Kind
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Lens.Micro.Platform

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
