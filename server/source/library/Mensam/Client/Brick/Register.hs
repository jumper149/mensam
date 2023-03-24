{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Register where

import Mensam.Client.Brick.Names

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Kind
import Data.Text qualified as T
import Lens.Micro.Platform

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

registerDraw :: ScreenRegisterState -> [Widget ClientName]
registerDraw = \case
  MkScreenRegisterState {_screenStateRegisterForm = form} ->
    [ centerLayer $ borderWithLabel (txt "Register") $ cropRightTo 60 $ renderForm form
    ]
