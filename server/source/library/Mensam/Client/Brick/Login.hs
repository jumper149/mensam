{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Login where

import Mensam.Client.Brick.Names

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Kind
import Data.Text qualified as T
import Lens.Micro.Platform

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

loginDraw :: ScreenLoginState -> [Widget ClientName]
loginDraw = \case
  MkScreenLoginState {_screenStateLoginForm = form} ->
    [ centerLayer $ borderWithLabel (txt "Login") $ cropRightTo 60 $ renderForm form
    ]
