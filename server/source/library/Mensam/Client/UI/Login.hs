{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Login where

import Mensam.API.Data.User.Username
import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.OrphanInstances
import Mensam.Client.UI.Brick.Draw
import Mensam.Client.UI.Brick.Events
import Mensam.Client.UI.Brick.Names

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.Trans.Class
import Data.Kind
import Data.Text qualified as T
import Graphics.Vty.Input.Events
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
    [ (str "Username: " <+>)
        @@= editField
          loginInfoUsername
          ClientNameLoginUsername
          (Just 1)
          id
          ( \case
              [line] -> either (const Nothing) (Just . unUsername) $ mkUsername line
              _ -> Nothing
          )
          (txt . T.intercalate "\n")
          id
    , (str "Password: " <+>) @@= editPasswordField loginInfoPassword ClientNameLoginPassword
    ]
    MkLoginInfo
      { _loginInfoUsername = ""
      , _loginInfoPassword = ""
      }

type ScreenLoginState :: Type
newtype ScreenLoginState = MkScreenLoginState
  { _screenStateLoginForm :: Form LoginInfo ClientEvent ClientName
  }
makeLenses ''ScreenLoginState

loginDraw :: ScreenLoginState -> [Widget ClientName]
loginDraw = \case
  MkScreenLoginState {_screenStateLoginForm = form} ->
    [ centerLayer $ borderWithLabel (txt "Login") $ cropRightTo 60 $ renderForm form
    , drawHelp
    ]

loginHandleEvent :: BrickEvent ClientName ClientEvent -> ApplicationT (EventM ClientName ScreenLoginState) ()
loginHandleEvent = \case
  VtyEvent (EvKey KEnter []) -> do
    s <- lift get
    case formState $ _screenStateLoginForm s of
      loginInfo -> do
        sendEvent $
          ClientEventSendRequestLogin $
            MkCredentials
              { credentialsUsername = loginInfo ^. loginInfoUsername
              , credentialsPassword = loginInfo ^. loginInfoPassword
              }
  event -> lift $ zoom screenStateLoginForm $ handleFormEvent event
