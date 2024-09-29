{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Register where

import Mensam.API.Data.User.Password
import Mensam.API.Data.User.Username
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.Application.Event.Class
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
import Mensam.Client.Application
import Text.Email.Text

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
    [ (str "Username: " <+>)
        @@= editField
          registerInfoUsername
          ClientNameRegisterUsername
          (Just 1)
          id
          ( \case
              [line] -> either (const Nothing) (Just . unUsername) $ mkUsername line
              _ -> Nothing
          )
          (txt . T.intercalate "\n")
          id
    , (str "Password: " <+>) @@= editPasswordField registerInfoPassword ClientNameRegisterPassword
    , (str "Email: " <+>)
        @@= editField
          registerInfoEmail
          ClientNameRegisterEmail
          (Just 1)
          id
          ( \case
              [line] -> either (const Nothing) (Just . toText) $ fromText line
              _ -> Nothing
          )
          (txt . T.intercalate "\n")
          id
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
  { _screenStateRegisterForm :: Form RegisterInfo ClientEvent ClientName
  }
makeLenses ''ScreenRegisterState

registerDraw :: ScreenRegisterState -> [Widget ClientName]
registerDraw = \case
  MkScreenRegisterState {_screenStateRegisterForm = form} ->
    [ centerLayer $ borderWithLabel (txt "Register") $ cropRightTo 60 $ renderForm form
    , drawHelp
    ]

registerHandleEvent :: BrickEvent ClientName ClientEvent -> ApplicationT (EventM ClientName ScreenRegisterState) ()
registerHandleEvent = \case
  VtyEvent (EvKey KEnter []) -> do
    s <- lift get
    case formState $ _screenStateRegisterForm s of
      registerInfo -> do
        sendEvent $
          ClientEventSendRequestRegister
            Route.User.MkRequestRegister
              { Route.User.requestRegisterName = MkUsernameUnsafe $ registerInfo ^. registerInfoUsername
              , Route.User.requestRegisterPassword = MkPasswordUnsafe $ registerInfo ^. registerInfoPassword
              , Route.User.requestRegisterEmail = fromTextUnsafe $ registerInfo ^. registerInfoEmail
              , Route.User.requestRegisterEmailVisible = registerInfo ^. registerInfoEmailVisible
              , Route.User.requestRegisterEmailNotifications = False
              }
  event -> lift $ zoom screenStateRegisterForm $ handleFormEvent event
