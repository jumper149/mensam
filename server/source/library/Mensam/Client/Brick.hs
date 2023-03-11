{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick where

import Brick
import Brick.Forms
import Data.Text qualified as T
import Graphics.Vty
import Lens.Micro.Platform
import Servant.RawM.Client ()
import Data.Kind

type ClientName :: Type
data ClientName
    = ClientNameLoginUsername
    | ClientNameLoginPassword
  deriving stock (Eq, Ord, Show)

type LoginState :: Type
data LoginState = MkLoginState
    { _loginStateUsername :: T.Text
    , _loginStatePassword :: T.Text
    }
makeLenses ''LoginState

loginFormInitial :: Form LoginState e ClientName
loginFormInitial =
  newForm
    [ (str "Username: " <+>) @@= editTextField loginStateUsername ClientNameLoginUsername (Just 1)
    , (str "Password: " <+>) @@= editTextField loginStatePassword ClientNameLoginPassword (Just 1)
    ]
    MkLoginState
      { _loginStateUsername = ""
      , _loginStatePassword = ""
      }

type ClientState :: Type -> Type
data ClientState e = MkClientState
  { _clientStateLogin :: Form LoginState e ClientName
  , _clientStateJWT :: Maybe T.Text
  }
makeLenses ''ClientState

type ClientEvent :: Type
data ClientEvent
    = ClientEventLoginRequire
    | ClientEventLoginSend
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = do
  let
    app :: App (ClientState ClientEvent) ClientEvent ClientName
    app = App
      { appDraw = \case
          MkClientState { _clientStateJWT = Nothing, _clientStateLogin = loginForm } -> [renderForm loginForm]
          MkClientState { _clientStateJWT = Just jwt } -> [txt jwt]
      , appChooseCursor = \_ cursors ->
          case cursors of
            [] -> Nothing
            x : _ -> pure x
      , appHandleEvent = \case
        --AppEvent ClientEventLoginRequire -> halt
        VtyEvent (EvKey KEnter []) -> do
            clientState <- get
            case _clientStateJWT clientState of
              Nothing -> undefined
              Just _ -> pure ()
            pure ()
        VtyEvent (EvKey KEsc []) -> halt
        event -> zoom clientStateLogin $ handleFormEvent event
      , appStartEvent = pure ()
      , appAttrMap = \_ -> attrMap defAttr []
      }
    initialState :: ClientState ClientEvent
    initialState = MkClientState
      { _clientStateLogin = loginFormInitial
      , _clientStateJWT = Nothing
      }
  _finalState <- defaultMain app initialState
  pure ()
