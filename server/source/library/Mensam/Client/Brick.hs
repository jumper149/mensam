{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick where

import Mensam.Client.OrphanInstances
import Mensam.Server.Route.Type qualified as Route
import Mensam.Server.Route.User.Type qualified as Route.User

import Brick
import Brick.Forms
import Control.Monad.IO.Class
import Data.Kind
import Data.SOP
import Data.Text qualified as T
import Graphics.Vty
import Lens.Micro.Platform
import Network.HTTP.Client qualified as Network
import Servant
import Servant.Client
import Servant.RawM.Client ()

routes :: Route.Routes (AsClientT ClientM)
routes = client $ Proxy @(NamedRoutes Route.Routes)

type ClientName :: Type
data ClientName
  = ClientNameLoginUsername
  | ClientNameLoginPassword
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

type ClientState :: Type -> Type
data ClientState e
  = ClientStateLogin {_clientStateLoginForm :: Form LoginInfo e ClientName}
  | ClientStateLoggedIn {_clientStateJwt :: T.Text}
makeLenses ''ClientState

type ClientEvent :: Type
data ClientEvent
  = ClientEventLoginRequire
  | ClientEventLoginSend
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = do
  httpManager <- Network.newManager Network.defaultManagerSettings
  let baseUrl =
        BaseUrl
          { baseUrlScheme = Http
          , baseUrlHost = "localhost"
          , baseUrlPort = 8177
          , baseUrlPath = ""
          }
  let clientEnv = mkClientEnv httpManager baseUrl
  let
    app :: App (ClientState ClientEvent) ClientEvent ClientName
    app =
      App
        { appDraw = \case
            ClientStateLogin form -> [renderForm form]
            _ -> [txt "Hello"]
        , appChooseCursor = \_ cursors ->
            case cursors of
              [] -> Nothing
              x : _ -> pure x
        , appHandleEvent = \case
            VtyEvent (EvKey KEsc []) -> halt
            -- AppEvent ClientEventLoginRequire -> halt
            event -> do
              clientState <- get
              case clientState of
                ClientStateLogin form ->
                  case event of
                    VtyEvent (EvKey KEnter []) ->
                      case formState form of
                        loginInfo -> do
                          result <-
                            liftIO $
                              flip runClientM clientEnv $
                                routes // Route.routeUser // Route.User.routeLogin $
                                  DataBasicAuth
                                    MkCredentials
                                      { credentialsUsername = loginInfo ^. loginInfoUsername
                                      , credentialsPassword = loginInfo ^. loginInfoPassword
                                      }
                          case result of
                            Right (Z (I (WithStatus @200 (Route.User.MkResponseLogin jwt)))) ->
                              put $ ClientStateLoggedIn jwt
                            _ -> pure ()

                          pure ()
                    _ -> zoom clientStateLoginForm $ handleFormEvent event
                _ -> pure ()
        , appStartEvent = pure ()
        , appAttrMap = \_ -> attrMap defAttr []
        }
    initialState :: ClientState ClientEvent
    initialState = ClientStateLogin loginFormInitial
  _finalState <- defaultMain app initialState
  pure ()
