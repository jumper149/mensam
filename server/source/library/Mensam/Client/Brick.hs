module Mensam.Client.Brick where

import Mensam.Client.Brick.Type
import Mensam.Client.OrphanInstances
import Mensam.Server.Route.Type qualified as Route
import Mensam.Server.Route.User.Type qualified as Route.User

import Brick
import Brick.Forms
import Control.Monad.IO.Class
import Data.SOP
import Graphics.Vty
import Lens.Micro.Platform
import Network.HTTP.Client qualified as Network
import Servant
import Servant.Client
import Servant.RawM.Client ()

main :: IO ()
main = do
  clientEnv <- clientEnvDefault
  let
    app :: App ClientState () ClientName
    app =
      App
        { appDraw = draw
        , appChooseCursor = chooseCursor
        , appHandleEvent = handleEvent clientEnv
        , appStartEvent = pure ()
        , appAttrMap = \_ -> attrMap defAttr []
        }
    initialState :: ClientState
    initialState = ClientStateLogin loginFormInitial
  _finalState <- defaultMain app initialState
  pure ()

draw :: ClientState -> [Widget ClientName]
draw = \case
  ClientStateLogin form -> [renderForm form]
  ClientStateRegister form -> [renderForm form]
  _ -> [txt "Hello"]

handleEvent :: ClientEnv -> BrickEvent ClientName () -> EventM ClientName ClientState ()
handleEvent clientEnv = \case
  VtyEvent (EvKey KEsc []) -> halt
  VtyEvent (EvKey (KChar '1') [MAlt]) -> put $ ClientStateRegister registerFormInitial
  VtyEvent (EvKey (KChar '2') [MAlt]) -> put $ ClientStateLogin loginFormInitial
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
      ClientStateRegister form ->
        case event of
          VtyEvent (EvKey KEnter []) ->
            case formState form of
              registerInfo -> do
                result <-
                  liftIO $
                    flip runClientM clientEnv $
                      routes // Route.routeUser // Route.User.routeRegister $
                        Route.User.MkRequestRegister
                          { Route.User.requestRegisterName = undefined $ registerInfo ^. registerInfoUsername
                          , Route.User.requestRegisterPassword = registerInfo ^. registerInfoPassword
                          , Route.User.requestRegisterEmail = registerInfo ^. registerInfoEmail
                          , Route.User.requestRegisterEmailVisible = registerInfo ^. registerInfoEmailVisible
                          }
                case result of
                  Right (Z (I (WithStatus @200 ()))) ->
                    put $ ClientStateLogin loginFormInitial
                  _ -> pure ()
                pure ()
          _ -> zoom clientStateLoginForm $ handleFormEvent event
      _ -> pure ()

chooseCursor :: ClientState -> [CursorLocation ClientName] -> Maybe (CursorLocation ClientName)
chooseCursor _clientState cursors =
  case cursors of
    [] -> Nothing
    x : _ -> pure x

routes :: Route.Routes (AsClientT ClientM)
routes = client $ Proxy @(NamedRoutes Route.Routes)

clientEnvDefault :: IO ClientEnv
clientEnvDefault = do
  httpManager <- Network.newManager Network.defaultManagerSettings
  let baseUrl =
        BaseUrl
          { baseUrlScheme = Http
          , baseUrlHost = "localhost"
          , baseUrlPort = 8177
          , baseUrlPath = ""
          }
  pure $ mkClientEnv httpManager baseUrl
