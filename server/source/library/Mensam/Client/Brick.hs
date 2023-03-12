module Mensam.Client.Brick where

import Mensam.API.User.Username
import Mensam.Client.Brick.Type
import Mensam.Client.OrphanInstances
import Mensam.Server.Server.Route.Booking.Type qualified as Route.Booking
import Mensam.Server.Server.Route.Type qualified as Route
import Mensam.Server.Server.Route.User.Type qualified as Route.User

import Brick
import Brick.Forms
import Control.Monad.IO.Class
import Data.SOP
import Data.Text qualified as T
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
        , appChooseCursor = showFirstCursor
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
  ClientStateLoggedIn jwt spaces -> [txt $ "Logged in: " <> jwt <> "\n" <> T.pack (show spaces)]

handleEvent :: ClientEnv -> BrickEvent ClientName () -> EventM ClientName ClientState ()
handleEvent clientEnv = \case
  VtyEvent (EvKey KEsc []) -> halt
  VtyEvent (EvKey (KChar '1') [MMeta]) -> put $ ClientStateRegister registerFormInitial
  VtyEvent (EvKey (KChar '2') [MMeta]) -> put $ ClientStateLogin loginFormInitial
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
                    put $ ClientStateLoggedIn jwt []
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
                          { Route.User.requestRegisterName = MkUsernameUnsafe $ registerInfo ^. registerInfoUsername
                          , Route.User.requestRegisterPassword = registerInfo ^. registerInfoPassword
                          , Route.User.requestRegisterEmail = registerInfo ^. registerInfoEmail
                          , Route.User.requestRegisterEmailVisible = registerInfo ^. registerInfoEmailVisible
                          }
                case result of
                  Right (Z (I (WithStatus @200 ()))) ->
                    put $ ClientStateLogin loginFormInitial
                  _ -> pure ()
                pure ()
          _ -> zoom clientStateRegisterForm $ handleFormEvent event
      ClientStateLoggedIn jwt spaces ->
        case event of
          VtyEvent (EvKey KEnter []) -> do
            result <-
              liftIO $
                flip runClientM clientEnv $
                  (routes // Route.routeBooking // Route.Booking.routeSpaceList)
                    (DataJWT $ MkJWToken jwt)
                    (Route.Booking.MkRequestSpaceList ())
            case result of
              Right (Z (I (WithStatus @200 (Route.Booking.MkResponseSpaceList xs)))) ->
                put $ ClientStateLoggedIn jwt xs
              _ -> pure ()
            pure ()
      _ -> pure ()

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
