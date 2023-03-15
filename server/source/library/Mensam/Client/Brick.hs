module Mensam.Client.Brick where

import Mensam.API.Order
import Mensam.API.Route.Booking.Type qualified as Route.Booking
import Mensam.API.Route.Type qualified as Route
import Mensam.API.Route.User.Type qualified as Route.User
import Mensam.API.Space
import Mensam.API.User.Username
import Mensam.Client.Brick.Type
import Mensam.Client.OrphanInstances

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Table
import Control.Monad.IO.Class
import Data.SOP
import Data.Text qualified as T
import Graphics.Vty
import Lens.Micro.Platform
import Network.HTTP.Client qualified as Network
import Servant
import Servant.Client
import Servant.RawM.Client ()
import Text.Email.Text

runBrick :: IO ()
runBrick = do
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
  ClientStateLogin form -> [centerLayer $ border $ cropRightTo 60 $ renderForm form, hCenter $ txt "Login"]
  ClientStateRegister form -> [centerLayer $ border $ cropRightTo 60 $ renderForm form, hCenter $ txt "Register"]
  ClientStateLoggedIn jwt spaces -> [hCenter (txt "Logged in") <=> hCenter (txt jwt) <=> borderWithLabel (txt "Spaces") (padBottom Max $ padRight Max $ renderTable $ table $ [txt "id", txt "name"] : ((\space -> [txt $ T.pack $ show $ unIdentifierSpace $ spaceId space, txt $ spaceName space]) <$> spaces))]

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
                          , Route.User.requestRegisterEmail = fromTextUnsafe $ registerInfo ^. registerInfoEmail
                          , Route.User.requestRegisterEmailVisible = registerInfo ^. registerInfoEmailVisible
                          }
                case result of
                  Right (Z (I (WithStatus @201 ()))) ->
                    put $ ClientStateLogin loginFormInitial
                  _ -> pure ()
                pure ()
          _ -> zoom clientStateRegisterForm $ handleFormEvent event
      ClientStateLoggedIn jwt _spaces ->
        case event of
          VtyEvent (EvKey KEnter []) -> do
            result <-
              liftIO $
                flip runClientM clientEnv $
                  (routes // Route.routeBooking // Route.Booking.routeSpaceList)
                    (DataJWT $ MkJWToken jwt)
                    (Route.Booking.MkRequestSpaceList $ MkOrderByCategories [])
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