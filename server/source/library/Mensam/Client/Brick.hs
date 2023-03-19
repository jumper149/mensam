module Mensam.Client.Brick where

import Mensam.API.Data.Space
import Mensam.API.Data.User.Username
import Mensam.API.Order
import Mensam.API.Route.Booking.Type qualified as Route.Booking
import Mensam.API.Route.Type qualified as Route
import Mensam.API.Route.User.Type qualified as Route.User
import Mensam.Client.Brick.Type
import Mensam.Client.OrphanInstances

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Monad.IO.Class
import Data.SOP
import Data.Sequence qualified as Seq
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
    initialState =
      MkClientState
        { _clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState {_screenStateLoginForm = loginFormInitial}
        , _clientStatePopup = Nothing
        , _clientStateJwt = Nothing
        }
  _finalState <- defaultMain app initialState
  pure ()

draw :: ClientState -> [Widget ClientName]
draw MkClientState {_clientStateScreenState, _clientStatePopup} =
  case _clientStatePopup of
    Nothing -> drawScreen _clientStateScreenState
    Just popup -> [center $ borderWithLabel (txt "Error") $ txt popup]

drawHelp :: Widget a
drawHelp =
  vBox
    [ txt title
    , padTop Max (padLeft Max (txt "Exit (Escape) | Register (Alt-1) | Login (Alt-2) | Spaces (Alt-3)"))
    ]
 where
  title :: T.Text
  title =
    "  __  __                             \n\
    \ |  \\/  | ___  _ _   ___ __ _  _ __  \n\
    \ | |\\/| |/ -_)| ' \\ (_-// _` || '  \\ \n\
    \ |_|  |_|\\___||_||_|/__/\\__/_||_|_|_|\n"

drawScreen :: ClientScreenState -> [Widget ClientName]
drawScreen = \case
  ClientScreenStateLogin (MkScreenLoginState form) ->
    [ centerLayer $ borderWithLabel (txt "Login") $ cropRightTo 60 $ renderForm form
    , drawHelp
    ]
  ClientScreenStateRegister (MkScreenRegisterState form) ->
    [ centerLayer $ borderWithLabel (txt "Register") $ cropRightTo 60 $ renderForm form
    , drawHelp
    ]
  ClientScreenStateSpaces (MkScreenSpacesState spaces) ->
    [ borderWithLabel (txt "Spaces") $
        padBottom Max $
          padRight Max $
            renderList (\_focus space -> txt $ T.pack ("#" <> show (unIdentifierSpace $ spaceId space) <> " ") <> spaceName space) True $
              list
                ClientNameSpacesList
                (Seq.fromList spaces)
                1
    ]

handleEvent :: ClientEnv -> BrickEvent ClientName () -> EventM ClientName ClientState ()
handleEvent clientEnv = \case
  VtyEvent (EvKey KEsc []) -> halt
  VtyEvent (EvKey (KChar '1') [MMeta]) -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateRegister $ MkScreenRegisterState registerFormInitial}
  VtyEvent (EvKey (KChar '2') [MMeta]) -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState loginFormInitial}
  VtyEvent (EvKey (KChar '3') [MMeta]) -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces $ MkScreenSpacesState []}
  event -> do
    clientState <- get
    case clientState of
      MkClientState {_clientStatePopup = Just _popup} ->
        case event of
          VtyEvent (EvKey KEnter []) ->
            modify $ \s -> s {_clientStatePopup = Nothing}
          _ -> pure ()
      MkClientState {_clientStateScreenState = ClientScreenStateLogin (MkScreenLoginState form)} ->
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
                    modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState []), _clientStateJwt = Just jwt}
                  err ->
                    modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
                pure ()
          _ -> zoom (clientStateScreenState . clientScreenStateLogin . screenStateLoginForm) $ handleFormEvent event
      MkClientState {_clientStateScreenState = ClientScreenStateRegister (MkScreenRegisterState form)} ->
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
                    modify $ \s -> s {_clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState loginFormInitial}
                  err ->
                    modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
                pure ()
          _ -> zoom (clientStateScreenState . clientScreenStateRegister . screenStateRegisterForm) $ handleFormEvent event
      MkClientState {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState _spaces)} ->
        case clientState ^. clientStateJwt of
          Just jwt ->
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
                    modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState xs)}
                  err ->
                    modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
                pure ()
              _ -> pure ()
          Nothing -> pure ()

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
