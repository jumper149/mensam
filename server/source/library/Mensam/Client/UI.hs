module Mensam.Client.UI where

import Mensam.API.Aeson
import Mensam.API.Data.Space
import Mensam.API.Order
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.Application.MensamClient.Class
import Mensam.Client.OrphanInstances
import Mensam.Client.UI.Brick.AttrMap
import Mensam.Client.UI.Brick.Events
import Mensam.Client.UI.Brick.Names
import Mensam.Client.UI.Brick.State
import Mensam.Client.UI.Desks
import Mensam.Client.UI.Login
import Mensam.Client.UI.Menu
import Mensam.Client.UI.Register
import Mensam.Client.UI.Spaces

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Monad.IO.Class
import Data.SOP
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time qualified as T
import Graphics.Vty
import Lens.Micro.Platform
import Servant

ui :: IO ()
ui = do
  chan <- newBChan 10
  tz <- liftIO T.getCurrentTimeZone
  let
    app :: App ClientState ClientEvent ClientName
    app =
      App
        { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent chan
        , appStartEvent = pure ()
        , appAttrMap = const attrsDefault
        }
    initialState :: ClientState
    initialState =
      MkClientState
        { _clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState {_screenStateLoginForm = loginFormInitial}
        , _clientStatePopup = Nothing
        , _clientStateJwt = Nothing
        , _clientStateTimezone = tz
        }
    initVty = mkVty defaultConfig
  vty <- initVty
  _finalState <- customMain vty initVty (Just chan) app initialState
  pure ()

draw :: ClientState -> [Widget ClientName]
draw MkClientState {_clientStateScreenState, _clientStatePopup} =
  case _clientStatePopup of
    Nothing -> drawScreen _clientStateScreenState
    Just popup ->
      let
        foreground =
          updateAttrMap (const attrsForeground) $
            putCursor ClientNamePopupButton (Location (0, 0)) $
              hLimitPercent 60 $
                vLimitPercent 60 $
                  vBox
                    [ borderWithLabel (txt "Error") $ txtWrap popup
                    , padLeft Max $ txt " OK (Enter) "
                    ]
        backgrounds = (updateAttrMap (const attrsBackground) <$> drawScreen _clientStateScreenState)
       in
        centerLayer foreground : backgrounds
 where
  drawScreen :: ClientScreenState -> [Widget ClientName]
  drawScreen = \case
    ClientScreenStateLogin s -> loginDraw s
    ClientScreenStateRegister s -> registerDraw s
    ClientScreenStateSpaces s -> spacesDraw s
    ClientScreenStateDesks s -> desksDraw s
    ClientScreenStateMenu s -> menuDraw s

handleEvent :: BChan ClientEvent -> BrickEvent ClientName ClientEvent -> EventM ClientName ClientState ()
handleEvent chan = \case
  AppEvent event ->
    case event of
      ClientEventSwitchToScreenLogin -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState loginFormInitial}
      ClientEventSwitchToScreenRegister -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateRegister $ MkScreenRegisterState registerFormInitial}
      ClientEventSwitchToScreenSpaces -> do
        clientState <- get
        case clientState ^. clientStateJwt of
          Just jwt -> do
            result <-
              runApplicationT chan $
                mensamCall $
                  endpointSpaceList
                    (DataJWT jwt)
                    (Route.Booking.MkRequestSpaceList $ MkOrderByCategories [])
            case result of
              Right (Z (I (WithStatus @200 (Route.Booking.MkResponseSpaceList xs)))) -> do
                let l = listReplace (Seq.fromList xs) (Just 0) spacesListInitial
                modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState l False Nothing)}
              err -> modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
          Nothing -> modify $ \s -> s {_clientStatePopup = Just "Error: Not logged in."}
      ClientEventSwitchToScreenDesks space -> do
        clientState <- get
        case clientState ^. clientStateJwt of
          Just jwt -> do
            result <-
              runApplicationT chan $
                mensamCall $
                  endpointDeskList
                    (DataJWT jwt)
                    ( Route.Booking.MkRequestDeskList
                        { Route.Booking.requestDeskListSpace = Identifier $ spaceId space
                        , Route.Booking.requestDeskListTimeBegin = Nothing
                        , Route.Booking.requestDeskListTimeEnd = Nothing
                        }
                    )
            case result of
              Right (Z (I (WithStatus @200 (Route.Booking.MkResponseDeskList desks)))) -> do
                let l = listReplace (Seq.fromList desks) (Just 0) desksListInitial
                currentDay <- T.utctDay <$> liftIO T.getCurrentTime
                modify $ \s -> s {_clientStateScreenState = ClientScreenStateDesks (MkScreenDesksState space l False Nothing currentDay (_clientStateTimezone s) Nothing)}
              err ->
                modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
          Nothing -> modify $ \s -> s {_clientStatePopup = Just "Error: Not logged in."}
      ClientEventSwitchToScreenMenu -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateMenu $ MkScreenMenuState menuListInitial}
      ClientEventSendRequestLogin credentials -> do
        result <- runApplicationT chan $ mensamCall $ endpointLogin $ DataBasicAuth credentials
        case result of
          Right (Z (I (WithStatus @200 (Route.User.MkResponseLogin jwt _timeout)))) -> do
            modify $ \s -> s {_clientStateJwt = Just jwt}
            runApplicationT chan $ sendEvent ClientEventSwitchToScreenSpaces
          err -> modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
      ClientEventSendRequestRegister request -> do
        result <- runApplicationT chan $ mensamCall $ endpointRegister request
        case result of
          Right (Z (I (WithStatus @201 ()))) -> runApplicationT chan $ sendEvent ClientEventSwitchToScreenLogin
          err -> modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
      ClientEventSendRequestCreateSpace request -> do
        clientState <- get
        case clientState ^. clientStateJwt of
          Just jwt -> do
            result <- runApplicationT chan $ mensamCall $ endpointSpaceCreate (DataJWT jwt) request
            case result of
              Right (Z (I (WithStatus @201 (Route.Booking.MkResponseSpaceCreate _)))) -> runApplicationT chan $ sendEvent ClientEventSwitchToScreenSpaces
              err -> modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
          Nothing -> modify $ \s -> s {_clientStatePopup = Just "Error: Not logged in."}
      ClientEventSendRequestCreateDesk space request -> do
        clientState <- get
        case clientState ^. clientStateJwt of
          Just jwt -> do
            result <- runApplicationT chan $ mensamCall $ endpointDeskCreate (DataJWT jwt) request
            case result of
              Right (Z (I (WithStatus @201 (Route.Booking.MkResponseDeskCreate _)))) -> runApplicationT chan $ sendEvent (ClientEventSwitchToScreenDesks space)
              err -> modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
          Nothing -> modify $ \s -> s {_clientStatePopup = Just "Error: Not logged in."}
      ClientEventSendRequestCreateReservation space request -> do
        clientState <- get
        case clientState ^. clientStateJwt of
          Just jwt -> do
            result <- runApplicationT chan $ mensamCall $ endpointReservationCreate (DataJWT jwt) request
            case result of
              Right (Z (I (WithStatus @201 (Route.Booking.MkResponseReservationCreate _)))) -> runApplicationT chan $ sendEvent (ClientEventSwitchToScreenDesks space)
              err -> modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
          Nothing -> modify $ \s -> s {_clientStatePopup = Just "Error: Not logged in."}
  VtyEvent (EvKey KEsc []) -> do
    s <- get
    case _clientStateScreenState s of
      ClientScreenStateMenu _ ->
        zoom (clientStateScreenState . clientScreenStateMenu) $ runApplicationT chan $ menuHandleEvent (VtyEvent (EvKey KEsc []))
      _ -> runApplicationT chan $ sendEvent ClientEventSwitchToScreenMenu
  VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt
  event -> do
    clientState <- get
    case clientState of
      MkClientState {_clientStatePopup = Just _popup} ->
        case event of
          VtyEvent (EvKey KEnter []) ->
            modify $ \s -> s {_clientStatePopup = Nothing}
          _ -> pure ()
      MkClientState {_clientStateScreenState = ClientScreenStateLogin _} ->
        zoom (clientStateScreenState . clientScreenStateLogin) $ runApplicationT chan $ loginHandleEvent event
      MkClientState {_clientStateScreenState = ClientScreenStateRegister _} ->
        zoom (clientStateScreenState . clientScreenStateRegister) $ runApplicationT chan $ registerHandleEvent event
      MkClientState {_clientStateScreenState = ClientScreenStateSpaces _} ->
        zoom (clientStateScreenState . clientScreenStateSpaces) $ runApplicationT chan $ spacesHandleEvent event
      MkClientState {_clientStateScreenState = ClientScreenStateDesks _} ->
        zoom (clientStateScreenState . clientScreenStateDesks) $ runApplicationT chan $ desksHandleEvent event
      MkClientState {_clientStateScreenState = ClientScreenStateMenu _} ->
        zoom (clientStateScreenState . clientScreenStateMenu) $ runApplicationT chan $ menuHandleEvent event
