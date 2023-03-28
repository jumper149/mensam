module Mensam.Client.Brick where

import Mensam.API.Aeson
import Mensam.API.Data.Space
import Mensam.API.Data.User.Username
import Mensam.API.Order
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.Application.MensamClient.Class
import Mensam.Client.Brick.Desks
import Mensam.Client.Brick.Draw
import Mensam.Client.Brick.Events
import Mensam.Client.Brick.Login
import Mensam.Client.Brick.Names
import Mensam.Client.Brick.Register
import Mensam.Client.Brick.Spaces
import Mensam.Client.Brick.Type
import Mensam.Client.OrphanInstances

import Brick
import Brick.BChan
import Brick.Forms
import Brick.Widgets.List
import Data.SOP
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Graphics.Vty
import Lens.Micro.Platform
import Servant
import Servant.RawM.Client ()
import Text.Email.Text

runBrick :: IO ()
runBrick = do
  chan <- newBChan 10
  let
    app :: App ClientState ClientEvent ClientName
    app =
      App
        { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent chan
        , appStartEvent = pure ()
        , appAttrMap = \_ ->
            attrMap
              defAttr
              [ (listAttr, defAttr)
              , (listSelectedAttr, defAttr `withStyle` standout)
              ]
        }
    initialState :: ClientState
    initialState =
      MkClientState
        { _clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState {_screenStateLoginForm = loginFormInitial}
        , _clientStatePopup = Nothing
        , _clientStateJwt = Nothing
        }
    initVty = mkVty defaultConfig
  vty <- initVty
  _finalState <- customMain vty initVty (Just chan) app initialState
  pure ()

handleEvent :: BChan ClientEvent -> BrickEvent ClientName ClientEvent -> EventM ClientName ClientState ()
handleEvent chan = \case
  AppEvent event ->
    case event of
      ClientEventSwitchToScreenLogin -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState loginFormInitial}
      ClientEventSwitchToScreenRegister -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateRegister $ MkScreenRegisterState registerFormInitial}
      ClientEventSendRequestLogin credentials -> do
        result <- runApplicationT chan $ mensamCall $ endpointLogin $ DataBasicAuth credentials
        case result of
          Right (Z (I (WithStatus @200 (Route.User.MkResponseLogin jwt)))) -> do
            modify $ \s -> s {_clientStateJwt = Just jwt}
            resultSpaces <-
              runApplicationT chan $
                mensamCall $
                  endpointSpaceList
                    (DataJWT $ MkJWToken jwt)
                    (Route.Booking.MkRequestSpaceList $ MkOrderByCategories [])
            case resultSpaces of
              Right (Z (I (WithStatus @200 (Route.Booking.MkResponseSpaceList xs)))) -> do
                let l = listReplace (Seq.fromList xs) (Just 0) spacesListInitial
                modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState l Nothing)}
              err ->
                modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
          err ->
            modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
  VtyEvent (EvKey KEsc []) -> halt
  VtyEvent (EvKey (KChar '1') [MMeta]) -> runApplicationT chan $ sendEvent ClientEventSwitchToScreenRegister
  VtyEvent (EvKey (KChar '2') [MMeta]) -> runApplicationT chan $ sendEvent ClientEventSwitchToScreenLogin
  VtyEvent (EvKey (KChar '3') [MMeta]) -> do
    clientState <- get
    case clientState ^. clientStateJwt of
      Just jwt -> do
        result <-
          runApplicationT chan $
            mensamCall $
              endpointSpaceList
                (DataJWT $ MkJWToken jwt)
                (Route.Booking.MkRequestSpaceList $ MkOrderByCategories [])
        case result of
          Right (Z (I (WithStatus @200 (Route.Booking.MkResponseSpaceList xs)))) -> do
            let l = listReplace (Seq.fromList xs) (Just 0) spacesListInitial
            modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState l Nothing)}
          err ->
            modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
      Nothing ->
        modify $ \s -> s {_clientStatePopup = Just "Error: Not logged in."}
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
              loginInfo ->
                runApplicationT chan $
                  sendEvent $
                    ClientEventSendRequestLogin $
                      MkCredentials
                        { credentialsUsername = loginInfo ^. loginInfoUsername
                        , credentialsPassword = loginInfo ^. loginInfoPassword
                        }
          _ -> zoom (clientStateScreenState . clientScreenStateLogin . screenStateLoginForm) $ handleFormEvent event
      MkClientState {_clientStateScreenState = ClientScreenStateRegister (MkScreenRegisterState form)} ->
        case event of
          VtyEvent (EvKey KEnter []) ->
            case formState form of
              registerInfo -> do
                result <-
                  runApplicationT chan $
                    mensamCall $
                      endpointRegister
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
      MkClientState {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState spaces createForm)} ->
        case clientState ^. clientStateJwt of
          Just jwt ->
            case createForm of
              Nothing ->
                case event of
                  VtyEvent (EvKey (KChar 'r') []) -> do
                    result <-
                      runApplicationT chan $
                        mensamCall $
                          endpointSpaceList
                            (DataJWT $ MkJWToken jwt)
                            (Route.Booking.MkRequestSpaceList $ MkOrderByCategories [])
                    case result of
                      Right (Z (I (WithStatus @200 (Route.Booking.MkResponseSpaceList xs)))) ->
                        clientStateScreenState . clientScreenStateSpaces . screenStateSpacesList %= listReplace (Seq.fromList xs) (Just 0)
                      err ->
                        modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
                    pure ()
                  VtyEvent (EvKey (KChar 'c') []) -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces $ MkScreenSpacesState spaces $ Just newSpaceFormInitial}
                  VtyEvent (EvKey KEnter []) -> do
                    case listSelectedElement spaces of
                      Nothing ->
                        modify $ \s -> s {_clientStatePopup = Just "No space selected."}
                      Just (_index, space) -> do
                        result <-
                          runApplicationT chan $
                            mensamCall $
                              endpointDeskList
                                (DataJWT $ MkJWToken jwt)
                                ( Route.Booking.MkRequestDeskList
                                    { Route.Booking.requestDeskListSpace = Identifier $ spaceId space
                                    , Route.Booking.requestDeskListTimeBegin = Nothing
                                    , Route.Booking.requestDeskListTimeEnd = Nothing
                                    }
                                )
                        case result of
                          Right (Z (I (WithStatus @200 (Route.Booking.MkResponseDeskList desks)))) -> do
                            let l = listReplace (Seq.fromList desks) (Just 0) desksListInitial
                            modify $ \s -> s {_clientStateScreenState = ClientScreenStateDesks (MkScreenDesksState space l)}
                          err ->
                            modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
                  VtyEvent e -> zoom (clientStateScreenState . clientScreenStateSpaces . screenStateSpacesList) $ handleListEvent e
                  _ -> pure ()
              Just form ->
                case event of
                  VtyEvent (EvKey KEnter []) ->
                    case formState form of
                      newSpaceInfo -> do
                        result <-
                          runApplicationT chan $
                            mensamCall $
                              endpointSpaceCreate
                                (DataJWT $ MkJWToken jwt)
                                Route.Booking.MkRequestSpaceCreate
                                  { Route.Booking.requestSpaceCreateName = MkNameSpace $ newSpaceInfo ^. newSpaceInfoName
                                  , Route.Booking.requestSpaceCreateVisibility = newSpaceInfo ^. newSpaceInfoVisibility
                                  , Route.Booking.requestSpaceCreateAccessibility = newSpaceInfo ^. newSpaceInfoAccessibility
                                  }
                        case result of
                          Right (Z (I (WithStatus @201 (Route.Booking.MkResponseSpaceCreate _identifier)))) ->
                            -- TODO: Send event to list spaces.
                            clientStateScreenState . clientScreenStateSpaces . screenStateSpacesNewSpaceForm %= pure Nothing
                          err ->
                            modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
                        pure ()
                  _ ->
                    zoom (clientStateScreenState . clientScreenStateSpaces . screenStateSpacesNewSpaceForm . _Just) $ handleFormEvent event
          Nothing -> pure ()
      MkClientState {_clientStateScreenState = ClientScreenStateDesks (MkScreenDesksState space desks)} ->
        case clientState ^. clientStateJwt of
          Just jwt ->
            case event of
              VtyEvent (EvKey (KChar 'r') []) -> do
                result <-
                  runApplicationT chan $
                    mensamCall $
                      endpointDeskList
                        (DataJWT $ MkJWToken jwt)
                        ( Route.Booking.MkRequestDeskList
                            { Route.Booking.requestDeskListSpace = Identifier $ spaceId space
                            , Route.Booking.requestDeskListTimeBegin = Nothing
                            , Route.Booking.requestDeskListTimeEnd = Nothing
                            }
                        )
                case result of
                  Right (Z (I (WithStatus @200 (Route.Booking.MkResponseDeskList xs)))) ->
                    clientStateScreenState . clientScreenStateDesks . screenStateDesksList %= listReplace (Seq.fromList xs) (Just 0)
                  err ->
                    modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
                pure ()
              VtyEvent (EvKey KEnter []) -> do
                case listSelectedElement desks of
                  Nothing ->
                    modify $ \s -> s {_clientStatePopup = Just "No desk selected."}
                  Just (_index, desk) -> do
                    modify $ \s -> s {_clientStatePopup = Just $ "Desk selected: " <> T.pack (show desk)}
              VtyEvent e -> zoom (clientStateScreenState . clientScreenStateDesks . screenStateDesksList) $ handleListEvent e
              _ -> pure ()
          Nothing -> pure ()
