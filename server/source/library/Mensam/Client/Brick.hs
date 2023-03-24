module Mensam.Client.Brick where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.API.Data.User.Username
import Mensam.API.Order
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.Application
import Mensam.Client.Application.MensamClient.Class
import Mensam.Client.Brick.Login
import Mensam.Client.Brick.Names
import Mensam.Client.Brick.Register
import Mensam.Client.Brick.Type
import Mensam.Client.OrphanInstances

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
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
  let
    app :: App ClientState () ClientName
    app =
      App
        { appDraw = draw
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
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
  ClientScreenStateLogin s -> loginDraw s <> [drawHelp]
  ClientScreenStateRegister s -> registerDraw s <> [drawHelp]
  ClientScreenStateSpaces (MkScreenSpacesState spaces) ->
    [ borderWithLabel (txt "Spaces") $
        padBottom Max $
          padRight Max $
            renderList (\_focus space -> txt $ T.pack ("#" <> show (unIdentifierSpace $ spaceId space) <> " ") <> spaceName space) True spaces
    ]
  ClientScreenStateDesks (MkScreenDesksState space desks) ->
    [ borderWithLabel (txt $ "Desks (" <> spaceName space <> ")") $
        padBottom Max $
          padRight Max $
            renderList (\_focus desk -> txt $ T.pack ("#" <> show (unIdentifierDesk $ deskId desk) <> " ") <> deskName desk) True desks
    ]

handleEvent :: BrickEvent ClientName () -> EventM ClientName ClientState ()
handleEvent = \case
  VtyEvent (EvKey KEsc []) -> halt
  VtyEvent (EvKey (KChar '1') [MMeta]) -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateRegister $ MkScreenRegisterState registerFormInitial}
  VtyEvent (EvKey (KChar '2') [MMeta]) -> modify $ \s -> s {_clientStateScreenState = ClientScreenStateLogin $ MkScreenLoginState loginFormInitial}
  VtyEvent (EvKey (KChar '3') [MMeta]) -> do
    clientState <- get
    case clientState ^. clientStateJwt of
      Just jwt -> do
        result <-
          runApplicationT $
            mensamCall $
              endpointSpaceList
                (DataJWT $ MkJWToken jwt)
                (Route.Booking.MkRequestSpaceList $ MkOrderByCategories [])
        case result of
          Right (Z (I (WithStatus @200 (Route.Booking.MkResponseSpaceList xs)))) -> do
            let l = listReplace (Seq.fromList xs) (Just 0) spacesListInitial
            modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState l)}
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
              loginInfo -> do
                result <-
                  runApplicationT $
                    mensamCall $
                      endpointLogin $
                        DataBasicAuth
                          MkCredentials
                            { credentialsUsername = loginInfo ^. loginInfoUsername
                            , credentialsPassword = loginInfo ^. loginInfoPassword
                            }
                case result of
                  Right (Z (I (WithStatus @200 (Route.User.MkResponseLogin jwt)))) -> do
                    modify $ \s -> s {_clientStateJwt = Just jwt}
                    resultSpaces <-
                      runApplicationT $
                        mensamCall $
                          endpointSpaceList
                            (DataJWT $ MkJWToken jwt)
                            (Route.Booking.MkRequestSpaceList $ MkOrderByCategories [])
                    case resultSpaces of
                      Right (Z (I (WithStatus @200 (Route.Booking.MkResponseSpaceList xs)))) -> do
                        let l = listReplace (Seq.fromList xs) (Just 0) spacesListInitial
                        modify $ \s -> s {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState l)}
                      err ->
                        modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
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
                  runApplicationT $
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
      MkClientState {_clientStateScreenState = ClientScreenStateSpaces (MkScreenSpacesState spaces)} ->
        case clientState ^. clientStateJwt of
          Just jwt ->
            case event of
              VtyEvent (EvKey (KChar 'r') []) -> do
                result <-
                  runApplicationT $
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
              VtyEvent (EvKey KEnter []) -> do
                case listSelectedElement spaces of
                  Nothing ->
                    modify $ \s -> s {_clientStatePopup = Just "No space selected."}
                  Just (_index, space) -> do
                    result <-
                      runApplicationT $
                        mensamCall $
                          endpointDeskList
                            (DataJWT $ MkJWToken jwt)
                            (Route.Booking.MkRequestDeskList $ Identifier $ spaceId space)
                    case result of
                      Right (Z (I (WithStatus @200 (Route.Booking.MkResponseDeskList desks)))) -> do
                        let l = listReplace (Seq.fromList desks) (Just 0) desksListInitial
                        modify $ \s -> s {_clientStateScreenState = ClientScreenStateDesks (MkScreenDesksState space l)}
                      err ->
                        modify $ \s -> s {_clientStatePopup = Just $ T.pack $ show err}
              VtyEvent e -> zoom (clientStateScreenState . clientScreenStateSpaces . screenStateSpacesList) $ handleListEvent e
              _ -> pure ()
          Nothing -> pure ()
      MkClientState {_clientStateScreenState = ClientScreenStateDesks (MkScreenDesksState space desks)} ->
        case clientState ^. clientStateJwt of
          Just jwt ->
            case event of
              VtyEvent (EvKey (KChar 'r') []) -> do
                result <-
                  runApplicationT $
                    mensamCall $
                      endpointDeskList
                        (DataJWT $ MkJWToken jwt)
                        (Route.Booking.MkRequestDeskList $ Identifier $ spaceId space)
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
