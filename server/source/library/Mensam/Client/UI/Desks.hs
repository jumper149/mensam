{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Desks where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.API.Route.Api.Booking
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.UI.Brick.Events
import Mensam.Client.UI.Brick.Names

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Monad.Trans.Class
import Data.Kind
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Graphics.Vty.Input.Events
import Lens.Micro.Platform

desksListInitial :: GenericList ClientName Seq.Seq DeskWithInfo
desksListInitial =
  list
    ClientNameSpacesList
    mempty
    1

type NewDeskInfo :: Type
newtype NewDeskInfo = MkLoginInfo
  { _newDeskInfoName :: T.Text
  }
makeLenses ''NewDeskInfo

newDeskFormInitial :: Form NewDeskInfo e ClientName
newDeskFormInitial =
  newForm
    [ (str "Name: " <+>) @@= editTextField newDeskInfoName ClientNameDesksNewDeskName (Just 1)
    ]
    MkLoginInfo
      { _newDeskInfoName = ""
      }

type ScreenDesksState :: Type
data ScreenDesksState = MkScreenDesksState
  { _screenStateDesksSpace :: Space
  , _screenStateDesksList :: GenericList ClientName Seq.Seq DeskWithInfo
  , _screenStateDesksShowHelp :: Bool
  , _screenStateDesksShowReservations :: Maybe T.Text
  , _screenStateDesksNewDeskForm :: Maybe (Form NewDeskInfo ClientEvent ClientName)
  }
makeLenses ''ScreenDesksState

desksDraw :: ScreenDesksState -> [Widget ClientName]
desksDraw = \case
  s@MkScreenDesksState {_screenStateDesksShowHelp = True} ->
    [ centerLayer $
        borderWithLabel (txt "Help") $
          cropRightTo 80 $
            txt
              "? - Toggle Help\n\
              \r - Refresh Desks\n\
              \c - Create new Desk\n\
              \Enter - Toggle reservations for a desk\n\
              \"
    ]
      <> desksDraw s {_screenStateDesksShowHelp = False}
  s@MkScreenDesksState {_screenStateDesksShowReservations = Just reservationsTxt} ->
    [ centerLayer $
        borderWithLabel (txt "Reservations") $
          cropRightTo 80 $
            txt reservationsTxt
    ]
      <> desksDraw s {_screenStateDesksShowReservations = Nothing}
  s@MkScreenDesksState {_screenStateDesksNewDeskForm = Just form} ->
    [ centerLayer $ borderWithLabel (txt "New Desk") $ cropRightTo 80 $ renderForm form
    ]
      <> desksDraw (s {_screenStateDesksNewDeskForm = Nothing})
  MkScreenDesksState {_screenStateDesksSpace = space, _screenStateDesksList = desksWithInfo} ->
    [ borderWithLabel (txt $ "Desks (" <> unNameSpace (spaceName space) <> ")") $
        padBottom Max $
          padRight Max $
            renderList (\_focus (MkDeskWithInfo {deskWithInfoDesk, deskWithInfoReservations}) -> txt $ T.pack ("#" <> show (unIdentifierDesk $ deskId deskWithInfoDesk) <> " ") <> unNameDesk (deskName deskWithInfoDesk) <> ": " <> T.pack (show deskWithInfoReservations)) True desksWithInfo
    ]

desksHandleEvent :: BrickEvent ClientName ClientEvent -> ApplicationT (EventM ClientName ScreenDesksState) ()
desksHandleEvent event = do
  s <- lift get
  case formState <$> _screenStateDesksNewDeskForm s of
    Nothing ->
      case event of
        VtyEvent (EvKey (KChar '?') []) -> lift $ put s {_screenStateDesksShowHelp = not $ _screenStateDesksShowHelp s}
        VtyEvent (EvKey (KChar 'r') []) -> sendEvent . ClientEventSwitchToScreenDesks . _screenStateDesksSpace =<< lift get
        VtyEvent (EvKey (KChar 'c') []) -> lift $ screenStateDesksNewDeskForm %= const (Just newDeskFormInitial)
        VtyEvent (EvKey KEnter []) -> do
          case _screenStateDesksShowReservations s of
            Just _ -> lift $ put s {_screenStateDesksShowReservations = Nothing}
            Nothing ->
              case listSelectedElement $ _screenStateDesksList s of
                Nothing -> pure ()
                Just (_index, desk) ->
                  lift $
                    put $
                      s
                        { _screenStateDesksShowReservations = Just $ T.pack $ show $ deskWithInfoReservations desk
                        }
        VtyEvent e -> lift $ zoom screenStateDesksList $ handleListEvent e
        _ -> pure ()
    Just newDeskInfo ->
      case event of
        VtyEvent (EvKey KEnter []) ->
          sendEvent $
            ClientEventSendRequestCreateDesk
              (_screenStateDesksSpace s)
              Route.Booking.MkRequestDeskCreate
                { Route.Booking.requestDeskCreateName = MkNameDesk $ newDeskInfo ^. newDeskInfoName
                , Route.Booking.requestDeskCreateSpace = Identifier $ spaceId $ _screenStateDesksSpace s
                }
        _ -> lift $ zoom (screenStateDesksNewDeskForm . _Just) $ handleFormEvent event
