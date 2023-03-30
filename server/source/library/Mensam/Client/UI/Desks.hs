{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Desks where

import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.API.Route.Api.Booking
import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.UI.Brick.Events
import Mensam.Client.UI.Brick.Names

import Brick
import Brick.Widgets.Border
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

type ScreenDesksState :: Type
data ScreenDesksState = MkScreenDesksState
  { _screenStateDesksSpace :: Space
  , _screenStateDesksList :: GenericList ClientName Seq.Seq DeskWithInfo
  }
makeLenses ''ScreenDesksState

desksDraw :: ScreenDesksState -> [Widget ClientName]
desksDraw = \case
  MkScreenDesksState {_screenStateDesksSpace = space, _screenStateDesksList = desksWithInfo} ->
    [ borderWithLabel (txt $ "Desks (" <> unNameSpace (spaceName space) <> ")") $
        padBottom Max $
          padRight Max $
            renderList (\_focus (MkDeskWithInfo {deskWithInfoDesk, deskWithInfoReservations}) -> txt $ T.pack ("#" <> show (unIdentifierDesk $ deskId deskWithInfoDesk) <> " ") <> unNameDesk (deskName deskWithInfoDesk) <> ": " <> T.pack (show deskWithInfoReservations)) True desksWithInfo
    ]

desksHandleEvent :: BrickEvent ClientName ClientEvent -> ApplicationT (EventM ClientName ScreenDesksState) ()
desksHandleEvent = \case
  VtyEvent (EvKey (KChar 'r') []) -> sendEvent . ClientEventSwitchToScreenDesks . _screenStateDesksSpace =<< lift get
  VtyEvent (EvKey KEnter []) -> pure () -- TODO: What do we want to do with a selected desk?
  VtyEvent e -> lift $ zoom screenStateDesksList $ handleListEvent e
  _ -> pure ()
