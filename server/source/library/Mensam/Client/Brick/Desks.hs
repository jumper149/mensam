{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Desks where

import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.API.Route.Api.Booking
import Mensam.Client.Brick.Names

import Brick
import Brick.Widgets.Border
import Brick.Widgets.List
import Data.Kind
import Data.Sequence qualified as Seq
import Data.Text qualified as T
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
            renderList (\_focus (MkDeskWithInfo {deskWithInfoDesk, deskWithInfoReservations}) -> txt $ T.pack ("#" <> show (unIdentifierDesk $ deskId deskWithInfoDesk) <> " ") <> deskName deskWithInfoDesk <> ": " <> T.pack (show deskWithInfoReservations)) True desksWithInfo
    ]
