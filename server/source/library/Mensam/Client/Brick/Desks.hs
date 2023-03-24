{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Desks where

import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.Client.Brick.Names

import Brick
import Brick.Widgets.Border
import Brick.Widgets.List
import Data.Kind
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Lens.Micro.Platform

desksListInitial :: GenericList ClientName Seq.Seq Desk
desksListInitial =
  list
    ClientNameSpacesList
    mempty
    1

type ScreenDesksState :: Type
data ScreenDesksState = MkScreenDesksState
  { _screenStateDesksSpace :: Space
  , _screenStateDesksList :: GenericList ClientName Seq.Seq Desk
  }
makeLenses ''ScreenDesksState

desksDraw :: ScreenDesksState -> [Widget ClientName]
desksDraw = \case
  MkScreenDesksState {_screenStateDesksSpace = space, _screenStateDesksList = desks} ->
    [ borderWithLabel (txt $ "Desks (" <> spaceName space <> ")") $
        padBottom Max $
          padRight Max $
            renderList (\_focus desk -> txt $ T.pack ("#" <> show (unIdentifierDesk $ deskId desk) <> " ") <> deskName desk) True desks
    ]
