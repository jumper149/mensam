{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Spaces where

import Mensam.API.Data.Space
import Mensam.Client.Brick.Names

import Brick
import Brick.Widgets.Border
import Brick.Widgets.List
import Data.Kind
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Lens.Micro.Platform

spacesListInitial :: GenericList ClientName Seq.Seq Space
spacesListInitial =
  list
    ClientNameSpacesList
    mempty
    1

type ScreenSpacesState :: Type
newtype ScreenSpacesState = MkScreenSpacesState
  { _screenStateSpacesList :: GenericList ClientName Seq.Seq Space
  }
makeLenses ''ScreenSpacesState

spacesDraw :: ScreenSpacesState -> [Widget ClientName]
spacesDraw = \case
  MkScreenSpacesState {_screenStateSpacesList = spaces} ->
    [ borderWithLabel (txt "Spaces") $
        padBottom Max $
          padRight Max $
            renderList (\_focus space -> txt $ T.pack ("#" <> show (unIdentifierSpace $ spaceId space) <> " ") <> unNameSpace (spaceName space)) True spaces
    ]
