{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.Brick.Spaces where

import Mensam.API.Data.Space
import Mensam.Client.Brick.Names

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
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

type NewSpaceInfo :: Type
data NewSpaceInfo = MkLoginInfo
  { _newSpaceInfoName :: T.Text
  , _newSpaceInfoAccessibility :: AccessibilitySpace
  , _newSpaceInfoVisibility :: VisibilitySpace
  }
makeLenses ''NewSpaceInfo

newSpaceFormInitial :: Form NewSpaceInfo e ClientName
newSpaceFormInitial =
  newForm
    [ (str "Name: " <+>) @@= editTextField newSpaceInfoName ClientNameSpacesNewSpaceName (Just 1)
    , (str "Accessibility: " <+>) @@= radioField newSpaceInfoAccessibility (map (\x -> (x, ClientNameSpacesNewSpaceAccessibility x, T.pack $ show x)) [minBound @AccessibilitySpace .. maxBound])
    , (str "Visibility: " <+>) @@= radioField newSpaceInfoVisibility (map (\x -> (x, ClientNameSpacesNewSpaceVisibility x, T.pack $ show x)) [minBound @VisibilitySpace .. maxBound])
    ]
    MkLoginInfo
      { _newSpaceInfoName = ""
      , _newSpaceInfoAccessibility = MkAccessibilitySpaceJoinable
      , _newSpaceInfoVisibility = MkVisibilitySpaceVisible
      }

type ScreenSpacesState :: Type
data ScreenSpacesState = MkScreenSpacesState
  { _screenStateSpacesList :: GenericList ClientName Seq.Seq Space
  , _screenStateSpacesNewSpaceForm :: Maybe (Form NewSpaceInfo () ClientName)
  }
makeLenses ''ScreenSpacesState

spacesDraw :: ScreenSpacesState -> [Widget ClientName]
spacesDraw = \case
  s@MkScreenSpacesState {_screenStateSpacesNewSpaceForm = Just form} ->
    [ centerLayer $ borderWithLabel (txt "New Space") $ cropRightTo 80 $ renderForm form
    ]
      <> spacesDraw (s {_screenStateSpacesNewSpaceForm = Nothing})
  MkScreenSpacesState {_screenStateSpacesList = spaces} ->
    [ borderWithLabel (txt "Spaces") $
        padBottom Max $
          padRight Max $
            renderList (\_focus space -> txt $ T.pack ("#" <> show (unIdentifierSpace $ spaceId space) <> " ") <> unNameSpace (spaceName space)) True spaces
    ]
