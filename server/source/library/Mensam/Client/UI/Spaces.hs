{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Spaces where

import Mensam.API.Data.Space
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.UI.Brick.Draw
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
import Data.Time.Zones.All qualified as T
import Graphics.Vty.Input.Events
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
  , _newSpaceInfoTimezone :: T.TZLabel
  , _newSpaceInfoAccessibility :: AccessibilitySpace
  , _newSpaceInfoVisibility :: VisibilitySpace
  }
makeLenses ''NewSpaceInfo

newSpaceFormInitial :: Form NewSpaceInfo e ClientName
newSpaceFormInitial =
  newForm
    [ (str "Name: " <+>) @@= editTextField newSpaceInfoName ClientNameSpacesNewSpaceName (Just 1)
    , (str "Timezone: " <+>) @@= radioField newSpaceInfoTimezone (map (\x -> (x, ClientNameSpacesNewSpaceTimezone x, T.pack $ show x)) [T.UTC, T.Europe__Berlin])
    , (str "Accessibility: " <+>) @@= radioField newSpaceInfoAccessibility (map (\x -> (x, ClientNameSpacesNewSpaceAccessibility x, T.pack $ show x)) [minBound @AccessibilitySpace .. maxBound])
    , (str "Visibility: " <+>) @@= radioField newSpaceInfoVisibility (map (\x -> (x, ClientNameSpacesNewSpaceVisibility x, T.pack $ show x)) [minBound @VisibilitySpace .. maxBound])
    ]
    MkLoginInfo
      { _newSpaceInfoName = ""
      , _newSpaceInfoTimezone = T.UTC
      , _newSpaceInfoAccessibility = MkAccessibilitySpaceJoinable
      , _newSpaceInfoVisibility = MkVisibilitySpaceVisible
      }

type ScreenSpacesState :: Type
data ScreenSpacesState = MkScreenSpacesState
  { _screenStateSpacesList :: GenericList ClientName Seq.Seq Space
  , _screenStateSpacesShowHelp :: Bool
  , _screenStateSpacesNewSpaceForm :: Maybe (Form NewSpaceInfo ClientEvent ClientName)
  }
makeLenses ''ScreenSpacesState

spacesDraw :: ScreenSpacesState -> [Widget ClientName]
spacesDraw = \case
  s@MkScreenSpacesState {_screenStateSpacesShowHelp = True} ->
    [ centerLayer $
        borderWithLabel (txt "Help") $
          cropRightTo 80 $
            txt
              "? - Toggle Help\n\
              \r - Refresh Spaces\n\
              \c - Create new Space\n\
              \Enter - View Desk\n\
              \"
    ]
      <> spacesDraw s {_screenStateSpacesShowHelp = False}
  s@MkScreenSpacesState {_screenStateSpacesNewSpaceForm = Just form} ->
    [ centerLayer $ borderWithLabel (txt "New Space") $ cropRightTo 80 $ renderForm form
    ]
      <> spacesDraw (s {_screenStateSpacesNewSpaceForm = Nothing})
  MkScreenSpacesState {_screenStateSpacesList = spaces} ->
    [ vBox
        [ borderWithLabel (txt "Spaces") $
            padBottom Max $
              padRight Max $
                renderList (\_focus space -> padRight Max $ txt $ T.pack ("#" <> show (unIdentifierSpace $ spaceId space) <> " ") <> unNameSpace (spaceName space)) True spaces
        , padLeft Max $ txt footerMenuHelp
        ]
    ]

spacesHandleEvent :: BrickEvent ClientName ClientEvent -> ApplicationT (EventM ClientName ScreenSpacesState) ()
spacesHandleEvent event = do
  s <- lift get
  case formState <$> _screenStateSpacesNewSpaceForm s of
    Nothing ->
      case event of
        VtyEvent (EvKey (KChar '?') []) -> lift $ put s {_screenStateSpacesShowHelp = not $ _screenStateSpacesShowHelp s}
        VtyEvent (EvKey (KChar 'r') []) -> sendEvent ClientEventSwitchToScreenSpaces
        VtyEvent (EvKey (KChar 'c') []) -> lift $ screenStateSpacesNewSpaceForm %= const (Just newSpaceFormInitial)
        VtyEvent (EvKey KEnter []) -> do
          case listSelectedElement $ _screenStateSpacesList s of
            Nothing -> pure ()
            Just (_index, space) -> sendEvent $ ClientEventSwitchToScreenDesks space
        VtyEvent e -> lift $ zoom screenStateSpacesList $ handleListEvent e
        _ -> pure ()
    Just newSpaceInfo ->
      case event of
        VtyEvent (EvKey KEnter []) ->
          sendEvent $
            ClientEventSendRequestCreateSpace
              Route.Booking.MkRequestSpaceCreate
                { Route.Booking.requestSpaceCreateName = MkNameSpace $ newSpaceInfo ^. newSpaceInfoName
                , Route.Booking.requestSpaceCreateTimezone = newSpaceInfo ^. newSpaceInfoTimezone
                , Route.Booking.requestSpaceCreateVisibility = newSpaceInfo ^. newSpaceInfoVisibility
                , Route.Booking.requestSpaceCreateAccessibility = newSpaceInfo ^. newSpaceInfoAccessibility
                }
        _ -> lift $ zoom (screenStateSpacesNewSpaceForm . _Just) $ handleFormEvent event
