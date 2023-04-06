{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Menu where

import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.UI.Brick.Events
import Mensam.Client.UI.Brick.Names

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Monad.Trans.Class
import Data.Kind
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Graphics.Vty.Input.Events
import Lens.Micro.Platform

menuListInitial :: GenericList ClientName Seq.Seq MenuButton
menuListInitial =
  list
    ClientNameMenuList
    (Seq.fromList [minBound .. maxBound])
    1

type MenuButton :: Type
data MenuButton
  = MkMenuButtonLogin
  | MkMenuButtonRegister
  | MkMenuButtonSpaces
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

type ScreenMenuState :: Type
newtype ScreenMenuState = MkScreenMenuState
  { _screenStateMenuList :: GenericList ClientName Seq.Seq MenuButton
  }
makeLenses ''ScreenMenuState

menuDraw :: ScreenMenuState -> [Widget ClientName]
menuDraw = \case
  MkScreenMenuState {_screenStateMenuList = genericList} ->
    [ centerLayer $
        borderWithLabel (txt "Menu") $
          hLimit 20 $
            vLimit 10 $
              renderList buttonDraw True genericList
    , vBox
        [ txt title
        , padTop Max (padLeft Max (txt " Exit (Escape) "))
        ]
    ]
 where
  buttonDraw :: Bool -> MenuButton -> Widget n
  buttonDraw _selected = \case
    MkMenuButtonLogin -> padRight Max $ txt "Login"
    MkMenuButtonRegister -> padRight Max $ txt "Register"
    MkMenuButtonSpaces -> padRight Max $ txt "Spaces"
  title :: T.Text
  title =
    "  __  __                             \n\
    \ |  \\/  | ___  _ _   ___ __ _  _ __  \n\
    \ | |\\/| |/ -_)| ' \\ (_-// _` || '  \\ \n\
    \ |_|  |_|\\___||_||_|/__/\\__/_||_|_|_|\n"

menuHandleEvent :: BrickEvent ClientName ClientEvent -> ApplicationT (EventM ClientName ScreenMenuState) ()
menuHandleEvent event =
  case event of
    VtyEvent (EvKey KEsc []) -> lift halt
    VtyEvent (EvKey KEnter []) -> do
      s <- lift get
      case listSelectedElement $ _screenStateMenuList s of
        Nothing -> pure ()
        Just (_index, button) ->
          case button of
            MkMenuButtonLogin -> sendEvent ClientEventSwitchToScreenLogin
            MkMenuButtonRegister -> sendEvent ClientEventSwitchToScreenRegister
            MkMenuButtonSpaces -> sendEvent ClientEventSwitchToScreenSpaces
    VtyEvent e -> lift $ zoom screenStateMenuList $ handleListEvent e
    _ -> pure ()
