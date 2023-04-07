module Mensam.Client.UI.Brick.Draw where

import Mensam.Client.UI.Brick.Names
import Mensam.Client.UI.Brick.State
import Mensam.Client.UI.Desks
import Mensam.Client.UI.Login
import Mensam.Client.UI.Register
import Mensam.Client.UI.Spaces

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Text qualified as T

draw :: ClientState -> [Widget ClientName]
draw MkClientState {_clientStateScreenState, _clientStatePopup} =
  case _clientStatePopup of
    Nothing -> drawScreen _clientStateScreenState
    Just popup -> [center $ borderWithLabel (txt "Error") $ txt popup]

drawScreen :: ClientScreenState -> [Widget ClientName]
drawScreen = \case
  ClientScreenStateLogin s -> loginDraw s <> [drawHelp]
  ClientScreenStateRegister s -> registerDraw s <> [drawHelp]
  ClientScreenStateSpaces s -> spacesDraw s
  ClientScreenStateDesks s -> desksDraw s

drawHelp :: Widget a
drawHelp =
  vBox
    [ txt title
    , padTop Max (padLeft Max (txt " Exit (Escape) | Help (?) | Register (Alt-1) | Login (Alt-2) | Spaces (Alt-3) "))
    ]
 where
  title :: T.Text
  title =
    "  __  __                             \n\
    \ |  \\/  | ___  _ _   ___ __ _  _ __  \n\
    \ | |\\/| |/ -_)| ' \\ (_-// _` || '  \\ \n\
    \ |_|  |_|\\___||_||_|/__/\\__/_||_|_|_|\n"
