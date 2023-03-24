module Mensam.Client.Brick.Draw where

import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.Client.Brick.Login
import Mensam.Client.Brick.Names
import Mensam.Client.Brick.Register
import Mensam.Client.Brick.Type

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Data.Text qualified as T
import Servant.RawM.Client ()

draw :: ClientState -> [Widget ClientName]
draw MkClientState {_clientStateScreenState, _clientStatePopup} =
  case _clientStatePopup of
    Nothing -> drawScreen _clientStateScreenState
    Just popup -> [center $ borderWithLabel (txt "Error") $ txt popup]

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
