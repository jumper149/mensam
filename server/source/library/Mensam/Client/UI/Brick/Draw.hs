module Mensam.Client.UI.Brick.Draw where

import Brick
import Data.Text qualified as T

drawHelp :: Widget a
drawHelp =
  vBox
    [ txt title
    , padTop Max $ padLeft Max $ txt footerMenu
    ]

title :: T.Text
title =
  "  __  __                             \n\
  \ |  \\/  | ___  _ _   ___ __ _  _ __  \n\
  \ | |\\/| |/ -_)| ' \\ (_-// _` || '  \\ \n\
  \ |_|  |_|\\___||_||_|/__/\\__/_||_|_|_|\n"

footerMenuHelp :: T.Text
footerMenuHelp = " Menu (Escape) | Help (?) "

footerMenu :: T.Text
footerMenu = " Menu (Escape) "
