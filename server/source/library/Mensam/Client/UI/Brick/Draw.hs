module Mensam.Client.UI.Brick.Draw where

import Brick
import Data.Text qualified as T

drawHelp :: Widget a
drawHelp =
  vBox
    [ txt title
    , padTop Max $ padLeft Max $ txt footer
    ]

title :: T.Text
title =
  "  __  __                             \n\
  \ |  \\/  | ___  _ _   ___ __ _  _ __  \n\
  \ | |\\/| |/ -_)| ' \\ (_-// _` || '  \\ \n\
  \ |_|  |_|\\___||_||_|/__/\\__/_||_|_|_|\n"

footer :: T.Text
footer = " Menu (Escape) | Help (?) "
