module Mensam.Server where

import Mensam.Server.Application
import Mensam.Server.Server

main :: IO ()
main = runApplicationT server
