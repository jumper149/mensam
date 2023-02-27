module Mensam where

import Mensam.Application
import Mensam.Server

main :: IO ()
main = runApplicationT server
