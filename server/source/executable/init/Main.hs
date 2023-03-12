module Main (main) where

import Mensam.Server.Application
import Mensam.Server.Database

main :: IO ()
main = runApplicationT initDatabase
