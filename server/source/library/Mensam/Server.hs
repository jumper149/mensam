module Mensam.Server where

import Mensam.Server.Application
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.SQLite
import Mensam.Server.Database.Check
import Mensam.Server.Server

import Control.Monad

main :: IO ()
main = runApplicationT $ do
  config <- configuration
  when (sqliteCheckDataIntegrityOnStartup (configSqlite config)) checkDatabase
  server
