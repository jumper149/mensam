module Mensam.Server where

import Mensam.Server.Application
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.SQLite
import Mensam.Server.Database.Check
import Mensam.Server.Options
import Mensam.Server.Server

import Control.Monad

main :: IO ()
main = mainWithOptions defaultOptions

mainWithOptions :: Options -> IO ()
mainWithOptions _options = runApplicationT $ do
  config <- configuration
  when (sqliteCheckDataIntegrityOnStartup (configSqlite config)) checkDatabase
  server
