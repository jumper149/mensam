module Mensam.Server where

import Mensam.Server.Application
import Mensam.Server.Application.Configured
import Mensam.Server.Application.Environment
import Mensam.Server.Application.Environment.Acquisition
import Mensam.Server.Application.Logging (logLine)
import Mensam.Server.Application.SeldaPool
import Mensam.Server.Database
import Mensam.Server.Secrets
import Mensam.Server.Server

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Transparent
import Data.Foldable

main :: IO ()
main = runApplicationT server

initialize :: IO ()
initialize = do
  (env, preLog) <- runWriterLoggingT $ do
    logInfo "Start initialization."
    acquireEnvironment

  runTransparentT $
    runEnvironmentT env . deComposeT $
      runStdoutLoggingT . deComposeT $ do
        traverse_ logLine preLog
        runAppConfiguredT . deComposeT $ do
          logDebug "Initialize secrets."
          initSecrets
          logInfo "Initialized secrets."
          runSeldaPoolT . deComposeT $ do
            logDebug "Initialize database."
            initDatabase
            logInfo "Initialized database."
