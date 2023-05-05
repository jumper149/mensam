module Mensam.Server where

import Mensam.Server.Application
import Mensam.Server.Application.Configured
import Mensam.Server.Application.Environment
import Mensam.Server.Application.Environment.Acquisition
import Mensam.Server.Application.Logging
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
      runAppTimedLoggingT . deComposeT $ do
        traverse_ logLine preLog
        runAppConfiguredT . deComposeT $ do
          runSeldaPoolT . deComposeT $ do
            logDebug "Initializing database."
            createDatabase
            checkDatabase
            logInfo "Initialized database."
            logDebug "Initializing secrets."
            initSecrets
            logInfo "Initialized secrets."
