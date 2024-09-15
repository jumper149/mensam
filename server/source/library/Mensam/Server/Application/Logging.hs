{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.Logging where

import Mensam.Server.Application.Environment.Class

import Control.Monad.Catch
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader qualified as T
import Data.ByteString.Char8 qualified as B
import Data.Foldable
import Data.Kind
import Data.List qualified as L
import Data.Maybe qualified as M
import Data.Text qualified as T
import Data.Time qualified as T
import Data.Time.Format.ISO8601 qualified as T
import System.IO

type CustomLoggingT :: (Type -> Type) -> Type -> Type
newtype CustomLoggingT m a = CustomLoggingT {unCustomLoggingT :: ComposeT (T.ReaderT Bool) LoggingT m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadIO m => MonadLogger (CustomLoggingT m) where
  monadLoggerLog loc logSource logLevel logStr = do
    time <- lift $ liftIO T.getCurrentTime
    let timeInfo = T.iso8601Show time
    logColor <- CustomLoggingT ask
    let
      wrapWithFontEffects :: LogStr -> LogStr -> LogStr
      wrapWithFontEffects fontEffects str =
        if logColor
          then
            fold
              [ "\ESC[0m"
              , "\ESC[" <> fontEffects <> "m"
              , str
              , "\ESC[0m"
              ]
          else str
    let timeLogStr = wrapWithFontEffects "2;94" $ "@{" <> toLogStr timeInfo <> "} "
    CustomLoggingT . monadLoggerLog loc logSource logLevel $ timeLogStr <> toLogStr logStr

deriving via
  CustomLoggingT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadLogger (ComposeT CustomLoggingT t2 m)

runCustomLoggingT ::
  forall m a.
  (MonadIO m, MonadMask m) =>
  Maybe FilePath ->
  LogLevel ->
  -- | color
  Bool ->
  CustomLoggingT m a ->
  m a
runCustomLoggingT maybeFilePath configuredLogLevel configuredLogColor = run . withFilter . (`T.runReaderT` configuredLogColor) . deComposeT . unCustomLoggingT
 where
  run = maybe runStdoutLoggingTCustom runFileLoggingTCustom maybeFilePath
  withFilter = filterLogger $ \_src lvl -> lvl >= configuredLogLevel
  runStdoutLoggingTCustom :: LoggingT m a -> m a
  runStdoutLoggingTCustom = (`runLoggingT` withOutputHandle stdout)
  runFileLoggingTCustom :: FilePath -> LoggingT m a -> m a
  runFileLoggingTCustom file tma = bracket
    (liftIO $ openFile file AppendMode)
    (liftIO . hClose)
    $ \h -> do
      liftIO $ hSetBuffering h LineBuffering
      runLoggingT tma (withOutputHandle h)
  withOutputHandle :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  withOutputHandle h loc src lvl msg = B.hPutStr h $ logStrBS loc src lvl msg
  logStrBS :: Loc -> LogSource -> LogLevel -> LogStr -> B.ByteString
  logStrBS loc src lvl msg = fromLogStr $ logStr loc src lvl msg
  logStr :: Loc -> LogSource -> LogLevel -> LogStr -> LogStr
  logStr loc src lvl msg =
    (<> "\n") $
      fold $
        L.intersperse " " $
          M.catMaybes
            [ Just $ wrapWithFontEffects (levelColor lvl) $ levelLogStr lvl src
            , Just $ wrapWithFontEffects "0" msg
            , wrapWithFontEffects "2;34" <$> locLogStr loc
            ]
  levelLogStr :: LogLevel -> LogSource -> LogStr
  levelLogStr level source =
    fold
      [ "["
      , case level of
          LevelOther otherLevelAsText -> toLogStr otherLevelAsText
          _ -> toLogStr $ B.pack $ drop 5 $ show level
      , if T.null source
          then mempty
          else "#" <> toLogStr source
      , "]"
      ]
  locLogStr :: Loc -> Maybe LogStr
  locLogStr location =
    if location == defaultLoc
      then Nothing
      else
        Just $
          fold
            [ "@("
            , toLogStr $
                B.pack $
                  fold
                    [ loc_package location
                    , ':' : loc_module location
                    , ' ' : loc_filename location
                    , ':' : (show . fst . loc_start) location
                    , ':' : (show . snd . loc_start) location
                    ]
            , ")"
            ]
  wrapWithFontEffects :: LogStr -> LogStr -> LogStr
  wrapWithFontEffects fontEffects str =
    if configuredLogColor
      then
        fold
          [ "\ESC[0m"
          , "\ESC[" <> fontEffects <> "m"
          , str
          , "\ESC[0m"
          ]
      else str
  levelColor :: LogLevel -> LogStr
  levelColor = \case
    LevelDebug -> "36"
    LevelInfo -> "32"
    LevelWarn -> "33"
    LevelError -> "31"
    LevelOther _ -> "35"

runAppCustomLoggingT :: (MonadEnvironment m, MonadIO m, MonadMask m) => CustomLoggingT m a -> m a
runAppCustomLoggingT tma = do
  maybeLogFile <- environmentVariable $ EnvVar @"MENSAM_LOG_FILE"
  logLevel <- environmentVariable $ EnvVar @"MENSAM_LOG_LEVEL"
  logColor <- environmentVariable $ EnvVar @"MENSAM_LOG_COLOR"
  runCustomLoggingT maybeLogFile logLevel logColor tma

logLine ::
  MonadLogger m =>
  LogLine ->
  m ()
logLine (loc, logSource, logLevel, logStr) = monadLoggerLog loc logSource logLevel logStr
