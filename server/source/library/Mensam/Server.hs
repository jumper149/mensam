module Mensam.Server where

import Mensam.Application.Configured.Class
import Mensam.Application.SeldaPool.Class
import Mensam.Configuration
import Mensam.Configuration.BaseUrl
import Mensam.Server.Handler
import Mensam.Server.Handler.RequestHash
import Mensam.Server.Route
import Mensam.Server.Route.Type
import Mensam.User

import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Data.ByteString.Char8 qualified as B
import Data.Kind
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Trans
import Servant.API
import Servant.Auth.Server
import Servant.Server
import Servant.Server.Generic
import System.Posix.Signals
import System.Posix.Signals.Patterns

type API :: Type
type API = ToServantApi Routes
type WrappedAPI :: Type
type WrappedAPI = RequestHash :> API

type ContextList :: [Type]
type ContextList = '[BasicAuthCfg, CookieSettings, JWTSettings]

hoistServerRunHandlerT :: MonadLogger m => ServerT API (HandlerT m) -> ServerT WrappedAPI m
hoistServerRunHandlerT handler randomHash = hoistServerWithContext (Proxy @API) (Proxy @ContextList) (runHandlerT randomHash) handler

server :: (MonadConfigured m, MonadLogger m, MonadSeldaPool m, MonadUnliftIO m) => m ()
server = do
  logInfo "Configure warp."
  withPort <- setPort . fromEnum . configPort <$> configuration
  withShutdownHandler <- withRunInIO $ \runInIO ->
    pure . setInstallShutdownHandler $ \closeSocket -> do
      let catchOnceShutdown sig = CatchOnce $ do
            runInIO $ do
              logInfo $ "Received signal '" <> T.pack (show @Signal sig) <> "'."
              logWarn "Shutdown."
            closeSocket
      let installShutdownHandler sig = void $ installHandler sig (catchOnceShutdown sig) Nothing
      installShutdownHandler SIGHUP
      installShutdownHandler SIGINT
      installShutdownHandler SIGTERM
  let settings = withShutdownHandler $ withPort defaultSettings

  logInfo "Configure middleware."
  addMiddleware <- runMiddlewareT middleware

  logInfo "Start server."
  withRunInIO $ \runInIO ->
    runSettings settings . addMiddleware $ do
      let
        context :: Context ContextList
        context =
          MkRunLoginInIO runInIO
            :. cookieSettings
            :. jwtSettings
            :. EmptyContext
      serveWithContextT (Proxy @WrappedAPI) context (liftBase . runInIO) $
        hoistServerRunHandlerT $
          genericServerT routes

middleware :: (MonadConfigured m, MonadLogger m) => MiddlewareT m
middleware application req resp = do
  logDebug $ "Received HTTP request: " <> T.pack (show req)
  let
    path = T.decodeLatin1 $ rawPathInfo req
    found302Builder locationPath = do
      baseUrl <- configBaseUrl <$> configuration
      let location = B.pack (T.unpack $ displayBaseUrlPath baseUrl <> locationPath) <> rawQueryString req
      logInfo $ "Redirect HTTP request to new location: " <> T.pack (show location)
      resp $ responseBuilder status302 [(hLocation, location)] mempty
  case T.unsnoc path of
    Nothing -> found302Builder "/"
    Just (xs, '/') -> case T.unsnoc xs of
      Nothing -> application req resp
      Just (_, '/') -> application req resp
      Just (_, _) -> found302Builder xs
    _ -> application req resp
