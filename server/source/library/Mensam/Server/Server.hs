module Mensam.Server.Server where

import Mensam.API.API
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.BaseUrl
import Mensam.Server.Server.Auth
import Mensam.Server.Server.Handler
import Mensam.Server.Server.Handler.RequestHash
import Mensam.Server.Server.Route

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Error.Class
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

type WrappedAPI :: Type
type WrappedAPI = RequestHash :> API

type ContextList :: [Type]
type ContextList = '[BasicAuthCfg, CookieSettings, JWTSettings]

hoistServerRunHandlerT :: MonadLogger m => ServerT API (HandlerT m) -> ServerT WrappedAPI m
hoistServerRunHandlerT handler randomHash = hoistServerWithContext (Proxy @API) (Proxy @ContextList) (runHandlerT randomHash) handler

server :: forall m. (MonadConfigured m, MonadLogger m, MonadMask m, MonadSeldaPool m, MonadUnliftIO m) => m ()
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
        runInHandler :: forall a. m a -> Servant.Server.Handler a
        runInHandler ma =
          (liftEither =<<) $
            liftIO $
              runInIO $
                catch (Right <$> ma) $
                  \(err :: SomeException) -> do
                    logError $ "Handler encountered an exception: " <> T.pack (show err)
                    pure $
                      Left
                        ServerError
                          { errHTTPCode = 500
                          , errReasonPhrase = "Internal Server Error"
                          , errBody = "Encountered an internal error in the HTTP handler."
                          , errHeaders = mempty
                          }
      serveWithContextT (Proxy @WrappedAPI) context runInHandler $
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
