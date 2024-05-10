{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Server.Server.Auth where

import Mensam.API.Aeson.StaticText
import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.User

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Crypto.JOSE.JWK qualified as JOSE
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Servant hiding (BasicAuthResult (..))
import Servant.Auth.JWT.WithSession
import Servant.Auth.Server

handleAuthBasic ::
  ( MonadLogger m
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  ) =>
  AuthResult a ->
  (a -> m (Union responses)) ->
  m (Union responses)
handleAuthBasic authResult handler = do
  logDebug "Handling result of Basic authentication."
  case authResult of
    Authenticated authenticated -> do
      logInfo "Starting handler after successful authentication."
      handler authenticated
    BadPassword -> do
      logInfo "Can't access handler, because authentication failed due to wrong password."
      respond $ WithStatus @401 MkErrorBasicAuthPassword
    NoSuchUser -> do
      logInfo "Can't access handler, because authentication failed due to not existing username."
      respond $ WithStatus @401 MkErrorBasicAuthUsername
    Indefinite -> do
      logInfo "Can't access handler, because authentication failed for some reason."
      respond $ WithStatus @401 MkErrorBasicAuthIndefinite

handleAuthBearer ::
  ( MonadLogger m
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  ) =>
  AuthResult a ->
  (a -> m (Union responses)) ->
  m (Union responses)
handleAuthBearer authResult handler = do
  logDebug "Handling result of Bearer authentication."
  case authResult of
    Authenticated authenticated -> do
      logInfo "Starting handler after successful authentication."
      handler authenticated
    BadPassword -> do
      logError "Didn't expect to handle NoSuchUser in Bearer authentication."
      logWarn "Returning a HTTP 401 response even though this case was unexpected."
      respond $ WithStatus @401 $ MkErrorBearerAuth $ MkStaticText @"indefinite"
    NoSuchUser -> do
      logError "Didn't expect to handle NoSuchUser in Bearer authentication."
      logWarn "Returning a HTTP 401 response even though this case was unexpected."
      respond $ WithStatus @401 $ MkErrorBearerAuth $ MkStaticText @"indefinite"
    Indefinite -> do
      logInfo "Can't access handler, because authentication failed for some reason."
      respond $ WithStatus @401 $ MkErrorBearerAuth $ MkStaticText @"indefinite"

deriving anyclass instance FromJWT UserAuthenticated
deriving anyclass instance ToJWT UserAuthenticated

instance WithSession UserAuthenticated where
  validateSession MkRunLoginInIO {runLoginInIO} authenticated@MkUserAuthenticated {userAuthenticatedSession} =
    runLoginInIO $
      case userAuthenticatedSession of
        Nothing -> do
          logDebug "Authenticating user without session."
          pure $ Authenticated authenticated
        Just sessionIdentifier -> do
          logInfo "Validating session before authenticating user."
          seldaValidationResult <- runSeldaTransactionT $ userSessionValidate sessionIdentifier
          case seldaValidationResult of
            SeldaFailure err -> do
              -- This case should not occur under normal circumstances.
              -- The transaction in this case is just a read transaction.
              logError "Session validation failed because of a database error."
              throwM err
            SeldaSuccess validity ->
              case validity of
                SessionInvalid -> pure Indefinite
                SessionValid -> pure $ Authenticated authenticated

instance FromBasicAuthData UserAuthenticated where
  fromBasicAuthData BasicAuthData {basicAuthUsername, basicAuthPassword} MkRunLoginInIO {runLoginInIO} =
    runLoginInIO $ do
      logInfo "Starting password authentication."
      logDebug $ "Decoding UTF-8 username: " <> T.pack (show basicAuthUsername)
      case T.decodeUtf8' basicAuthUsername of
        Left err -> do
          logInfo $ "Failed to decode username as UTF-8: " <> T.pack (show err)
          pure NoSuchUser
        Right usernameText -> do
          logDebug $ "Decoded UTF-8 username: " <> T.pack (show usernameText)
          let username = MkUsernameUnsafe usernameText
          logDebug "Decoding UTF-8 password."
          case mkPassword <$> T.decodeUtf8' basicAuthPassword of
            Left _err -> do
              logInfo "Failed to decode password as UTF-8."
              pure BadPassword
            Right password -> do
              logDebug "Decoded UTF-8 password."
              seldaAuthResult <- runSeldaTransactionT $ userAuthenticate username password
              case seldaAuthResult of
                SeldaFailure err -> do
                  -- This case should not occur under normal circumstances.
                  -- The transaction in this case is just a read transaction.
                  logError "Authentication failed because of a database error."
                  throwM err
                SeldaSuccess authResult ->
                  case authResult of
                    Left AuthenticationErrorUserDoesNotExist -> pure NoSuchUser
                    Left AuthenticationErrorWrongPassword -> pure BadPassword
                    Right authenticated -> pure $ Authenticated authenticated

type instance BasicAuthCfg = RunLoginInIO

type instance SessionCfg = RunLoginInIO

type RunLoginInIO :: Type
data RunLoginInIO = forall m.
  (MonadIO m, MonadLogger m, MonadSeldaPool m, MonadThrow m) =>
  MkRunLoginInIO {runLoginInIO :: m (AuthResult UserAuthenticated) -> IO (AuthResult UserAuthenticated)}

mkJwtSettings :: JOSE.JWK -> JWTSettings
mkJwtSettings = defaultJWTSettings

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings
