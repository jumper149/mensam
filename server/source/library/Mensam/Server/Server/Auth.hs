{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Server.Server.Auth where

import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.User

import Control.Monad.Catch
import Control.Monad.Logger.CallStack
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Servant hiding (BasicAuthResult (..))
import Servant.Auth.Server

handleAuth ::
  ( MonadLogger m
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  ) =>
  AuthResult a ->
  (a -> m (Union responses)) ->
  m (Union responses)
handleAuth authResult handler =
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

deriving anyclass instance FromJWT UserAuthenticated
deriving anyclass instance ToJWT UserAuthenticated

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
          logDebug $ "Parsing username: " <> T.pack (show usernameText)
          case mkUsername usernameText of
            Left err -> do
              logInfo $ "Failed to parse username: " <> T.pack (show err)
              pure NoSuchUser
            Right username -> do
              logDebug $ "Parsed username: " <> T.pack (show username)
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

type RunLoginInIO :: Type
data RunLoginInIO = forall m.
  (MonadLogger m, MonadSeldaPool m, MonadThrow m) =>
  MkRunLoginInIO {runLoginInIO :: m (AuthResult UserAuthenticated) -> IO (AuthResult UserAuthenticated)}

-- TODO: Remove hardcoded JWK from source code.
jwtSettings :: JWTSettings
jwtSettings = defaultJWTSettings $ fromSecret "\155\EM\235\181/\128\236M\130\245\173\142\231<\241A\159Ds\238S|m\219\234\188\224\162\n(\212\148\212\147\180E>\138\141\"\161\US\154\173\212Y\FS\213\206\ENQ\196^\172\220\142\167r(\172\r\217\143~\234\179\244\204n\\2\149\n\195\NAKEh\176\130\bUt\141A_BE\146\211\247P\251\&5\231\DC2b\131=\b\GS\211\FS_\138z?A 6\178v\213\191\218\&7\ETB\161N\185\157\229\DC1m]\144\203\155\234\136\FS\148Q\228^\247\140\150g?_\ETX\164f\163\140\170\233{\153\189\248\r)\170'V'\RS\206q\f \222;8>\\\244\166\f\168\209B-\244\241\ACKZs\134f\225\&4\DLE\151\230\215\145]\160Z\t\ACKk\US'\174p\221\&9\ETB\214mV\140\STX\253P\179<BV^Ssp\163\&3\173>P\245T\160\\cr\194\ETX\178.D\253\233'\170y\148\250\232z\229\SYNK\212)>T\NUL2I\165\175\164\170\143\RS"

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings
