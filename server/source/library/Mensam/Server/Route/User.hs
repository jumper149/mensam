module Mensam.Server.Route.User where

import Mensam.Application.SeldaConnection.Class
import Mensam.Server.Route.User.Type
import Mensam.User

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Database.Selda qualified as Selda
import Servant.Auth.Server
import Servant.Server.Generic

handler ::
  (MonadIO m, MonadLogger m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeLogin = login
    }

login :: (MonadIO m, MonadLogger m) => AuthResult User -> m ResponseLogin
login authUser =
  case authUser of
    Authenticated user -> do
      logDebug $ "Creating JWT for user: " <> T.pack (show user)
      eitherJwt <- liftIO $ makeJWT user undefined undefined
      case eitherJwt of
        Left err -> do
          logInfo $ "Failed to create JWT: " <> T.pack (show err)
          undefined
        Right jwt -> do
          logInfo "Created JWT successfully."
          pure MkResponseLogin {responseLoginJWT = TL.decodeUtf8 jwt}
    failedAuthentication ->
      case failedAuthentication of
        BadPassword -> undefined
        NoSuchUser -> undefined
        Indefinite -> undefined

userCreate :: (MonadIO m, MonadSeldaConnection m) => RequestUserCreate -> m ()
userCreate request = do
  passwordHash :: PasswordHash Bcrypt <- hashPassword $ requestUserCreatePassword request
  let user =
        MkUser
          { userName = requestUserCreateName request
          , userPasswordHash = passwordHash
          , userEmail = requestUserCreateEmail request
          }
  let dbUser =
        MkDbUser
          { dbUser_id = Selda.def
          , dbUser_name = requestUserCreateName request
          , dbUser_password_hash = unPasswordHash $ userPasswordHash user
          , dbUser_email = requestUserCreateEmail request
          }
  runSeldaTransaction $
    Selda.insert_ usersTable [dbUser]
  pure ()
