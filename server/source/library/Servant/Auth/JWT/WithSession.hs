{-# LANGUAGE UndecidableInstances #-}

module Servant.Auth.JWT.WithSession where

import Control.Monad
import Data.Kind
import Servant.Auth.Server
import Servant.Auth.Server.Internal.Class

type JWTWithSession :: Type
data JWTWithSession

instance (WithSession usr, IsAuth JWT usr) => IsAuth JWTWithSession usr where
  type AuthArgs JWTWithSession = SessionCfg ': AuthArgs JWT
  runAuth _ proxyUsr sessionCfg jwtSettings =
    AuthCheck $
      runAuthCheck (runAuth (undefined :: proxy JWT) proxyUsr jwtSettings)
        >=> \case
          BadPassword -> pure BadPassword
          NoSuchUser -> pure NoSuchUser
          Authenticated u -> validateSession sessionCfg u
          Indefinite -> pure Indefinite

type WithSession :: Type -> Constraint
class WithSession a where
  validateSession :: SessionCfg -> a -> IO (AuthResult a)

type SessionCfg :: Type
type family SessionCfg
