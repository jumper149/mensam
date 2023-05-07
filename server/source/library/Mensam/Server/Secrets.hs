{-# LANGUAGE OverloadedLabels #-}

module Mensam.Server.Secrets where

import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Database.Schema

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Crypto.JOSE.JWK qualified as JOSE
import Data.Kind
import Data.Text qualified as T
import Data.Time qualified as T
import Database.Selda qualified as Selda
import Servant.Auth.Server

type Secrets :: Type
newtype Secrets = MkSecrets
  { secretsJwk :: JOSE.JWK
  }

jwkGetLatest ::
  ( MonadSeldaPool m
  , MonadLogger m
  ) =>
  SeldaTransactionT m (Maybe JOSE.JWK)
jwkGetLatest = do
  lift $ logDebug "Getting latest JWK."
  dbJwks <- Selda.query $ do
    dbJwk <- Selda.select tableJwk
    Selda.order (dbJwk Selda.! #dbJwk_id) Selda.Desc
    pure dbJwk
  case dbJwks of
    [] -> do
      lift $ logInfo "No JWK currently in database."
      pure Nothing
    dbJwk : _ -> do
      lift $ logDebug "Successfully got JWKs from database. Parsing the latest JWK."
      let jwk = fromSecret $ dbJwk_jwk dbJwk
      lift $ logDebug "Successfully parsed JWK."
      lift $ logInfo "Returning JWK."
      pure $ Just jwk

jwkSetLatest ::
  ( MonadSeldaPool m
  , MonadLogger m
  ) =>
  SeldaTransactionT m JOSE.JWK
jwkSetLatest = do
  lift $ logDebug "Setting latest JWK."
  maybeJwk <- jwkGetLatest
  case maybeJwk of
    Just jwk -> do
      lift $ logWarn "JWK already exists. Skipping."
      lift $ logInfo "Returning an old JWK."
      pure jwk
    Nothing -> do
      lift $ logDebug "Generating a new JWK."
      secret <- liftIO generateSecret
      let jwk = fromSecret secret
      currentTime <- liftIO T.getCurrentTime
      let dbJwk =
            MkDbJwk
              { dbJwk_id = Selda.def
              , dbJwk_jwk = secret
              , dbJwk_created = currentTime
              }
      identifier <- Selda.insertWithPK tableJwk [dbJwk]
      lift $ logInfo $ "Inserted new JWK: " <> T.pack (show identifier)
      lift $ logInfo "Returning newly generated JWK."
      pure jwk
