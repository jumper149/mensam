{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Time.Zones.All.OrphanInstances () where

import Control.Lens
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Zones.All
import Database.Selda qualified as Selda

instance A.FromJSON TZLabel where
  parseJSON json = do
    text <- A.prependFailure "parsing time zone database identifier failed, " $ A.parseJSON @T.Text json
    case fromTZName $ T.encodeUtf8 text of
      Nothing -> fail "parsing time zone database identifier failed"
      Just timezoneLabel -> pure timezoneLabel

instance A.ToJSON TZLabel where
  toJSON = A.String . T.decodeUtf8 . toTZName

instance ToSchema TZLabel where
  declareNamedSchema Proxy =
    pure $
      NamedSchema (Just "TZLabel") $
        mempty
          & type_ ?~ OpenApiString
          & description ?~ "IANA time zone database identifier"

instance Selda.SqlEnum TZLabel where
  toText = T.decodeUtf8 . toTZName
  fromText text =
    case fromTZName $ T.encodeUtf8 text of
      Nothing -> error $ "Failed to read time zone database identifier from an SQL value: " ++ show text
      Just timezoneLabel -> timezoneLabel

deriving anyclass instance Selda.SqlType TZLabel
