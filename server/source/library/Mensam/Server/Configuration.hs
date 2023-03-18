module Mensam.Server.Configuration where

import Mensam.API.Aeson
import Mensam.Server.Configuration.BaseUrl
import Mensam.Server.Configuration.Email
import Mensam.Server.Configuration.SQLite

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Word
import Deriving.Aeson qualified as A
import GHC.Generics

type Configuration :: Type
data Configuration = Configuration
  { configRevision :: Maybe T.Text
  , configSqlite :: SQLiteConfig
  , configEmailConfig :: Maybe EmailConfig
  , configDirectoryStatic :: FilePath
  , configPort :: Word16
  , configBaseUrl :: BaseUrl
  , configAuth :: AuthConfig
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "" "config") Configuration

type AuthConfig :: Type
data AuthConfig = AuthConfig
  { authJwkFilepath :: FilePath
  , authTimeoutSeconds :: Maybe Integer
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "" "auth") AuthConfig
