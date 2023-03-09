module Mensam.Configuration where

import Mensam.Aeson
import Mensam.Configuration.BaseUrl
import Mensam.Configuration.Contact
import Mensam.Configuration.Email
import Mensam.Configuration.SQLite

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
  , configContactInformation :: ContactInformation
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "" "config") Configuration
