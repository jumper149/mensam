module Mensam.Configuration where

import Mensam.Configuration.BaseUrl
import Mensam.Configuration.Contact

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Word
import Deriving.Aeson qualified as A
import GHC.Generics

type Configuration :: Type
data Configuration = Configuration
  { configRevision :: Maybe T.Text
  , configSqlitePath :: FilePath
  , configDirectoryStatic :: FilePath
  , configPort :: Word16
  , configBaseUrl :: BaseUrl
  , configContactInformation :: ContactInformation
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "config", A.CamelToKebab], A.RejectUnknownFields] Configuration
