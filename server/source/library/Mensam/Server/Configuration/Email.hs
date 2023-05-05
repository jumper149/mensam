module Mensam.Server.Configuration.Email where

import Mensam.API.Aeson

import Data.Aeson qualified as A
import Data.Kind
import Data.Word
import Deriving.Aeson qualified as A
import GHC.Generics
import Network.Mail.SMTP
import Network.Socket

type EmailConfig :: Type
data EmailConfig = MkEmailConfig
  { emailHostname :: HostName
  , emailPort :: Word16
  , emailUsername :: UserName
  , emailPassword :: Password
  , emailTls :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "email") EmailConfig
