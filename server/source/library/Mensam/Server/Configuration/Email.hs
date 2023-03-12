module Mensam.Server.Configuration.Email where

import Mensam.Server.Aeson

import Data.Aeson qualified as A
import Data.Kind
import Deriving.Aeson qualified as A
import GHC.Generics

type EmailConfig :: Type
data EmailConfig = MkEmailConfig
  { emailSendmailPath :: FilePath
  , emailSendmailArguments :: [String]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "email") EmailConfig
