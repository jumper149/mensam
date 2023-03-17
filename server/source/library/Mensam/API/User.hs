module Mensam.API.User where

import Mensam.API.Aeson
import Mensam.API.User.Username

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Deriving.Aeson qualified as A
import GHC.Generics
import Text.Email.OrphanInstances ()
import Text.Email.Parser

type User :: Type
data User = MkUser
  { userId :: IdentifierUser
  , userName :: Username
  , userEmail :: EmailAddress
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "user") User

type IdentifierUser :: Type
newtype IdentifierUser = MkIdentifierUser {unIdentifierUser :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type ErrorBasicAuth :: Type
data ErrorBasicAuth
  = MkErrorBasicAuthUsername
  | MkErrorBasicAuthPassword
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkErrorBasicAuth" "") ErrorBasicAuth
