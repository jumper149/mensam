module Mensam.API.Data.User where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Text qualified as T
import Data.Time qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API qualified as Servant
import Text.Email.OrphanInstances ()

type UserAuthenticated :: Type
data UserAuthenticated = MkUserAuthenticated
  { userAuthenticatedId :: IdentifierUser
  , userAuthenticatedSession :: Maybe IdentifierSession
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "userAuthenticated") UserAuthenticated

type IdentifierUser :: Type
newtype IdentifierUser = MkIdentifierUser {unIdentifierUser :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)

type Session :: Type
data Session = MkSession
  { sessionId :: IdentifierSession
  , sessionTimeCreated :: T.UTCTime
  , sessionTimeExpired :: Maybe T.UTCTime
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "session") Session

type IdentifierSession :: Type
newtype IdentifierSession = MkIdentifierSession {unIdentifierSession :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type ConfirmationSecret :: Type
newtype ConfirmationSecret = MkConfirmationSecret {unConfirmationSecret :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type ErrorBasicAuth :: Type
data ErrorBasicAuth
  = MkErrorBasicAuthUsername
  | MkErrorBasicAuthPassword
  | MkErrorBasicAuthIndefinite
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkErrorBasicAuth" "") ErrorBasicAuth

type ErrorBearerAuth :: Type
newtype ErrorBearerAuth = MkErrorBearerAuth {unErrorBearerAuth :: StaticText "indefinite"}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)
