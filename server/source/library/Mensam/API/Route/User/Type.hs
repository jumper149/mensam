module Mensam.API.Route.User.Type where

import Mensam.API.Aeson
import Mensam.API.User
import Mensam.API.User.Username

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API hiding (BasicAuth)
import Servant.Auth
import Text.Email.Parser

type Routes :: Type -> Type
data Routes route = Routes
  { routeLogin ::
      route
        :- Summary "Login"
          :> Description
              "Login to your user account.\n"
          :> "login"
          :> Auth '[BasicAuth, JWT] User
          :> UVerb POST '[JSON] [WithStatus 200 ResponseLogin, WithStatus 401 ErrorBasicAuth, WithStatus 500 ()]
  , routeRegister ::
      route
        :- Summary "Register"
          :> Description
              "Register a new user account.\n"
          :> "register"
          :> ReqBody' '[Lenient, Required] '[JSON] RequestRegister
          :> UVerb POST '[JSON] [WithStatus 201 (), WithStatus 400 ErrorParseBodyJson, WithStatus 500 ()]
  , routeProfile ::
      route
        :- Summary "Request User Profile"
          :> Description
              "Request information for a user profile.\n"
          :> "profile"
          :> QueryParam' '[Lenient, Required] "name" Username
          :> UVerb GET '[JSON] [WithStatus 200 ResponseProfile, WithStatus 400 (), WithStatus 404 (), WithStatus 500 ()]
  }
  deriving stock (Generic)

type ResponseLogin :: Type
newtype ResponseLogin = MkResponseLogin
  { responseLoginJWT :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseLogin") ResponseLogin

type RequestRegister :: Type
data RequestRegister = MkRequestRegister
  { requestRegisterName :: Username
  , requestRegisterPassword :: T.Text
  , requestRegisterEmail :: EmailAddress
  , requestRegisterEmailVisible :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") RequestRegister

type ResponseProfile :: Type
data ResponseProfile = MkResponseProfile
  { responseProfileId :: T.Text -- TODO: Use better type.
  , responseProfileName :: Username
  , responseProfileEmail :: EmailAddress
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") ResponseProfile
