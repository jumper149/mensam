module Mensam.Server.Route.User.Type where

import Mensam.Aeson
import Mensam.User

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API hiding (BasicAuth)
import Servant.Auth

type Routes :: Type -> Type
data Routes route = Routes
  { routeLogin :: route :- "login" :> Auth '[BasicAuth, JWT] User :> UVerb GET '[JSON] [WithStatus 200 ResponseLogin, WithStatus 400 (), WithStatus 401 (), WithStatus 500 ()]
  , routeRegister :: route :- "register" :> ReqBody' '[Lenient, Required] '[JSON] RequestRegister :> UVerb POST '[JSON] [WithStatus 200 (), WithStatus 400 ()]
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
  { requestRegisterName :: T.Text
  , requestRegisterPassword :: T.Text
  , requestRegisterEmail :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") RequestRegister
