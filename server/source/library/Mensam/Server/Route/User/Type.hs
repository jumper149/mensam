module Mensam.Server.Route.User.Type where

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import GHC.Generics
import Mensam.User
import Servant.API hiding (BasicAuth)
import Servant.Auth

type Routes :: Type -> Type
data Routes route = Routes
  { routeLogin :: route :- "login" :> Auth '[BasicAuth, JWT] User :> UVerb GET '[JSON] [WithStatus 200 ResponseLogin, WithStatus 400 NoContent, WithStatus 401 NoContent, WithStatus 500 NoContent]
  , routeRegister :: route :- "register" :> ReqBody' '[Lenient, Required] '[JSON] RequestRegister :> UVerb POST '[JSON] [WithStatus 200 NoContent, WithStatus 400 NoContent]
  }
  deriving stock (Generic)

type ResponseLogin :: Type
newtype ResponseLogin = MkResponseLogin
  { responseLoginJWT :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type RequestRegister :: Type
data RequestRegister = MkRequestRegister
  { requestRegisterName :: T.Text
  , requestRegisterPassword :: T.Text
  , requestRegisterEmail :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)
