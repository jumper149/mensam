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
  { routeLogin :: route :- "login" :> Auth '[BasicAuth, JWT] User :> Get '[JSON] ResponseLogin
  , routeRegister :: route :- "register" :> ReqBody' '[Lenient, Required] '[JSON] RequestRegister :> Post '[JSON] NoContent
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
