module Mensam.Server.Route.User.Type where

import Data.Aeson qualified as A
import Data.Kind
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Generics
import Mensam.User
import Servant.API hiding (BasicAuth)
import Servant.Auth

type Routes :: Type -> Type
newtype Routes route = Routes
  { routeLogin :: route :- "login" :> Auth '[BasicAuth] User :> Get '[JSON] ResponseLogin
  }
  deriving stock (Generic)

type ResponseLogin :: Type
newtype ResponseLogin = MkResponseLogin
  { responseLoginJWT :: TL.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.ToJSON)

type RequestUserCreate :: Type
data RequestUserCreate = MkRequestUserCreate
  { requestUserCreateName :: T.Text
  , requestUserCreatePassword :: Password
  , requestUserCreateEmail :: T.Text
  }
