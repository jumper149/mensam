module Mensam.Server.Route.Booking.Type where

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import GHC.Generics
import Mensam.User
import Servant.API hiding (BasicAuth)
import Servant.Auth

type Routes :: Type -> Type
data Routes route = Routes
  { routeSpaceCreate ::
      route
        :- "space"
          :> "create"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceCreate
          :> UVerb POST '[JSON] [WithStatus 200 (), WithStatus 400 ()]
  , routeDeskCreate ::
      route
        :- "desk"
          :> "create"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskCreate
          :> UVerb POST '[JSON] [WithStatus 200 (), WithStatus 400 ()]
  }
  deriving stock (Generic)

type RequestSpaceCreate :: Type
data RequestSpaceCreate = MkRequestSpaceCreate
  { requestSpaceCreateName :: T.Text
  , requestSpaceCreateVisible :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type RequestDeskCreate :: Type
data RequestDeskCreate = MkRequestDeskCreate
  { requestDeskCreateName :: T.Text
  , requestDeskCreateSpaceName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)
