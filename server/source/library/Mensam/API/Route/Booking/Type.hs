module Mensam.API.Route.Booking.Type where

import Mensam.API.Aeson
import Mensam.API.User
import Mensam.Server.Booking
import Mensam.Server.Database.Extra qualified as Selda
import Mensam.Server.Database.Schema

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API hiding (BasicAuth)
import Servant.Auth

type Routes :: Type -> Type
data Routes route = Routes
  { routeSpaceCreate ::
      route
        :- Summary "Create Space"
          :> Description
              "Create a new space.\n\
              \You will be an administrator of this newly created space."
          :> "space"
          :> "create"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceCreate
          :> UVerb POST '[JSON] [WithStatus 200 (), WithStatus 400 (), WithStatus 401 (), WithStatus 500 ()]
  , routeSpaceList ::
      route
        :- Summary "List Spaces"
          :> Description
              "List visible spaces."
          :> "space"
          :> "list"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceList
          :> UVerb POST '[JSON] [WithStatus 200 ResponseSpaceList, WithStatus 400 (), WithStatus 401 (), WithStatus 500 ()]
  , routeDeskCreate ::
      route
        :- Summary "List Desks"
          :> Description
              "Create a new desk.\n\
              \This desk will belong to the given space."
          :> "desk"
          :> "create"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskCreate
          :> UVerb POST '[JSON] [WithStatus 200 (), WithStatus 400 (), WithStatus 401 (), WithStatus 500 ()]
  , routeDeskList ::
      route
        :- Summary "List Desks"
          :> Description
              "List desks."
          :> "desk"
          :> "list"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskList
          :> UVerb POST '[JSON] [WithStatus 200 ResponseDeskList, WithStatus 400 (), WithStatus 401 (), WithStatus 500 ()]
  }
  deriving stock (Generic)

type RequestSpaceCreate :: Type
data RequestSpaceCreate = MkRequestSpaceCreate
  { requestSpaceCreateName :: T.Text
  , requestSpaceCreateVisible :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") RequestSpaceCreate

type RequestSpaceList :: Type
newtype RequestSpaceList = MkRequestSpaceList
  { requestSpaceList :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") RequestSpaceList

type ResponseSpaceList :: Type
newtype ResponseSpaceList = MkResponseSpaceList
  { responseSpaceListSpaces :: [Space]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") ResponseSpaceList

type RequestDeskCreate :: Type
data RequestDeskCreate = MkRequestDeskCreate
  { requestDeskCreateName :: T.Text
  , requestDeskCreateSpace :: Either T.Text (Selda.Identifier DbSpace)
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") RequestDeskCreate

type RequestDeskList :: Type
newtype RequestDeskList = MkRequestDeskList
  { requestDeskListSpace :: Either T.Text (Selda.Identifier DbSpace)
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestDeskList") RequestDeskList

type ResponseDeskList :: Type
newtype ResponseDeskList = MkResponseDeskList
  { responseDeskListDesks :: [Desk]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseDeskList") ResponseDeskList
