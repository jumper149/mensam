module Mensam.API.Route.Booking.Type where

import Mensam.API.Aeson
import Mensam.API.Desk
import Mensam.API.Order
import Mensam.API.Space
import Mensam.API.User

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Time qualified as T
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
              \You will be an administrator of this newly created space.\n"
          :> "space"
          :> "create"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ()
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBasicAuth
              , WithStatus 500 ()
              ]
  , routeSpaceList ::
      route
        :- Summary "List Spaces"
          :> Description
              "List visible spaces.\n"
          :> "space"
          :> "list"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceList
          :> UVerb
              GET
              '[JSON]
              [ WithStatus 200 ResponseSpaceList
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBasicAuth
              , WithStatus 500 ()
              ]
  , routeDeskCreate ::
      route
        :- Summary "Create Desk"
          :> Description
              "Create a new desk.\n\
              \This desk will belong to the given space.\n\
              \You have to be an administrator for that space to create desks.\n"
          :> "desk"
          :> "create"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ()
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBasicAuth
              , WithStatus 500 ()
              ]
  , routeDeskList ::
      route
        :- Summary "List Desks"
          :> Description
              "List desks.\n"
          :> "desk"
          :> "list"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskList
          :> UVerb
              GET
              '[JSON]
              [ WithStatus 200 ResponseDeskList
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBasicAuth
              , WithStatus 500 ()
              ]
  , routeReservationCreate ::
      route
        :- Summary "Create Reservation"
          :> Description
              "Request a desk reservation.\n\
              \A desk can only be reserved by one user at any time.\n"
          :> "reservation"
          :> "create"
          :> Auth '[JWT] User
          :> ReqBody' '[Lenient, Required] '[JSON] RequestReservationCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseReservationCreate
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBasicAuth
              , WithStatus 409 ()
              , WithStatus 500 ()
              ]
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
  { requestSpaceListOrder :: OrderByCategories SpaceOrderCategory
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
  , requestDeskCreateSpace :: NameOrIdentifier T.Text IdentifierSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") RequestDeskCreate

type RequestDeskList :: Type
newtype RequestDeskList = MkRequestDeskList
  { requestDeskListSpace :: NameOrIdentifier T.Text IdentifierSpace
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

type RequestReservationCreate :: Type
data RequestReservationCreate = MkRequestReservationCreate
  { requestReservationCreateDesk :: NameOrIdentifier T.Text IdentifierDesk
  , requestReservationCreateTimeBegin :: T.UTCTime
  , requestReservationCreateTimeEnd :: T.UTCTime
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCreate") RequestReservationCreate

type ResponseReservationCreate :: Type
newtype ResponseReservationCreate = MkResponseReservationCreate
  { responseReservationCreateUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCreate") ResponseReservationCreate
