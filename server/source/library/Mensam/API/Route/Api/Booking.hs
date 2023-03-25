module Mensam.API.Route.Api.Booking where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.User
import Mensam.API.Order

import Data.Aeson qualified as A
import Data.Kind
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
          :> Auth '[JWT] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseSpaceCreate
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
          :> Auth '[JWT] UserAuthenticated
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
          :> Auth '[JWT] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseDeskCreate
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
          :> Auth '[JWT] UserAuthenticated
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
          :> Auth '[JWT] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestReservationCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseReservationCreate
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBasicAuth
              , WithStatus 409 (StaticText "Desk is not available within the given time window.")
              , WithStatus 500 ()
              ]
  }
  deriving stock (Generic)

type RequestSpaceCreate :: Type
data RequestSpaceCreate = MkRequestSpaceCreate
  { requestSpaceCreateName :: NameSpace
  , requestSpaceCreateVisible :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") RequestSpaceCreate

type ResponseSpaceCreate :: Type
newtype ResponseSpaceCreate = MkResponseSpaceCreate
  { responseSpaceCreateId :: IdentifierSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceCreate") ResponseSpaceCreate

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
  { requestDeskCreateName :: NameDesk
  , requestDeskCreateSpace :: NameOrIdentifier NameSpace IdentifierSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") RequestDeskCreate

type ResponseDeskCreate :: Type
newtype ResponseDeskCreate = MkResponseDeskCreate
  { responseDeskCreateId :: IdentifierDesk
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseDeskCreate") ResponseDeskCreate

type RequestDeskList :: Type
data RequestDeskList = MkRequestDeskList
  { requestDeskListSpace :: NameOrIdentifier NameSpace IdentifierSpace
  , requestDeskListTimeBegin :: Maybe T.UTCTime
  , requestDeskListTimeEnd :: Maybe T.UTCTime
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestDeskList") RequestDeskList

type DeskWithInfo :: Type
data DeskWithInfo = MkDeskWithInfo
  { deskWithInfoDesk :: Desk
  , deskWithInfoReservations :: [Reservation]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "deskWithInfo") DeskWithInfo

type ResponseDeskList :: Type
newtype ResponseDeskList = MkResponseDeskList
  { responseDeskListDesks :: [DeskWithInfo]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseDeskList") ResponseDeskList

type RequestReservationCreate :: Type
data RequestReservationCreate = MkRequestReservationCreate
  { requestReservationCreateDesk :: NameOrIdentifier (NameSpace, NameDesk) IdentifierDesk
  , requestReservationCreateTimeBegin :: T.UTCTime
  , requestReservationCreateTimeEnd :: T.UTCTime
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCreate") RequestReservationCreate

type ResponseReservationCreate :: Type
newtype ResponseReservationCreate = MkResponseReservationCreate
  { responseReservationCreateId :: IdentifierReservation
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCreate") ResponseReservationCreate