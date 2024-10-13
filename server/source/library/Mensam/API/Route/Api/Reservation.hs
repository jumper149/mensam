module Mensam.API.Route.Api.Reservation where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User

import Data.Aeson qualified as A
import Data.Kind
import Data.Time qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.JWT.WithSession

type Routes :: Type -> Type
data Routes route = Routes
  { routeReservationCreate ::
      route
        :- Summary "Create Reservation"
          :> Description
              "Request a desk reservation.\n\
              \A desk can only be reserved by one user at any time.\n"
          :> "create"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestReservationCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseReservationCreate
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionCreateReservation)
              , WithStatus 409 (StaticText "Desk is not available within the given time window.")
              , WithStatus 500 ()
              ]
  , routeReservationCancel ::
      route
        :- Summary "Cancel Reservation"
          :> Description
              "Cancel a desk reservation.\n"
          :> "cancel"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestReservationCancel
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseReservationCancel
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionCancelReservation)
              , WithStatus 409 (StaticText "Already cancelled.")
              , WithStatus 410 (StaticText "Already happened.")
              , WithStatus 500 ()
              ]
  , routeReservationList ::
      route
        :- Summary "List Reservations"
          :> Description
              "View all of your desk reservations.\n\
              \Use the time-window to restrict the result to overlapping reservations.\n"
          :> "list"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestReservationList
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseReservationList
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 500 ()
              ]
  }
  deriving stock (Generic)

type RequestReservationCreate :: Type
data RequestReservationCreate = MkRequestReservationCreate
  { requestReservationCreateDesk :: NameOrIdentifier DeskNameWithContext IdentifierDesk
  , requestReservationCreateTimeWindow :: IntervalNonDegenerate T.UTCTime
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCreate") RequestReservationCreate

type ResponseReservationCreate :: Type
data ResponseReservationCreate = MkResponseReservationCreate
  { responseReservationCreateId :: IdentifierReservation
  , responseReservationCreateEmailSent :: Maybe Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCreate") ResponseReservationCreate

type RequestReservationCancel :: Type
newtype RequestReservationCancel = MkRequestReservationCancel
  { requestReservationCancelId :: IdentifierReservation
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCancel") RequestReservationCancel

type ResponseReservationCancel :: Type
newtype ResponseReservationCancel = MkResponseReservationCancel
  { responseReservationCancelUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCancel") ResponseReservationCancel

type RequestReservationList :: Type
newtype RequestReservationList = MkRequestReservationList
  { requestReservationListTimeWindow :: IntervalUnbounded T.UTCTime
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestReservationList") RequestReservationList

type ReservationWithInfo :: Type
data ReservationWithInfo = MkReservationWithInfo
  { reservationWithInfoReservation :: Reservation
  , reservationWithInfoDesk :: Desk
  , reservationWithInfoSpace :: Space
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "reservationWithInfo") ReservationWithInfo

type ResponseReservationList :: Type
newtype ResponseReservationList = MkResponseReservationList
  { responseReservationListReservations :: [ReservationWithInfo]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseReservationList") ResponseReservationList
