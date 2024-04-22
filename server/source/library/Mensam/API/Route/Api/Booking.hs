module Mensam.API.Route.Api.Booking where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.User
import Mensam.API.Order
import Mensam.API.Update

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Time qualified as T
import Data.Time.Zones.All qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.JWT.WithSession

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
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseSpaceCreate
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 500 ()
              ]
  , routeSpaceDelete ::
      route
        :- Summary "Delete Space"
          :> Description
              "Delete a space irreversibly.\n\
              \This also purges data associated with this space including reservations, desks and member roles.\n"
          :> "space"
          :> "delete"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceDelete
          :> UVerb
              DELETE
              '[JSON]
              [ WithStatus 200 ResponseSpaceDelete
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (StaticText "Insufficient permission.")
              , WithStatus 404 (StaticText "Space not found.")
              , WithStatus 500 ()
              ]
  , routeSpaceEdit ::
      route
        :- Summary "Edit Space"
          :> Description
              "Update the configuration of a space.\n"
          :> "space"
          :> "edit"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceEdit
          :> UVerb
              PATCH
              '[JSON]
              [ WithStatus 200 ResponseSpaceEdit
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (StaticText "Insufficient permission.")
              , WithStatus 404 (StaticText "Space not found.")
              , WithStatus 500 ()
              ]
  , routeSpaceJoin ::
      route
        :- Summary "Join Space"
          :> Description
              "Become a member of a space.\n"
          :> "space"
          :> "join"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceJoin
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseSpaceJoin
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (StaticText "Wrong space password.")
              , WithStatus 500 ()
              ]
  , routeSpaceLeave ::
      route
        :- Summary "Leave Space"
          :> Description
              "Abandon membership of a space.\n"
          :> "space"
          :> "leave"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceLeave
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseSpaceLeave
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (StaticText "Owner cannot leave space.")
              , WithStatus 500 ()
              ]
  , routeSpaceView ::
      route
        :- Summary "View Space"
          :> Description
              "View a single space in detail.\n"
          :> "space"
          :> "view"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceView
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseSpaceView
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (StaticText "Insufficient permission.")
              , WithStatus 500 ()
              ]
  , routeSpaceList ::
      route
        :- Summary "List Spaces"
          :> Description
              "List visible spaces.\n"
          :> "space"
          :> "list"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceList
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseSpaceList
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 500 ()
              ]
  , routeDeskCreate ::
      route
        :- Summary "Create Desk"
          :> Description
              "Create a new desk.\n\
              \This desk will belong to the given space.\n\
              \You need the `desk-edit` permission for that space to create desks.\n"
          :> "desk"
          :> "create"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseDeskCreate
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (StaticText "Insufficient permission.")
              , WithStatus 404 (StaticText "Space not found.")
              , WithStatus 500 ()
              ]
  , routeDeskDelete ::
      route
        :- Summary "Delete Desk"
          :> Description
              "Delete a desk.\n\
              \You need the `desk-edit` permission for that space to delete desks.\n"
          :> "desk"
          :> "delete"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskDelete
          :> UVerb
              DELETE
              '[JSON]
              [ WithStatus 200 ResponseDeskDelete
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (StaticText "Insufficient permission.")
              , WithStatus 404 (StaticText "Desk not found.")
              , WithStatus 500 ()
              ]
  , routeDeskList ::
      route
        :- Summary "List Desks"
          :> Description
              "List desks.\n"
          :> "desk"
          :> "list"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskList
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseDeskList
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
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
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestReservationCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseReservationCreate
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 409 (StaticText "Desk is not available within the given time window.")
              , WithStatus 500 ()
              ]
  , routeReservationCancel ::
      route
        :- Summary "Cancel Reservation"
          :> Description
              "Cancel a desk reservation.\n"
          :> "reservation"
          :> "cancel"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestReservationCancel
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseReservationCancel
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 500 ()
              ]
  , routeReservationList ::
      route
        :- Summary "List Reservations"
          :> Description
              "View all of your desk reservations.\n"
          :> "reservation"
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

type RequestSpaceCreate :: Type
data RequestSpaceCreate = MkRequestSpaceCreate
  { requestSpaceCreateName :: NameSpace
  , requestSpaceCreateTimezone :: T.TZLabel
  , requestSpaceCreateVisibility :: VisibilitySpace
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

type RequestSpaceDelete :: Type
newtype RequestSpaceDelete = MkRequestSpaceDelete
  { requestSpaceDeleteId :: IdentifierSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceDelete") RequestSpaceDelete

type ResponseSpaceDelete :: Type
newtype ResponseSpaceDelete = MkResponseSpaceDelete
  { responseSpaceDeleteUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceDelete") ResponseSpaceDelete

type RequestSpaceEdit :: Type
data RequestSpaceEdit = MkRequestSpaceEdit
  { requestSpaceEditId :: IdentifierSpace
  , requestSpaceEditName :: Updatable NameSpace
  , requestSpaceEditTimezone :: Updatable T.TZLabel
  , requestSpaceEditVisibility :: Updatable VisibilitySpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceEdit") RequestSpaceEdit

type ResponseSpaceEdit :: Type
data ResponseSpaceEdit = MkResponseSpaceEdit
  { responseSpaceEditId :: IdentifierSpace
  , responseSpaceEditName :: NameSpace
  , responseSpaceEditTimezone :: T.TZLabel
  , responseSpaceEditVisibility :: VisibilitySpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceEdit") ResponseSpaceEdit

type RequestSpaceJoin :: Type
data RequestSpaceJoin = MkRequestSpaceJoin
  { requestSpaceJoinSpace :: NameOrIdentifier NameSpace IdentifierSpace
  , requestSpaceJoinRole :: NameOrIdentifier NameSpaceRole IdentifierSpaceRole
  , requestSpaceJoinPassword :: Maybe T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceJoin") RequestSpaceJoin

type ResponseSpaceJoin :: Type
newtype ResponseSpaceJoin = MkResponseSpaceJoin
  { responseSpaceJoinUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceJoin") ResponseSpaceJoin

type RequestSpaceLeave :: Type
newtype RequestSpaceLeave = MkRequestSpaceLeave
  { requestSpaceLeaveSpace :: NameOrIdentifier NameSpace IdentifierSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceLeave") RequestSpaceLeave

type ResponseSpaceLeave :: Type
newtype ResponseSpaceLeave = MkResponseSpaceLeave
  { responseSpaceLeaveUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceLeave") ResponseSpaceLeave

type RequestSpaceView :: Type
newtype RequestSpaceView = MkRequestSpaceView
  { requestSpaceViewId :: IdentifierSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceView") RequestSpaceView

type ResponseSpaceView :: Type
newtype ResponseSpaceView = MkResponseSpaceView
  { responseSpaceViewSpace :: SpaceView
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceView") ResponseSpaceView

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
    via A.CustomJSON (JSONSettings "MkRequest" "requestDeskCreate") RequestDeskCreate

type ResponseDeskCreate :: Type
newtype ResponseDeskCreate = MkResponseDeskCreate
  { responseDeskCreateId :: IdentifierDesk
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseDeskCreate") ResponseDeskCreate

type RequestDeskDelete :: Type
newtype RequestDeskDelete = MkRequestDeskDelete
  { requestDeskDeleteId :: IdentifierDesk
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestDeskDelete") RequestDeskDelete

type ResponseDeskDelete :: Type
newtype ResponseDeskDelete = MkResponseDeskDelete
  { responseDeskDeleteUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseDeskDelete") ResponseDeskDelete

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
  { requestReservationCreateDesk :: NameOrIdentifier DeskNameWithContext IdentifierDesk
  , requestReservationCreateTimeWindow :: IntervalNonDegenerate T.UTCTime
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
data RequestReservationList = MkRequestReservationList
  { requestReservationListTimeBegin :: Maybe T.UTCTime
  , requestReservationListTimeEnd :: Maybe T.UTCTime
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
