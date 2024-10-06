module Mensam.API.Route.Api.Space where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User
import Mensam.API.Order
import Mensam.API.Update

import Data.Aeson qualified as A
import Data.Kind
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time qualified as T
import Data.Time.Zones.All qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Numeric.Natural
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
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditSpace)
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
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditSpace)
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
              , WithStatus 403 (StaticTexts ["Role is inaccessible.", "Wrong role password."])
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
  , routeSpaceKick ::
      route
        :- Summary "Kick User from Space"
          :> Description
              "Kick a user out of a space.\n\
              \You need the `edit-user` permission for that space to remove users.\n"
          :> "space"
          :> "kick"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceKick
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseSpaceKick
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditUser)
              , WithStatus 500 ()
              ]
  , routeSpaceUserRole ::
      route
        :- Summary "Set User Role for Space"
          :> Description
              "Give a new role to a user of a space.\n\
              \You need the `edit-user` permission for that space to redefine user roles.\n"
          :> "space"
          :> "user"
          :> "role"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestSpaceUserRole
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseSpaceUserRole
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditUser)
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
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceViewSpace)
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
  , routeRoleCreate ::
      route
        :- Summary "Create Role"
          :> Description
              "Create a new role.\n\
              \This role will be a way to access the given space.\n\
              \You need the `edit-role` permission for that space to create roles.\n"
          :> "role"
          :> "create"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestRoleCreate
          :> UVerb
              PUT
              '[JSON]
              [ WithStatus 201 ResponseRoleCreate
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)
              , WithStatus 404 (StaticText "Space not found.")
              , WithStatus 500 ()
              ]
  , routeRoleEdit ::
      route
        :- Summary "Edit Role"
          :> Description
              "Update settings of a role.\n\
              \You need the `edit-role` permission for the space to edit roles.\n"
          :> "role"
          :> "edit"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestRoleEdit
          :> UVerb
              PATCH
              '[JSON]
              [ WithStatus 200 ResponseRoleEdit
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)
              , WithStatus 500 ()
              ]
  , routeRoleDelete ::
      route
        :- Summary "Delete Role"
          :> Description
              "Delete a role.\n\
              \You have to provide a fallback role to reassign members to that fallback role.\n\
              \You need the `edit-role` permission for the space to delete roles.\n"
          :> "role"
          :> "delete"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestRoleDelete
          :> UVerb
              DELETE
              '[JSON]
              [ WithStatus 200 ResponseRoleDelete
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)
              , WithStatus 500 ()
              ]
  , routeDeskCreate ::
      route
        :- Summary "Create Desk"
          :> Description
              "Create a new desk.\n\
              \This desk will belong to the given space.\n\
              \You need the `edit-desk` permission for that space to create desks.\n"
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
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)
              , WithStatus 404 (StaticText "Space not found.")
              , WithStatus 500 ()
              ]
  , routeDeskDelete ::
      route
        :- Summary "Delete Desk"
          :> Description
              "Delete a desk.\n\
              \You need the `edit-desk` permission for that space to delete desks.\n"
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
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)
              , WithStatus 404 (StaticText "Desk not found.")
              , WithStatus 500 ()
              ]
  , routeDeskEdit ::
      route
        :- Summary "Edit Desk"
          :> Description
              "Update a desk.\n\
              \You need the `edit-desk` permission for that space to edit desks.\n"
          :> "desk"
          :> "edit"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestDeskEdit
          :> UVerb
              PATCH
              '[JSON]
              [ WithStatus 200 ResponseDeskEdit
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)
              , WithStatus 404 (StaticText "Desk not found.")
              , WithStatus 500 ()
              ]
  , routeDeskList ::
      route
        :- Summary "List Desks"
          :> Description
              "List desks.\n\
              \Use the time-window to restrict the reservations in the result to overlapping time frames.\n"
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
              , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceViewSpace)
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

type RequestSpaceKick :: Type
data RequestSpaceKick = MkRequestSpaceKick
  { requestSpaceKickSpace :: IdentifierSpace
  , requestSpaceKickUser :: IdentifierUser
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceKick") RequestSpaceKick

type ResponseSpaceKick :: Type
newtype ResponseSpaceKick = MkResponseSpaceKick
  { responseSpaceKickUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceKick") ResponseSpaceKick

type RequestSpaceUserRole :: Type
data RequestSpaceUserRole = MkRequestSpaceUserRole
  { requestSpaceUserRoleSpace :: IdentifierSpace
  , requestSpaceUserRoleUser :: IdentifierUser
  , requestSpaceUserRoleRole :: IdentifierSpaceRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceUserRole") RequestSpaceUserRole

type ResponseSpaceUserRole :: Type
newtype ResponseSpaceUserRole = MkResponseSpaceUserRole
  { responseSpaceUserRoleUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceUserRole") ResponseSpaceUserRole

type RequestSpaceView :: Type
newtype RequestSpaceView = MkRequestSpaceView
  { requestSpaceViewId :: IdentifierSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceView") RequestSpaceView

type ResponseSpaceView :: Type
data ResponseSpaceView = MkResponseSpaceView
  { responseSpaceViewId :: IdentifierSpace
  , responseSpaceViewName :: NameSpace
  , responseSpaceViewTimezone :: T.TZLabel
  , responseSpaceViewVisibility :: VisibilitySpace
  , responseSpaceViewOwner :: IdentifierUser
  , responseSpaceViewRoles :: S.Set SpaceRole
  , responseSpaceViewUsers :: S.Set SpaceUser
  , responseSpaceViewYourRole :: Maybe IdentifierSpaceRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceView") ResponseSpaceView

type RequestSpaceList :: Type
data RequestSpaceList = MkRequestSpaceList
  { requestSpaceListOrder :: OrderByCategories SpaceOrderCategory
  , requestSpaceListMember :: Maybe Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") RequestSpaceList

type ResponseSpaceList :: Type
newtype ResponseSpaceList = MkResponseSpaceList
  { responseSpaceListSpaces :: [SpaceListSpace]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") ResponseSpaceList

type SpaceListSpace :: Type
data SpaceListSpace = MkSpaceListSpace
  { spaceListSpaceId :: IdentifierSpace
  , spaceListSpaceName :: NameSpace
  , spaceListSpaceTimezone :: T.TZLabel
  , spaceListSpaceOwner :: IdentifierUser
  , spaceListSpaceUsers :: Natural
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "spaceListSpace") SpaceListSpace

type RequestRoleCreate :: Type
data RequestRoleCreate = MkRequestRoleCreate
  { requestRoleCreateSpace :: IdentifierSpace
  , requestRoleCreateName :: NameSpaceRole
  , requestRoleCreateAccessibility :: AccessibilitySpaceRole
  , requestRoleCreatePassword :: Maybe T.Text
  , requestRoleCreatePermissions :: S.Set PermissionSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestRoleCreate") RequestRoleCreate

type ResponseRoleCreate :: Type
newtype ResponseRoleCreate = MkResponseRoleCreate
  { responseRoleCreateId :: IdentifierSpaceRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseRoleCreate") ResponseRoleCreate

type RequestRoleEdit :: Type
data RequestRoleEdit = MkRequestRoleEdit
  { requestRoleEditId :: IdentifierSpaceRole
  , requestRoleEditName :: Updatable NameSpaceRole
  , requestRoleEditAccessibilityAndPassword :: Updatable RoleEditAccessibilityAndPassword
  , requestRoleEditPermissions :: Updatable (S.Set PermissionSpace)
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestRoleEdit") RequestRoleEdit

type RoleEditAccessibilityAndPassword :: Type
data RoleEditAccessibilityAndPassword = MkRoleEditAccessibilityAndPassword
  { roleEditAccessibilityAndPasswordAccessibility :: AccessibilitySpaceRole
  , roleEditAccessibilityAndPasswordPassword :: Maybe T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "RoleEdit" "roleEditAccessibilityAndPassword") RoleEditAccessibilityAndPassword

type ResponseRoleEdit :: Type
newtype ResponseRoleEdit = MkResponseRoleEdit
  { responseRoleEditUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseRoleEdit") ResponseRoleEdit

type RequestRoleDelete :: Type
data RequestRoleDelete = MkRequestRoleDelete
  { requestRoleDeleteId :: IdentifierSpaceRole
  , requestRoleDeleteFallbackId :: IdentifierSpaceRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestRoleDelete") RequestRoleDelete

type ResponseRoleDelete :: Type
newtype ResponseRoleDelete = MkResponseRoleDelete
  { responseRoleDeleteUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseRoleDelete") ResponseRoleDelete

type RequestDeskCreate :: Type
data RequestDeskCreate = MkRequestDeskCreate
  { requestDeskCreateName :: NameDesk
  , requestDeskCreateSpace :: NameOrIdentifier NameSpace IdentifierSpace
  , requestDeskCreateLocation :: Maybe LocationDesk
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

type RequestDeskEdit :: Type
data RequestDeskEdit = MkRequestDeskEdit
  { requestDeskEditId :: IdentifierDesk
  , requestDeskEditName :: Updatable NameDesk
  , requestDeskEditLocation :: Updatable (Maybe LocationDesk)
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestDeskEdit") RequestDeskEdit

type ResponseDeskEdit :: Type
newtype ResponseDeskEdit = MkResponseDeskEdit
  { responseDeskEditUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseDeskEdit") ResponseDeskEdit

type RequestDeskList :: Type
data RequestDeskList = MkRequestDeskList
  { requestDeskListSpace :: NameOrIdentifier NameSpace IdentifierSpace
  , requestDeskListTimeWindow :: IntervalUnbounded T.UTCTime
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
