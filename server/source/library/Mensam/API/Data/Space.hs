module Mensam.API.Data.Space where

import Mensam.API.Aeson

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Zones.All qualified as T
import Data.Time.Zones.All.OrphanInstances ()
import Deriving.Aeson qualified as A
import GHC.Generics

type SpaceView :: Type
data SpaceView = MkSpaceView
  { spaceViewId :: IdentifierSpace
  , spaceViewName :: NameSpace
  , spaceViewTimezone :: T.TZLabel
  , spaceViewVisibility :: VisibilitySpace
  , spaceViewRoles :: S.Set SpaceRole
  , spaceViewPermissions :: S.Set PermissionSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "spaceView") SpaceView

type Space :: Type
data Space = MkSpace
  { spaceId :: IdentifierSpace
  , spaceName :: NameSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "space") Space

type IdentifierSpace :: Type
newtype IdentifierSpace = MkIdentifierSpace {unIdentifierSpace :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type NameSpace :: Type
newtype NameSpace = MkNameSpace {unNameSpace :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type VisibilitySpace :: Type
data VisibilitySpace
  = MkVisibilitySpaceVisible
  | MkVisibilitySpaceHidden
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkVisibilitySpace" "") VisibilitySpace

type PermissionSpace :: Type
data PermissionSpace
  = MkPermissionSpaceViewSpace
  | MkPermissionSpaceEditDesk
  | MkPermissionSpaceCreateReservation
  | MkPermissionSpaceCancelReservation
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkPermissionSpace" "") PermissionSpace

type SpaceRole :: Type
data SpaceRole = MkSpaceRole
  { spaceRoleId :: IdentifierSpaceRole
  , spaceRoleName :: NameSpaceRole
  , spaceRolePermissions :: S.Set PermissionSpace
  , spaceRoleAccessibility :: AccessibilitySpaceRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "spaceRole") SpaceRole

type IdentifierSpaceRole :: Type
newtype IdentifierSpaceRole = MkIdentifierSpaceRole {unIdentifierSpaceRole :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type NameSpaceRole :: Type
newtype NameSpaceRole = MkNameSpaceRole {unNameSpaceRole :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)
  deriving newtype (A.FromJSONKey, A.ToJSONKey)

type AccessibilitySpaceRole :: Type
data AccessibilitySpaceRole
  = MkAccessibilitySpaceRoleJoinable
  | MkAccessibilitySpaceRoleInaccessible
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkAccessibilitySpaceRole" "") AccessibilitySpaceRole

type SpaceOrderCategory :: Type
data SpaceOrderCategory
  = SpaceOrderCategoryId
  | SpaceOrderCategoryName
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "SpaceOrderCategory" "") SpaceOrderCategory
