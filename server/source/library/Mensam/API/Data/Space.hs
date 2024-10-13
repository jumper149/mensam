module Mensam.API.Data.Space where

import Mensam.API.Aeson
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Zones.All qualified as T
import Data.Time.Zones.All.OrphanInstances ()
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API qualified as Servant

type Space :: Type
data Space = MkSpace
  { spaceId :: IdentifierSpace
  , spaceName :: NameSpace
  , spaceTimezone :: T.TZLabel
  , spaceOwner :: IdentifierUser
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "space") Space

type IdentifierSpace :: Type
newtype IdentifierSpace = MkIdentifierSpace {unIdentifierSpace :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)

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

type Role :: Type
data Role = MkRole
  { roleId :: IdentifierRole
  , roleSpace :: IdentifierSpace
  , roleName :: NameRole
  , rolePermissions :: S.Set PermissionSpace
  , roleAccessibility :: AccessibilityRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "role") Role

type IdentifierRole :: Type
newtype IdentifierRole = MkIdentifierRole {unIdentifierRole :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type NameRole :: Type
newtype NameRole = MkNameRole {unNameRole :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)
  deriving newtype (A.FromJSONKey, A.ToJSONKey)

type AccessibilityRole :: Type
data AccessibilityRole
  = MkAccessibilityRoleJoinable
  | MkAccessibilityRoleJoinableWithPassword
  | MkAccessibilityRoleInaccessible
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkAccessibilityRole" "") AccessibilityRole

type SpaceOrderCategory :: Type
data SpaceOrderCategory
  = SpaceOrderCategoryId
  | SpaceOrderCategoryName
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "SpaceOrderCategory" "") SpaceOrderCategory

type SpaceUser :: Type
data SpaceUser = MkSpaceUser
  { spaceUserUser :: IdentifierUser
  , spaceUserRole :: IdentifierRole
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "spaceUser") SpaceUser
