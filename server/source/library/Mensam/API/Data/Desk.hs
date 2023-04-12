module Mensam.API.Data.Desk where

import Mensam.API.Aeson
import Mensam.API.Data.Space

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Text qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics

type Desk :: Type
data Desk = MkDesk
  { deskId :: IdentifierDesk
  , deskSpace :: IdentifierSpace
  , deskName :: NameDesk
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "desk") Desk

type IdentifierDesk :: Type
newtype IdentifierDesk = MkIdentifierDesk {unIdentifierDesk :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type NameDesk :: Type
newtype NameDesk = MkNameDesk {unNameDesk :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type DeskNameWithContext :: Type
data DeskNameWithContext = MkDeskNameWithContext
  { deskNameWithContextDesk :: NameDesk
  , deskNameWithContextSpace :: NameSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "deskNameWithContext") DeskNameWithContext
