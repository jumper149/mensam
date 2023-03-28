module Mensam.API.Order where

import Mensam.API.Aeson

import Data.Aeson qualified as A
import Data.Kind
import Deriving.Aeson qualified as A
import GHC.Generics

type Order :: Type
data Order = Ascending | Descending
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "" "") Order

type OrderByCategory :: Type -> Type
data OrderByCategory a = MkOrderByCategory
  { orderByCategoryCategory :: a
  , orderByCategoryOrder :: Order
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "orderByCategory") (OrderByCategory a)

type OrderByCategories :: Type -> Type
newtype OrderByCategories a = MkOrderByCategories {unOrderByCategories :: [OrderByCategory a]}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)
