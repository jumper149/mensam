module Mensam.API.Desk where

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Text qualified as T
import GHC.Generics

type Desk :: Type
data Desk = MkDesk
  { deskId :: IdentifierDesk
  , deskName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type IdentifierDesk :: Type
newtype IdentifierDesk = MkIdentifierDesk {unIdentifierDesk :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)
