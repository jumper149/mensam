module Mensam.API.Space where

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Text qualified as T
import GHC.Generics

type Space :: Type
data Space = MkSpace
  { spaceId :: IdentifierSpace
  , spaceName :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type IdentifierSpace :: Type
newtype IdentifierSpace = MkIdentifierSpace {unIdentifierSpace :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)
