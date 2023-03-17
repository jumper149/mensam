module Mensam.API.Reservation where

import Mensam.API.Desk
import Mensam.API.User

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Time qualified as T
import GHC.Generics

type Reservation :: Type
data Reservation = MkReservation
  { reservationId :: IdentifierReservation
  , reservationDesk :: IdentifierDesk
  , reservationUser :: IdentifierUser
  , reservationTimeBegin :: T.UTCTime
  , reservationTimeEnd :: T.UTCTime
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type IdentifierReservation :: Type
newtype IdentifierReservation = MkIdentifierReservation {unIdentifierReservation :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)
