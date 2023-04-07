module Mensam.API.Data.Reservation where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.User

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Time qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics

type Reservation :: Type
data Reservation = MkReservation
  { reservationId :: IdentifierReservation
  , reservationDesk :: IdentifierDesk
  , reservationUser :: IdentifierUser
  , reservationTimeBegin :: T.UTCTime
  , reservationTimeEnd :: T.UTCTime
  , reservationStatus :: StatusReservation
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type IdentifierReservation :: Type
newtype IdentifierReservation = MkIdentifierReservation {unIdentifierReservation :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (A.FromJSON, A.ToJSON)

type StatusReservation :: Type
data StatusReservation
  = MkStatusReservationPlanned
  | MkStatusReservationCancelled
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkStatusReservation" "") StatusReservation

type Interval :: Type -> Type
data Interval a = MkIntervalUnsafe
  { intervalStart :: a
  , intervalEnd :: a
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

instance (A.FromJSON a, Ord a) => A.FromJSON (Interval a) where
  parseJSON value = do
    A.CustomJSON intervalUnsafe <- A.parseJSON @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval a)) value
    case mkInterval (intervalStart intervalUnsafe) (intervalEnd intervalUnsafe) of
      Nothing -> fail "expected ordered interval, but encountered boundaries in the wrong order"
      Just interval -> pure interval
deriving via A.CustomJSON (JSONSettings "Mk" "interval") (Interval a) instance A.ToJSON a => A.ToJSON (Interval a)

mkInterval :: Ord a => a -> a -> Maybe (Interval a)
mkInterval intervalStart intervalEnd =
  if intervalStart <= intervalEnd
    then Just MkIntervalUnsafe {intervalStart, intervalEnd}
    else Nothing
