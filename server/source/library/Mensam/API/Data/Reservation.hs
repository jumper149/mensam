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
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "reservation") Reservation

type IdentifierReservation :: Type
newtype IdentifierReservation = MkIdentifierReservation {unIdentifierReservation :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

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

intervalIsDegenerate :: Ord a => Interval a -> Bool
intervalIsDegenerate interval = intervalStart interval == intervalEnd interval

type IntervalNonDegenerate :: Type -> Type
newtype IntervalNonDegenerate a = MkIntervalNonDegenerateUnsafe {unIntervalNonDegenerate :: Interval a}
  deriving stock (Eq, Generic, Ord, Read, Show)

instance (A.FromJSON a, Ord a) => A.FromJSON (IntervalNonDegenerate a) where
  parseJSON value = do
    interval <- A.parseJSON value
    if intervalIsDegenerate interval
      then fail "expected non-degenerate interval, but boundaries are equal"
      else pure $ MkIntervalNonDegenerateUnsafe interval
deriving newtype instance A.ToJSON a => A.ToJSON (IntervalNonDegenerate a)

type IntervalUnbounded :: Type -> Type
data IntervalUnbounded a = MkIntervalUnboundedUnsafe
  { intervalUnboundedStart :: MaybeUnboundedLow a
  , intervalUnboundedEnd :: MaybeUnboundedHigh a
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

instance (A.FromJSON a, Ord a) => A.FromJSON (IntervalUnbounded a) where
  parseJSON value = do
    A.CustomJSON intervalUnsafe <- A.parseJSON @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval (Maybe a))) value
    let
      low = maybe NothingUnboundedLow JustUnboundedLow $ intervalStart intervalUnsafe
      high = maybe NothingUnboundedHigh JustUnboundedHigh $ intervalEnd intervalUnsafe
    case mkIntervalUnbounded low high of
      Nothing -> fail "expected ordered (potentially unbounded) interval, but encountered boundaries in the wrong order"
      Just interval -> pure interval
instance A.ToJSON a => A.ToJSON (IntervalUnbounded a) where
  toJSON value =
    A.toJSON $
      MkIntervalUnsafe
        { intervalStart =
            case intervalUnboundedStart value of
              NothingUnboundedLow -> Nothing
              JustUnboundedLow x -> Just x
        , intervalEnd =
            case intervalUnboundedEnd value of
              NothingUnboundedHigh -> Nothing
              JustUnboundedHigh x -> Just x
        }

mkIntervalUnbounded :: Ord a => MaybeUnboundedLow a -> MaybeUnboundedHigh a -> Maybe (IntervalUnbounded a)
mkIntervalUnbounded intervalUnboundedStart intervalUnboundedEnd =
  if MkMaybeUnboundedLow intervalUnboundedStart <= MkMaybeUnboundedHigh intervalUnboundedEnd
    then Just MkIntervalUnboundedUnsafe {intervalUnboundedStart, intervalUnboundedEnd}
    else Nothing

unbounded :: IntervalUnbounded a
unbounded =
  MkIntervalUnboundedUnsafe
    { intervalUnboundedStart = NothingUnboundedLow
    , intervalUnboundedEnd = NothingUnboundedHigh
    }

intervalUnboundedIsDegenerate :: Ord a => IntervalUnbounded a -> Bool
intervalUnboundedIsDegenerate intervalUnbounded = MkMaybeUnboundedLow (intervalUnboundedStart intervalUnbounded) == MkMaybeUnboundedHigh (intervalUnboundedEnd intervalUnbounded)

type IntervalUnboundedNonDegenerate :: Type -> Type
newtype IntervalUnboundedNonDegenerate a = MkIntervalUnboundedNonDegenerateUnsafe {unIntervalUnboundedNonDegenerate :: IntervalUnbounded a}
  deriving stock (Eq, Generic, Ord, Read, Show)

instance (A.FromJSON a, Ord a) => A.FromJSON (IntervalUnboundedNonDegenerate a) where
  parseJSON value = do
    intervalUnbounded <- A.parseJSON value
    if intervalUnboundedIsDegenerate intervalUnbounded
      then fail "expected non-degenerate (potentially unbounded) interval, but boundaries are equal"
      else pure $ MkIntervalUnboundedNonDegenerateUnsafe intervalUnbounded
deriving newtype instance A.ToJSON a => A.ToJSON (IntervalUnboundedNonDegenerate a)

type MaybeUnbounded :: Type -> Type
data MaybeUnbounded a
  = MkMaybeUnboundedLow (MaybeUnboundedLow a)
  | MkMaybeUnboundedHigh (MaybeUnboundedHigh a)
  deriving stock (Generic, Read, Show)

instance Eq a => Eq (MaybeUnbounded a) where
  (==) = \cases
    (MkMaybeUnboundedLow x) (MkMaybeUnboundedLow y) -> (==) x y
    (MkMaybeUnboundedHigh x) (MkMaybeUnboundedHigh y) -> (==) x y
    (MkMaybeUnboundedLow NothingUnboundedLow) (MkMaybeUnboundedHigh _) -> False
    (MkMaybeUnboundedLow (JustUnboundedLow _)) (MkMaybeUnboundedHigh NothingUnboundedHigh) -> False
    (MkMaybeUnboundedHigh NothingUnboundedHigh) (MkMaybeUnboundedLow _) -> False
    (MkMaybeUnboundedHigh _) (MkMaybeUnboundedLow NothingUnboundedLow) -> False
    (MkMaybeUnboundedLow (JustUnboundedLow x)) (MkMaybeUnboundedHigh (JustUnboundedHigh y)) -> (==) x y
    (MkMaybeUnboundedHigh (JustUnboundedHigh x)) (MkMaybeUnboundedLow (JustUnboundedLow y)) -> (==) x y
instance Ord a => Ord (MaybeUnbounded a) where
  compare = \cases
    (MkMaybeUnboundedLow x) (MkMaybeUnboundedLow y) -> compare x y
    (MkMaybeUnboundedHigh x) (MkMaybeUnboundedHigh y) -> compare x y
    (MkMaybeUnboundedLow NothingUnboundedLow) (MkMaybeUnboundedHigh _) -> LT
    (MkMaybeUnboundedLow (JustUnboundedLow _)) (MkMaybeUnboundedHigh NothingUnboundedHigh) -> LT
    (MkMaybeUnboundedHigh NothingUnboundedHigh) (MkMaybeUnboundedLow _) -> GT
    (MkMaybeUnboundedHigh _) (MkMaybeUnboundedLow NothingUnboundedLow) -> GT
    (MkMaybeUnboundedLow (JustUnboundedLow x)) (MkMaybeUnboundedHigh (JustUnboundedHigh y)) -> compare x y
    (MkMaybeUnboundedHigh (JustUnboundedHigh x)) (MkMaybeUnboundedLow (JustUnboundedLow y)) -> compare x y

type MaybeUnboundedLow :: Type -> Type
data MaybeUnboundedLow a
  = NothingUnboundedLow
  | JustUnboundedLow a
  deriving stock (Eq, Generic, Read, Show)

instance Ord a => Ord (MaybeUnboundedLow a) where
  compare = \cases
    NothingUnboundedLow NothingUnboundedLow -> EQ
    NothingUnboundedLow (JustUnboundedLow _) -> LT
    (JustUnboundedLow x) (JustUnboundedLow y) -> compare x y
    (JustUnboundedLow _) NothingUnboundedLow -> GT

type MaybeUnboundedHigh :: Type -> Type
data MaybeUnboundedHigh a
  = NothingUnboundedHigh
  | JustUnboundedHigh a
  deriving stock (Eq, Generic, Read, Show)

instance Ord a => Ord (MaybeUnboundedHigh a) where
  compare = \cases
    NothingUnboundedHigh NothingUnboundedHigh -> EQ
    NothingUnboundedHigh (JustUnboundedHigh _) -> GT
    (JustUnboundedHigh x) (JustUnboundedHigh y) -> compare x y
    (JustUnboundedHigh _) NothingUnboundedHigh -> LT
