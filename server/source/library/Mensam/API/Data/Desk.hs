module Mensam.API.Data.Desk where

import Mensam.API.Aeson
import Mensam.API.Data.Space
import Mensam.API.Pretty

import Data.Aeson qualified as A
import Data.Int
import Data.Kind
import Data.Proxy
import Data.Text qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import GHC.TypeLits

type Desk :: Type
data Desk = MkDesk
  { deskId :: IdentifierDesk
  , deskSpace :: IdentifierSpace
  , deskName :: NameDesk
  , deskLocation :: Maybe LocationDesk
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "desk") Desk

type IdentifierDesk :: Type
newtype IdentifierDesk = MkIdentifierDesk {unIdentifierDesk :: Int64}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

instance ToPrettyText IdentifierDesk where
  toPrettyText = ("#" <>) . T.pack . show . unIdentifierDesk

deriving via PrettyHtml5ViaPrettyText IdentifierDesk instance ToPrettyHtml5 IdentifierDesk

type NameDesk :: Type
newtype NameDesk = MkNameDesk {unNameDesk :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

deriving via PrettyTextViaShow T.Text instance ToPrettyText NameDesk
deriving via PrettyHtml5ViaPrettyText NameDesk instance ToPrettyHtml5 NameDesk

type DeskNameWithContext :: Type
data DeskNameWithContext = MkDeskNameWithContext
  { deskNameWithContextDesk :: NameDesk
  , deskNameWithContextSpace :: NameSpace
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "deskNameWithContext") DeskNameWithContext

type LocationDesk :: Type
data LocationDesk = MkLocationDesk
  { locationDeskPosition :: PositionDesk
  , locationDeskDirection :: DirectionDesk
  , locationDeskSize :: SizeDesk
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "locationDesk") LocationDesk

type PositionDesk :: Type
data PositionDesk = MkPositionDesk
  { positionDeskX :: ConstrainedDouble '[]
  , positionDeskY :: ConstrainedDouble '[]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "positionDesk") PositionDesk

type DirectionDesk :: Type
newtype DirectionDesk = MkDirectionDesk {unDirectionDesk :: Direction}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type SizeDesk :: Type
data SizeDesk = MkSizeDesk
  { sizeDeskWidth :: ConstrainedDouble '[MkConstraintDoubleGreaterEqual 30, MkConstraintDoubleLessEqual 600]
  , sizeDeskDepth :: ConstrainedDouble '[MkConstraintDoubleGreaterEqual 30, MkConstraintDoubleLessEqual 600]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "Mk" "sizeDesk") SizeDesk

-- | Direction on a 2D plane given as the angle relativ to North.
--
-- [North]:   0
-- [East]:   90
-- [South]: 180
-- [West]:  270
type Direction :: Type
newtype Direction = MkDirectionDegrees {unDirectionDegrees :: ConstrainedDouble '[MkConstraintDoubleGreaterEqual 0, MkConstraintDoubleLessThan 360]}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type ConstrainedDouble :: [ConstraintDouble] -> Type
newtype ConstrainedDouble constraints = MkConstrainedDoubleUnsafe {unConstrainedDouble :: Double}
  deriving stock (Eq, Generic, Ord, Read, Show)

instance A.FromJSON (ConstrainedDouble '[]) where
  parseJSON val = do
    parsedDouble <- A.parseJSON val
    if parsedDouble < (1 / 0) && parsedDouble > (-1 / 0)
      then pure MkConstrainedDoubleUnsafe {unConstrainedDouble = parsedDouble}
      else fail "Parsing constrained double failed: Not a number"
instance (A.FromJSON (ConstrainedDouble cs), KnownNat n) => A.FromJSON (ConstrainedDouble (MkConstraintDoubleGreaterEqual n : cs)) where
  parseJSON val = do
    parsedConstrainedDouble :: ConstrainedDouble cs <- A.parseJSON val
    let
      parsedDouble = unConstrainedDouble parsedConstrainedDouble
      minGEDouble = fromInteger $ natVal (Proxy :: Proxy n)
    if parsedDouble >= minGEDouble
      then pure MkConstrainedDoubleUnsafe {unConstrainedDouble = parsedDouble}
      else fail $ "Parsing constrained double failed: Expected n >= " ++ show minGEDouble
instance (A.FromJSON (ConstrainedDouble cs), KnownNat n) => A.FromJSON (ConstrainedDouble (MkConstraintDoubleGreaterThan n : cs)) where
  parseJSON val = do
    parsedConstrainedDouble :: ConstrainedDouble cs <- A.parseJSON val
    let
      parsedDouble = unConstrainedDouble parsedConstrainedDouble
      minGTDouble = fromInteger $ natVal (Proxy :: Proxy n)
    if parsedDouble >= minGTDouble
      then pure MkConstrainedDoubleUnsafe {unConstrainedDouble = parsedDouble}
      else fail $ "Parsing constrained double failed: Expected n > " ++ show minGTDouble
instance (A.FromJSON (ConstrainedDouble cs), KnownNat n) => A.FromJSON (ConstrainedDouble (MkConstraintDoubleLessEqual n : cs)) where
  parseJSON val = do
    parsedConstrainedDouble :: ConstrainedDouble cs <- A.parseJSON val
    let
      parsedDouble = unConstrainedDouble parsedConstrainedDouble
      maxLEDouble = fromInteger $ natVal (Proxy :: Proxy n)
    if parsedDouble <= maxLEDouble
      then pure MkConstrainedDoubleUnsafe {unConstrainedDouble = parsedDouble}
      else fail $ "Parsing constrained double failed: Expected n <= " ++ show maxLEDouble
instance (A.FromJSON (ConstrainedDouble cs), KnownNat n) => A.FromJSON (ConstrainedDouble (MkConstraintDoubleLessThan n : cs)) where
  parseJSON val = do
    parsedConstrainedDouble :: ConstrainedDouble cs <- A.parseJSON val
    let
      parsedDouble = unConstrainedDouble parsedConstrainedDouble
      maxLTDouble = fromInteger $ natVal (Proxy :: Proxy n)
    if parsedDouble <= maxLTDouble
      then pure MkConstrainedDoubleUnsafe {unConstrainedDouble = parsedDouble}
      else fail $ "Parsing constrained double failed: Expected n < " ++ show maxLTDouble
deriving newtype instance A.ToJSON (ConstrainedDouble cs)

type ConstraintDouble :: Type
type data ConstraintDouble
  = MkConstraintDoubleGreaterEqual Natural
  | MkConstraintDoubleGreaterThan Natural
  | MkConstraintDoubleLessEqual Natural
  | MkConstraintDoubleLessThan Natural
