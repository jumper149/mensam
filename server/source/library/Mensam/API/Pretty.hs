module Mensam.API.Pretty where

import Data.Kind
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Format.ISO8601 qualified as Time
import Data.Time.LocalTime qualified as Time
import Data.Time.Zones.All qualified as Time
import Text.Blaze.Html5 qualified as Html5

type ToPrettyText :: Type -> Constraint
class ToPrettyText a where
  toPrettyText :: a -> Text.Text

type ToPrettyHtml5 :: Type -> Constraint
class ToPrettyHtml5 a where
  toPrettyHtml5 :: a -> Html5.Html

type PrettyTextViaShow :: Type -> Type
newtype PrettyTextViaShow a = MkPrettyTextViaShow {unPrettyTextViaShow :: a}

instance Show a => ToPrettyText (PrettyTextViaShow a) where
  toPrettyText = Text.pack . show . unPrettyTextViaShow

type PrettyHtml5ViaPrettyText :: Type -> Type
newtype PrettyHtml5ViaPrettyText a = MkPrettyHtml5ViaPrettyText {unPrettyHtml5ViaPrettyText :: a}

instance ToPrettyText a => ToPrettyHtml5 (PrettyHtml5ViaPrettyText a) where
  toPrettyHtml5 = Html5.toHtml . toPrettyText . unPrettyHtml5ViaPrettyText

instance ToPrettyText Time.TZLabel where
  toPrettyText = Text.Encoding.decodeUtf8 . Time.toTZName

deriving via PrettyHtml5ViaPrettyText Time.TZLabel instance ToPrettyHtml5 Time.TZLabel

instance ToPrettyText Time.LocalTime where
  toPrettyText = Text.pack . Time.iso8601Show

deriving via PrettyHtml5ViaPrettyText Time.LocalTime instance ToPrettyHtml5 Time.LocalTime
