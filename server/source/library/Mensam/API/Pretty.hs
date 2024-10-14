module Mensam.API.Pretty where

import Data.Kind
import Data.Text
import Text.Blaze.Html5

type ToPrettyText :: Type -> Constraint
class ToPrettyText a where
  toPrettyText :: a -> Text

type ToPrettyHtml5 :: Type -> Constraint
class ToPrettyHtml5 a where
  toPrettyHtml5 :: a -> Html

type PrettyTextViaShow :: Type -> Type
newtype PrettyTextViaShow a = MkPrettyTextViaShow {unPrettyTextViaShow :: a}

instance Show a => ToPrettyText (PrettyTextViaShow a) where
  toPrettyText = pack . show . unPrettyTextViaShow

type PrettyHtml5ViaPrettyText :: Type -> Type
newtype PrettyHtml5ViaPrettyText a = MkPrettyHtml5ViaPrettyText {unPrettyHtml5ViaPrettyText :: a}

instance ToPrettyText a => ToPrettyHtml5 (PrettyHtml5ViaPrettyText a) where
  toPrettyHtml5 = toHtml . toPrettyText . unPrettyHtml5ViaPrettyText
