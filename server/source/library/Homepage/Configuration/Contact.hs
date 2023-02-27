module Homepage.Configuration.Contact where

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics

type ContactInformation :: Type
data ContactInformation = ContactInformation
  { contactHeaderIcons :: HeaderIcons
  , contactName :: T.Text
  , contactSourceUrl :: Maybe T.Text
  , contactHomepageLabel :: Maybe T.Text
  , contactDonateInformation :: Maybe DonateInformation
  , contactEmailAddress :: Maybe T.Text
  , contactMatrix :: Maybe T.Text
  , contactLiberaChat :: Maybe T.Text
  , contactGithubUsername :: Maybe T.Text
  , contactGitlabUsername :: Maybe T.Text
  , contactHackageUsername :: Maybe T.Text
  , contactAurUsername :: Maybe T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "contact", A.CamelToKebab], A.RejectUnknownFields] ContactInformation

type HeaderIcons :: Type
data HeaderIcons = HeaderIcons
  { headerIconFeed :: Bool
  , headerIconGithub :: Bool
  , headerIconGitlab :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "headerIcon", A.CamelToKebab], A.RejectUnknownFields] HeaderIcons

type DonateInformation :: Type
data DonateInformation = DonateInformation
  { donatePaypalUrl :: Maybe T.Text
  , donateXmrAddress :: Maybe T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "donate", A.CamelToKebab], A.RejectUnknownFields] DonateInformation
