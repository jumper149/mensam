module Mensam.Configuration.Files where

import Data.Aeson qualified as A
import Data.Kind
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Calendar
import Deriving.Aeson qualified as A
import GHC.Generics

type FileFormat :: Type
data FileFormat = FileFormat
  { fileFormatName :: T.Text
  , fileFormatExtension :: Maybe T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "fileFormat", A.CamelToKebab], A.RejectUnknownFields] FileFormat

type FileEntry :: Type
data FileEntry = FileEntry
  { fileIdentifier :: T.Text
  , fileFormats :: [FileFormat]
  , fileName :: T.Text
  , fileSection :: Maybe T.Text
  , fileTimestamp :: Day
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "file", A.CamelToKebab], A.RejectUnknownFields] FileEntry

type FileEntries :: Type
newtype FileEntries = FileEntries {unFileEntries :: S.Set FileEntry}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.UnwrapUnaryRecords, A.RejectUnknownFields] FileEntries

groupFiles :: S.Set FileEntry -> M.Map (Maybe T.Text) (S.Set FileEntry)
groupFiles entries = M.fromList $ sectionGroup <$> S.toList sections
 where
  sections = S.map fileSection entries
  sectionGroup section = (section, S.filter ((section ==) . fileSection) entries)
