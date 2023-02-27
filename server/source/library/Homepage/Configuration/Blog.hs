module Homepage.Configuration.Blog where

import Data.Aeson qualified as A
import Data.Kind
import Data.List qualified as L
import Data.Map qualified as M
import Data.Ord
import Data.Text qualified as T
import Data.Time.Calendar
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant qualified as S

type BlogId :: Type
newtype BlogId = BlogId {unBlogId :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSONKey, A.ToJSONKey)
  deriving newtype (S.FromHttpApiData, S.ToHttpApiData)

type BlogLink :: Type
data BlogLink = BlogLink
  { blogLinkDescription :: T.Text
  , blogLinkUrl :: T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "blogLink", A.CamelToKebab], A.RejectUnknownFields] BlogLink

type BlogEntry :: Type
data BlogEntry = BlogEntry
  { blogTitle :: T.Text
  , blogTimestamp :: Day
  , blogDiscussion :: [BlogLink]
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.FieldLabelModifier [A.StripPrefix "blog", A.CamelToKebab], A.RejectUnknownFields] BlogEntry

type BlogEntries :: Type
newtype BlogEntries = BlogEntries {unBlogEntries :: M.Map BlogId BlogEntry}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON [A.UnwrapUnaryRecords, A.RejectUnknownFields] BlogEntries

lookupBlog :: BlogId -> BlogEntries -> Maybe BlogEntry
lookupBlog k = M.lookup k . unBlogEntries

recentBlogEntries :: Maybe Word -> BlogEntries -> BlogEntries
recentBlogEntries count = BlogEntries . M.fromList . takeMaxLength . L.sortOn (Down . blogTimestamp . snd) . M.toList . unBlogEntries
 where
  takeMaxLength :: [a] -> [a]
  takeMaxLength = case count of
    Nothing -> id
    Just n -> take $ fromIntegral n
