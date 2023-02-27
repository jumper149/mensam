module Mensam.Server.Tab where

import Data.Kind
import Data.Text qualified as T
import GHC.Generics

type Tab :: Type
data Tab
  = TabHome
  | TabBlog
  | TabFiles
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

type TabDescription :: Type
data TabDescription = TabDescription
  { tabName :: T.Text
  , tabPath :: T.Text
  , tabPageName :: T.Text
  , tabMetaDescription :: Maybe T.Text
  }
  deriving stock (Eq, Generic, Ord, Read, Show)

describeTab :: Tab -> TabDescription
describeTab TabHome =
  TabDescription
    { tabName = "Home"
    , tabPath = ""
    , tabPageName = "Homepage"
    , tabMetaDescription = Just "I am some guy, live somewhere, like some stuff, spend some time, have some projects and you want to know about me."
    }
describeTab TabBlog =
  TabDescription
    { tabName = "Blog"
    , tabPath = "blog"
    , tabPageName = "Blog"
    , tabMetaDescription = Nothing
    }
describeTab TabFiles =
  TabDescription
    { tabName = "Files"
    , tabPath = "files"
    , tabPageName = "Files"
    , tabMetaDescription = Nothing
    }
