module Homepage.Server.Route.Blog.Atom.Type where

import Data.Kind
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import GHC.Generics
import Network.HTTP.Media qualified as Media
import Servant.API

type API :: Type
type API = UVerb GET '[Atom] [WithStatus 200 AtomFeed, WithStatus 500 NoContent]

type Atom :: Type
data Atom

instance Accept Atom where
  contentType _ = "application" Media.// "atom+xml"

type AtomFeed :: Type
newtype AtomFeed = AtomFeed {unAtomFeed :: LT.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)

instance MimeRender Atom AtomFeed where
  mimeRender _ (AtomFeed text) = LT.encodeUtf8 text
