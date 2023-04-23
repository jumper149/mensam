module Mensam.API.Route.Frontend where

import Data.Kind
import Data.Text qualified as T
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze.Html5

type API :: Type
type API = CaptureAll "segments" T.Text :> Get '[HTML] Html
