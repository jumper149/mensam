module Mensam.API.Route.Frontend where

import Data.Kind
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Data.Text qualified as T

type API :: Type
type API = CaptureAll "segments" T.Text :> Get '[HTML] Html
