module Mensam.API.Route.Static.Type where

import Data.Kind
import Servant.RawM qualified as RawM

type API :: Type
type API = RawM.RawM