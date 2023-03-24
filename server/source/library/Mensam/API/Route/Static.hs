module Mensam.API.Route.Static where

import Data.Kind
import Servant.RawM qualified as RawM

type API :: Type
type API = RawM.RawM
