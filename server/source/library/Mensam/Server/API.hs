module Mensam.Server.API where

import Mensam.Server.Route.Type

import Data.Kind
import Servant.API

type API :: Type
type API = ToServantApi Routes
