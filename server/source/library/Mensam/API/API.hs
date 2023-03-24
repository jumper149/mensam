module Mensam.API.API where

import Mensam.API.Route

import Data.Kind
import Servant.API

type API :: Type
type API = ToServantApi Routes
