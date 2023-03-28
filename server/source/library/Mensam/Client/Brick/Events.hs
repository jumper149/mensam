module Mensam.Client.Brick.Events where

import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.OrphanInstances (Credentials)

import Data.Kind

type ClientEvent :: Type
data ClientEvent
  = ClientEventSwitchToScreenLogin
  | ClientEventSwitchToScreenRegister
  | ClientEventSwitchToScreenSpaces
  | ClientEventSendRequestLogin Credentials
  | ClientEventSendRequestRegister Route.User.RequestRegister
  deriving stock (Eq, Ord, Show)
