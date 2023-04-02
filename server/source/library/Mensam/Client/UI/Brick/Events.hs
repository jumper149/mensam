module Mensam.Client.UI.Brick.Events where

import Mensam.API.Data.Space
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.OrphanInstances (Credentials)

import Data.Kind

type ClientEvent :: Type
data ClientEvent
  = ClientEventSwitchToScreenLogin
  | ClientEventSwitchToScreenRegister
  | ClientEventSwitchToScreenSpaces
  | ClientEventSwitchToScreenDesks Space
  | ClientEventSendRequestLogin Credentials
  | ClientEventSendRequestRegister Route.User.RequestRegister
  | ClientEventSendRequestCreateSpace Route.Booking.RequestSpaceCreate
  | ClientEventSendRequestCreateDesk Space Route.Booking.RequestDeskCreate
  deriving stock (Eq, Ord, Show)
