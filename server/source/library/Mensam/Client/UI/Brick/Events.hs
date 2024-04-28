module Mensam.Client.UI.Brick.Events where

import Mensam.API.Data.Space
import Mensam.API.Route.Api.Reservation qualified as Route.Reservation
import Mensam.API.Route.Api.Space qualified as Route.Space
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.OrphanInstances (Credentials)

import Data.Kind

type ClientEvent :: Type
data ClientEvent
  = ClientEventExit
  | ClientEventSwitchToScreenLogin
  | ClientEventSwitchToScreenRegister
  | ClientEventSwitchToScreenSpaces
  | ClientEventSwitchToScreenDesks Space
  | ClientEventSwitchToScreenMenu
  | ClientEventSendRequestLogin Credentials
  | ClientEventSendRequestLogout
  | ClientEventSendRequestRegister Route.User.RequestRegister
  | ClientEventSendRequestCreateSpace Route.Space.RequestSpaceCreate
  | ClientEventSendRequestCreateDesk Space Route.Space.RequestDeskCreate
  | ClientEventSendRequestCreateReservation Space Route.Reservation.RequestReservationCreate
  deriving stock (Eq, Ord, Show)
