module Mensam.Client.UI.Brick.Names where

import Mensam.API.Data.Space

import Data.Kind
import Data.Time.Zones.All qualified as T

type ClientName :: Type
data ClientName
  = ClientNameLoginUsername
  | ClientNameLoginPassword
  | ClientNameRegisterUsername
  | ClientNameRegisterPassword
  | ClientNameRegisterEmail
  | ClientNameRegisterEmailVisible
  | ClientNameSpacesList
  | ClientNameSpacesNewSpaceName
  | ClientNameSpacesNewSpaceTimezone T.TZLabel
  | ClientNameSpacesNewSpaceDiscoverability DiscoverabilitySpace
  | ClientNameDesksNewDeskName
  | ClientNameDesksReservationsViewport
  | ClientNameDesksNewReservationDesk
  | ClientNameDesksNewReservationTimeBegin
  | ClientNameDesksNewReservationTimeEnd
  | ClientNameMenuList
  | ClientNamePopupButton
  deriving stock (Eq, Ord, Show)
