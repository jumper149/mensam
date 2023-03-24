module Mensam.Client.Brick.Events where

import Data.Kind

type ClientEvent :: Type
data ClientEvent
  = ClientEventSwitchToScreenLogin
  | ClientEventSwitchToScreenRegister
  deriving stock (Bounded, Enum, Eq, Ord, Show)
