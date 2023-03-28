module Mensam.Client.Brick.Events where

import Mensam.Client.OrphanInstances (Credentials)

import Data.Kind

type ClientEvent :: Type
data ClientEvent
  = ClientEventSwitchToScreenLogin
  | ClientEventSendRequestLogin Credentials
  | ClientEventSwitchToScreenRegister
  deriving stock (Eq, Ord, Show)
