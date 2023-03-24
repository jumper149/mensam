module Mensam.Client.Brick.Names where

import Data.Kind

type ClientName :: Type
data ClientName
  = ClientNameLoginUsername
  | ClientNameLoginPassword
  | ClientNameRegisterUsername
  | ClientNameRegisterPassword
  | ClientNameRegisterEmail
  | ClientNameRegisterEmailVisible
  | ClientNameSpacesList
  deriving stock (Eq, Ord, Show)
