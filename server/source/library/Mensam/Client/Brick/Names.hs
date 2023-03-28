module Mensam.Client.Brick.Names where

import Mensam.API.Data.Space

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
  | ClientNameSpacesNewSpaceName
  | ClientNameSpacesNewSpaceAccessibility AccessibilitySpace
  | ClientNameSpacesNewSpaceVisibility VisibilitySpace
  deriving stock (Eq, Ord, Show)
