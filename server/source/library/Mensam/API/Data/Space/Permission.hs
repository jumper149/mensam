{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mensam.API.Data.Space.Permission where

import Mensam.API.Aeson

import Data.Aeson qualified as A
import Data.Kind
import Data.Singletons.TH
import Data.Text qualified as T
import Data.Typeable
import Deriving.Aeson qualified as A
import GHC.Generics

type PermissionSpace :: Type
data PermissionSpace
  = MkPermissionSpaceViewSpace
  | MkPermissionSpaceEditDesk
  | MkPermissionSpaceEditUser
  | MkPermissionSpaceEditRole
  | MkPermissionSpaceEditSpace
  | MkPermissionSpaceCreateReservation
  | MkPermissionSpaceCancelReservation
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkPermissionSpace" "") PermissionSpace

type ErrorInsufficientPermission :: PermissionSpace -> Type
data ErrorInsufficientPermission p = MkErrorInsufficientPermission
  deriving stock (Eq, Generic, Ord, Read, Show)

instance Typeable p => A.FromJSON (ErrorInsufficientPermission p) where
  parseJSON =
    A.withText errorName $ \text ->
      case T.stripPrefix "Insufficient permission: " text of
        Nothing -> fail $ "Parsing " ++ errorName ++ "failed, expected prefix \"Insufficient permission: \""
        Just suffix ->
          if T.unpack suffix == permissionName
            then pure MkErrorInsufficientPermission
            else fail $ "Parsing " ++ errorName ++ "failed, expected suffix \"" ++ permissionName ++ "\""
   where
    errorName = tyConName (typeRepTyCon (typeRep $ Proxy @(ErrorInsufficientPermission p)))
    permissionName = tyConName (typeRepTyCon (typeRep $ Proxy @p))

instance Typeable p => A.ToJSON (ErrorInsufficientPermission p) where
  toJSON MkErrorInsufficientPermission =
    A.String $ T.pack $ "Insufficient permission: " ++ permissionName
   where
    permissionName = tyConName (typeRepTyCon (typeRep $ Proxy @p))

genSingletons [''PermissionSpace]
