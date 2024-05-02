{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mensam.API.Aeson.StaticText.Internal where

import Mensam.API.Aeson.StaticText.Internal.Union qualified as Union

import Control.Applicative
import Data.Aeson qualified as A
import Data.SOP qualified as SOP
import Text.Read qualified

instance Read (SOP.NS f '[]) where
  readPrec = fail "Cannot read empty sum"
deriving stock instance (Read (f x), Read (SOP.NS f xs)) => Read (SOP.NS f (x : xs))

instance A.ToJSON (SOP.NS f '[]) where
  toJSON = \case {}
instance (A.ToJSON (f x), A.ToJSON (SOP.NS f xs), Union.Unique (x : xs)) => A.ToJSON (SOP.NS f (x : xs)) where
  toJSON = \case
    SOP.Z ix -> A.toJSON ix
    SOP.S ns -> A.toJSON ns

instance A.FromJSON (SOP.NS f '[]) where
  parseJSON _ = fail "Cannot parse empty sum"
instance (A.FromJSON (f x), A.FromJSON (SOP.NS f xs), Union.Unique (x : xs)) => A.FromJSON (SOP.NS f (x : xs)) where
  parseJSON value = (SOP.Z <$> A.parseJSON @(f x) value) <|> (SOP.S <$> A.parseJSON @(SOP.NS f xs) value)

deriving newtype instance A.ToJSON a => A.ToJSON (SOP.I a)
deriving newtype instance A.FromJSON a => A.FromJSON (SOP.I a)
