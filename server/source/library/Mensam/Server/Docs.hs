{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Server.Docs where

import Mensam.Server.Route.Type

import Servant
import Servant.Auth.Docs ()
import Servant.Docs
import Servant.RawM.Docs ()
import Text.Blaze.Html

documentation :: API
documentation = docs $ Proxy @(ToServantApi Routes)

instance ToSample Markup where
  toSamples _ = noSamples
