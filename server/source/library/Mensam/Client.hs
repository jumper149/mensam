{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Client where

import Mensam.Server.Route.Type qualified as Route

import Data.Proxy
import Servant
import Servant.Auth.Client ()
import Servant.Client
import Servant.HTML.Blaze
import Servant.RawM.Client ()
import Text.Blaze.Html

instance MimeUnrender HTML Markup where
  mimeUnrender Proxy = error "blaze-html doesn't support parsing"

routes :: Route.Routes (AsClientT ClientM)
routes = client $ Proxy @(NamedRoutes Route.Routes)
