{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mensam.API.Data.Space.Permission where

import Mensam.API.Data.Space

import Data.Singletons.TH

genSingletons [''PermissionSpace]
