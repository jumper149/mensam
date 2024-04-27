module Mensam.Server.Application.SeldaPool.Servant where

import Mensam.Server.Application.SeldaPool.Class

import Control.Exception
import Servant

handleSeldaException ::
  ( Exception e
  , HasStatus r
  , Applicative m
  , IsMember r responses
  ) =>
  Proxy e ->
  r ->
  SeldaResult a ->
  (SeldaResult a -> m (Union responses)) ->
  m (Union responses)
handleSeldaException (Proxy :: Proxy e) response seldaResult handleResult =
  case seldaResult of
    SeldaSuccess _ -> handleResult seldaResult
    SeldaFailure err ->
      case fromException @e err of
        Just _ -> do
          respond response
        Nothing ->
          handleResult seldaResult

handleSeldaSomeException ::
  ( HasStatus r
  , Applicative m
  , IsMember r responses
  ) =>
  r ->
  SeldaResult a ->
  (a -> m (Union responses)) ->
  m (Union responses)
handleSeldaSomeException response seldaResult handleResult =
  case seldaResult of
    SeldaSuccess x -> handleResult x
    SeldaFailure _ -> respond response
