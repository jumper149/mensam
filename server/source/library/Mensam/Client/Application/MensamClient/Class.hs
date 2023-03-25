module Mensam.Client.Application.MensamClient.Class where

import Mensam.API.Aeson
import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.API.Route.Api qualified as Route.Api
import Mensam.API.Route.Api.Booking qualified as Route.Api.Booking
import Mensam.API.Route.Api.OpenApi qualified as Route.Api.OpenApi
import Mensam.API.Route.Api.User qualified as Route.Api.User
import Mensam.Client.OrphanInstances

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Data.Proxy
import Servant.API
import Servant.Client
import Servant.RawM.Client ()

import Data.OpenApi qualified
import Servant.Auth qualified

type MonadMensamClient :: (Type -> Type) -> Constraint
class Monad m => MonadMensamClient m where
  mensamCall :: ClientM a -> m (Either ClientError a)

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadMensamClient m
  ) =>
  MonadMensamClient (Elevator t m)
  where
  mensamCall = lift . mensamCall

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadMensamClient (t2 m)
    ) =>
    MonadMensamClient (ComposeT t1 t2 m)

endpointOpenApi ::
  ClientM Data.OpenApi.OpenApi
endpointLogin ::
  AuthData '[Servant.Auth.BasicAuth, Servant.Auth.JWT] ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseLogin
         , WithStatus 401 ErrorBasicAuth
         , WithStatus 500 ()
         ]
    )
endpointRegister ::
  Route.Api.User.RequestRegister ->
  ClientM
    ( Union
        '[ WithStatus 201 ()
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 500 ()
         ]
    )
endpointProfile ::
  Username ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseProfile
         , WithStatus 400 ()
         , WithStatus 404 ()
         , WithStatus 500 ()
         ]
    )
endpointSpaceCreate ::
  AuthData '[Servant.Auth.JWT] ->
  Route.Api.Booking.RequestSpaceCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Booking.ResponseSpaceCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBasicAuth
         , WithStatus 500 ()
         ]
    )
endpointSpaceList ::
  AuthData '[Servant.Auth.JWT] ->
  Route.Api.Booking.RequestSpaceList ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Booking.ResponseSpaceList
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBasicAuth
         , WithStatus 500 ()
         ]
    )
endpointDeskCreate ::
  AuthData '[Servant.Auth.JWT] ->
  Route.Api.Booking.RequestDeskCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Booking.ResponseDeskCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBasicAuth
         , WithStatus 500 ()
         ]
    )
endpointDeskList ::
  AuthData '[Servant.Auth.JWT] ->
  Route.Api.Booking.RequestDeskList ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Booking.ResponseDeskList
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBasicAuth
         , WithStatus 500 ()
         ]
    )
endpointReservationCreate ::
  AuthData '[Servant.Auth.JWT] ->
  Route.Api.Booking.RequestReservationCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Booking.ResponseReservationCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBasicAuth
         , WithStatus 409 (StaticText "Desk is not available within the given time window.")
         , WithStatus 500 ()
         ]
    )
Route.Api.Routes
  { Route.Api.routeOpenApi =
    Route.Api.OpenApi.Routes
      { Route.Api.OpenApi.routeJson = endpointOpenApi
      }
  , Route.Api.routeUser =
    Route.Api.User.Routes
      { Route.Api.User.routeLogin = endpointLogin
      , Route.Api.User.routeRegister = endpointRegister
      , Route.Api.User.routeProfile = endpointProfile
      }
  , Route.Api.routeBooking =
    Route.Api.Booking.Routes
      { Route.Api.Booking.routeSpaceCreate = endpointSpaceCreate
      , Route.Api.Booking.routeSpaceList = endpointSpaceList
      , Route.Api.Booking.routeDeskCreate = endpointDeskCreate
      , Route.Api.Booking.routeDeskList = endpointDeskList
      , Route.Api.Booking.routeReservationCreate = endpointReservationCreate
      }
  } = client $ Proxy @(NamedRoutes Route.Api.Routes)