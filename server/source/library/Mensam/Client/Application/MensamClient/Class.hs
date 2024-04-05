module Mensam.Client.Application.MensamClient.Class where

import Mensam.API.Aeson
import Mensam.API.Data.User
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
import Servant.Auth.JWT.WithSession qualified as Servant.Auth

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
  AuthData '[Servant.Auth.BasicAuth, Servant.Auth.JWTWithSession] ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseLogin
         , WithStatus 401 ErrorBasicAuth
         , WithStatus 500 ()
         ]
    )
endpointLogout ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseLogout
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointRegister ::
  Route.Api.User.RequestRegister ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.User.ResponseRegister
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 500 ()
         ]
    )
endpointConfirm ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.User.RequestConfirm ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseConfirm
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 410 ()
         , WithStatus 500 ()
         ]
    )
endpointProfile ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.User.RequestProfile ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseProfile
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 404 ()
         , WithStatus 500 ()
         ]
    )
endpointSpaceCreate ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestSpaceCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Booking.ResponseSpaceCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointSpaceDelete ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestSpaceDelete ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Booking.ResponseSpaceDelete
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (StaticText "Insufficient permission.")
         , WithStatus 500 ()
         ]
    )
endpointSpaceView ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestSpaceView ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Booking.ResponseSpaceView
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (StaticText "Insufficient permission.")
         , WithStatus 500 ()
         ]
    )
endpointSpaceList ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestSpaceList ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Booking.ResponseSpaceList
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointDeskCreate ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestDeskCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Booking.ResponseDeskCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointDeskList ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestDeskList ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Booking.ResponseDeskList
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointReservationCreate ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestReservationCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Booking.ResponseReservationCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 409 (StaticText "Desk is not available within the given time window.")
         , WithStatus 500 ()
         ]
    )
endpointReservationCancel ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestReservationCancel ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Booking.ResponseReservationCancel
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointReservationList ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Booking.RequestReservationList ->
  ClientM
    ( Union
        [ WithStatus 200 Route.Api.Booking.ResponseReservationList
        , WithStatus 400 ErrorParseBodyJson
        , WithStatus 401 ErrorBearerAuth
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
      , Route.Api.User.routeLogout = endpointLogout
      , Route.Api.User.routeRegister = endpointRegister
      , Route.Api.User.routeConfirm = endpointConfirm
      , Route.Api.User.routeProfile = endpointProfile
      }
  , Route.Api.routeBooking =
    Route.Api.Booking.Routes
      { Route.Api.Booking.routeSpaceCreate = endpointSpaceCreate
      , Route.Api.Booking.routeSpaceDelete = endpointSpaceDelete
      , Route.Api.Booking.routeSpaceView = endpointSpaceView
      , Route.Api.Booking.routeSpaceList = endpointSpaceList
      , Route.Api.Booking.routeDeskCreate = endpointDeskCreate
      , Route.Api.Booking.routeDeskList = endpointDeskList
      , Route.Api.Booking.routeReservationCreate = endpointReservationCreate
      , Route.Api.Booking.routeReservationCancel = endpointReservationCancel
      , Route.Api.Booking.routeReservationList = endpointReservationList
      }
  } = client $ Proxy @(NamedRoutes Route.Api.Routes)
