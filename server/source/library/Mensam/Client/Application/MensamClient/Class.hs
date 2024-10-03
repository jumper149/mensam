module Mensam.Client.Application.MensamClient.Class where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User
import Mensam.API.Route.Api qualified as Route.Api
import Mensam.API.Route.Api.OpenApi qualified as Route.Api.OpenApi
import Mensam.API.Route.Api.Reservation qualified as Route.Api.Reservation
import Mensam.API.Route.Api.Space qualified as Route.Api.Space
import Mensam.API.Route.Api.User qualified as Route.Api.User
import Mensam.Client.OrphanInstances

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Data.Proxy
import Servant.API
import Servant.API.ImageJpeg
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
         , WithStatus 409 (StaticText "Username is taken.")
         , WithStatus 500 ()
         ]
    )
endpointPasswordChange ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.User.RequestPasswordChange ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponsePasswordChange
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointPictureUpload ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  ImageJpegBytes ->
  ClientM
    ( Union
        '[ WithStatus 200 (StaticText "Uploaded profile picture.")
         , WithStatus 400 Route.Api.User.ErrorParseBodyJpeg
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointPictureDelete ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  ClientM
    ( Union
        '[ WithStatus 200 (StaticText "Deleted profile picture.")
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointPictureDownload ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  IdentifierUser ->
  ClientM ImageJpegBytes
endpointConfirmationRequest ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseConfirmationRequest
         , WithStatus 401 ErrorBearerAuth
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
endpointNotifications ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.User.RequestNotifications ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.User.ResponseNotifications
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (StaticText "Email address is not verified.")
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
  Route.Api.Space.RequestSpaceCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Space.ResponseSpaceCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointSpaceDelete ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceDelete ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceDelete
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditSpace)
         , WithStatus 404 (StaticText "Space not found.")
         , WithStatus 500 ()
         ]
    )
endpointSpaceEdit ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceEdit ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceEdit
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditSpace)
         , WithStatus 404 (StaticText "Space not found.")
         , WithStatus 500 ()
         ]
    )
endpointSpaceJoin ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceJoin ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceJoin
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (StaticTexts ["Role is inaccessible.", "Wrong role password."])
         , WithStatus 500 ()
         ]
    )
endpointSpaceLeave ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceLeave ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceLeave
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (StaticText "Owner cannot leave space.")
         , WithStatus 500 ()
         ]
    )
endpointSpaceKick ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceKick ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceKick
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditUser)
         , WithStatus 500 ()
         ]
    )
endpointSpaceUserRole ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceUserRole ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceUserRole
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditUser)
         , WithStatus 500 ()
         ]
    )
endpointSpaceView ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceView ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceView
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceViewSpace)
         , WithStatus 500 ()
         ]
    )
endpointSpaceList ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestSpaceList ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseSpaceList
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 500 ()
         ]
    )
endpointRoleCreate ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestRoleCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Space.ResponseRoleCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)
         , WithStatus 404 (StaticText "Space not found.")
         , WithStatus 500 ()
         ]
    )
endpointRoleEdit ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestRoleEdit ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseRoleEdit
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)
         , WithStatus 500 ()
         ]
    )
endpointRoleDelete ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestRoleDelete ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseRoleDelete
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)
         , WithStatus 500 ()
         ]
    )
endpointDeskCreate ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestDeskCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Space.ResponseDeskCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)
         , WithStatus 404 (StaticText "Space not found.")
         , WithStatus 500 ()
         ]
    )
endpointDeskDelete ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestDeskDelete ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseDeskDelete
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)
         , WithStatus 404 (StaticText "Desk not found.")
         , WithStatus 500 ()
         ]
    )
endpointDeskEdit ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestDeskEdit ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseDeskEdit
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)
         , WithStatus 404 (StaticText "Desk not found.")
         , WithStatus 500 ()
         ]
    )
endpointDeskList ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Space.RequestDeskList ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Space.ResponseDeskList
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceViewSpace)
         , WithStatus 500 ()
         ]
    )
endpointReservationCreate ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Reservation.RequestReservationCreate ->
  ClientM
    ( Union
        '[ WithStatus 201 Route.Api.Reservation.ResponseReservationCreate
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceCreateReservation)
         , WithStatus 409 (StaticText "Desk is not available within the given time window.")
         , WithStatus 500 ()
         ]
    )
endpointReservationCancel ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Reservation.RequestReservationCancel ->
  ClientM
    ( Union
        '[ WithStatus 200 Route.Api.Reservation.ResponseReservationCancel
         , WithStatus 400 ErrorParseBodyJson
         , WithStatus 401 ErrorBearerAuth
         , WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceCancelReservation)
         , WithStatus 409 (StaticText "Already cancelled.")
         , WithStatus 410 (StaticText "Already happened.")
         , WithStatus 500 ()
         ]
    )
endpointReservationList ::
  AuthData '[Servant.Auth.JWTWithSession] ->
  Route.Api.Reservation.RequestReservationList ->
  ClientM
    ( Union
        [ WithStatus 200 Route.Api.Reservation.ResponseReservationList
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
      , Route.Api.User.routePasswordChange = endpointPasswordChange
      , Route.Api.User.routePictureUpload = endpointPictureUpload
      , Route.Api.User.routePictureDelete = endpointPictureDelete
      , Route.Api.User.routePictureDownload = endpointPictureDownload
      , Route.Api.User.routeConfirmationRequest = endpointConfirmationRequest
      , Route.Api.User.routeConfirm = endpointConfirm
      , Route.Api.User.routeNotifications = endpointNotifications
      , Route.Api.User.routeProfile = endpointProfile
      }
  , Route.Api.routeSpace =
    Route.Api.Space.Routes
      { Route.Api.Space.routeSpaceCreate = endpointSpaceCreate
      , Route.Api.Space.routeSpaceDelete = endpointSpaceDelete
      , Route.Api.Space.routeSpaceEdit = endpointSpaceEdit
      , Route.Api.Space.routeSpaceJoin = endpointSpaceJoin
      , Route.Api.Space.routeSpaceLeave = endpointSpaceLeave
      , Route.Api.Space.routeSpaceKick = endpointSpaceKick
      , Route.Api.Space.routeSpaceUserRole = endpointSpaceUserRole
      , Route.Api.Space.routeSpaceView = endpointSpaceView
      , Route.Api.Space.routeSpaceList = endpointSpaceList
      , Route.Api.Space.routeRoleCreate = endpointRoleCreate
      , Route.Api.Space.routeRoleEdit = endpointRoleEdit
      , Route.Api.Space.routeRoleDelete = endpointRoleDelete
      , Route.Api.Space.routeDeskCreate = endpointDeskCreate
      , Route.Api.Space.routeDeskDelete = endpointDeskDelete
      , Route.Api.Space.routeDeskEdit = endpointDeskEdit
      , Route.Api.Space.routeDeskList = endpointDeskList
      }
  , Route.Api.routeReservation =
    Route.Api.Reservation.Routes
      { Route.Api.Reservation.routeReservationCreate = endpointReservationCreate
      , Route.Api.Reservation.routeReservationCancel = endpointReservationCancel
      , Route.Api.Reservation.routeReservationList = endpointReservationList
      }
  } = client $ Proxy @(NamedRoutes Route.Api.Routes)
