module Mensam.Auth exposing (..)

import Mensam.Auth.Bearer
import Mensam.Storage
import Time


type Model
    = SignedOut
    | SignedIn Authentication


init : Maybe Mensam.Storage.Storage -> Model
init maybeStorage =
    case maybeStorage of
        Nothing ->
            SignedOut

        Just (Mensam.Storage.MkStorage storage) ->
            SignedIn <| MkAuthentication storage


type Authentication
    = MkAuthentication
        { jwt : Mensam.Auth.Bearer.Jwt
        , expiration : Maybe Time.Posix
        }


isExpired : Authentication -> Time.Posix -> Bool
isExpired (MkAuthentication authentication) now =
    case authentication.expiration of
        Nothing ->
            False

        Just expiration ->
            Time.posixToMillis now > Time.posixToMillis expiration
