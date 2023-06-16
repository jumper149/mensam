module Mensam.Auth exposing (..)

import Mensam.Auth.Bearer
import Mensam.Storage
import Mensam.User
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
            SignedIn <|
                MkAuthentication
                    { jwt = storage.jwt
                    , expiration = storage.expiration
                    , user =
                        { id = storage.id
                        , info = Nothing
                        }
                    }


type Authentication
    = MkAuthentication
        { jwt : Mensam.Auth.Bearer.Jwt
        , expiration : Maybe Time.Posix
        , user :
            { id : Mensam.User.Identifier
            , info :
                Maybe
                    { name : Mensam.User.Name
                    }
            }
        }


isExpired : Authentication -> Time.Posix -> Bool
isExpired (MkAuthentication authentication) now =
    case authentication.expiration of
        Nothing ->
            False

        Just expiration ->
            Time.posixToMillis now > Time.posixToMillis expiration
