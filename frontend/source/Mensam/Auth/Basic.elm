module Mensam.Auth.Basic exposing (..)

import Mensam.Error


type Credentials
    = MkCredentials
        { username : String
        , password : String
        }


type Error
    = ErrorUsername
    | ErrorPassword
    | ErrorIndefinite


error : Error -> Mensam.Error.Error
error err =
    Mensam.Error.message "Basic authentication failed" <|
        case err of
            ErrorUsername ->
                Mensam.Error.message "Unknown username" Mensam.Error.undefined

            ErrorPassword ->
                Mensam.Error.message "Bad password" Mensam.Error.undefined

            ErrorIndefinite ->
                Mensam.Error.message "Indefinite" Mensam.Error.undefined
