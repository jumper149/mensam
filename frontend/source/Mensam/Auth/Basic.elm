module Mensam.Auth.Basic exposing (..)

import Json.Decode
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


decodeBody401 : Json.Decode.Decoder Error
decodeBody401 =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "username" ->
                        Json.Decode.succeed ErrorUsername

                    "password" ->
                        Json.Decode.succeed ErrorPassword

                    "indefinite" ->
                        Json.Decode.succeed ErrorIndefinite

                    _ ->
                        Json.Decode.fail <| "Trying to decode basic authentication error, but this option is not supported: " ++ string
            )
