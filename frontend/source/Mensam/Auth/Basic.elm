module Mensam.Auth.Basic exposing (..)

import Base64
import Http
import Json.Decode as Decode
import Mensam.Error


type Credentials
    = MkCredentials
        { username : String
        , password : String
        }


authorizationHeader : Credentials -> Http.Header
authorizationHeader (MkCredentials credentials) =
    Http.header
        "Authorization"
        ("Basic " ++ Base64.encode (credentials.username ++ ":" ++ credentials.password))


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
                Mensam.Error.undefined


http401BodyDecoder : Decode.Decoder Error
http401BodyDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "username" ->
                        Decode.succeed ErrorUsername

                    "password" ->
                        Decode.succeed ErrorPassword

                    "indefinite" ->
                        Decode.succeed ErrorIndefinite

                    _ ->
                        Decode.fail <| "Trying to decode basic authentication error, but this option is not supported: " ++ string
            )
