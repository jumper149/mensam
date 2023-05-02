module Mensam.Auth.Basic exposing (..)

import Base64
import Http
import Json.Decode
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


http401BodyDecoder : Json.Decode.Decoder Error
http401BodyDecoder =
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
