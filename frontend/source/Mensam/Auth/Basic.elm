module Mensam.Auth.Basic exposing (..)

import Base64
import Http
import Json.Decode as Decode


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
