module Mensam.Auth.Bearer exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Error


type Jwt
    = MkJwt String


decoder : Decode.Decoder Jwt
decoder =
    Decode.map MkJwt Decode.string


encode : Jwt -> Encode.Value
encode (MkJwt string) =
    Encode.string string


authorizationHeader : Jwt -> Http.Header
authorizationHeader (MkJwt string) =
    Http.header "Authorization" ("Bearer " ++ string)


type Error
    = ErrorIndefinite


error : Error -> Mensam.Error.Error
error err =
    Mensam.Error.message "Bearer authentication failed" <|
        case err of
            ErrorIndefinite ->
                Mensam.Error.undefined


http401BodyDecoder : Decode.Decoder Error
http401BodyDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "indefinite" ->
                        Decode.succeed ErrorIndefinite

                    _ ->
                        Decode.fail <| "Trying to decode authentication error, but this option is not supported: " ++ string
            )
