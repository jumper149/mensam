module Mensam.Auth.Bearer exposing (..)

import Http
import Json.Decode
import Json.Encode
import Mensam.Error


type Jwt
    = MkJwt String


decoder : Json.Decode.Decoder Jwt
decoder =
    Json.Decode.map MkJwt Json.Decode.string


encode : Jwt -> Json.Encode.Value
encode (MkJwt string) =
    Json.Encode.string string


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
                Mensam.Error.message "Indefinite" Mensam.Error.undefined


decodeBody401 : Json.Decode.Decoder Error
decodeBody401 =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "indefinite" ->
                        Json.Decode.succeed ErrorIndefinite

                    _ ->
                        Json.Decode.fail <| "Trying to decode authentication error, but this option is not supported: " ++ string
            )
