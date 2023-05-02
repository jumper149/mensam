module Mensam.Auth.Bearer exposing (..)

import Http
import Json.Decode
import Json.Encode


type Jwt
    = MkJwt String


decode : Json.Decode.Decoder Jwt
decode =
    Json.Decode.map MkJwt Json.Decode.string


encode : Jwt -> Json.Encode.Value
encode (MkJwt string) =
    Json.Encode.string string


authorizationHeader : Jwt -> Http.Header
authorizationHeader (MkJwt string) =
    Http.header "Authorization" ("Bearer " ++ string)
