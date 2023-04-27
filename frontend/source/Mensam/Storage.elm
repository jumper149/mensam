port module Mensam.Storage exposing (..)

import Iso8601
import Json.Decode
import Json.Encode
import Mensam.Jwt
import Time


type Storage
    = MkStorage
        { jwt : Mensam.Jwt.Jwt
        , expiration : Maybe Time.Posix
        }


port setStorageJson : Json.Encode.Value -> Cmd msg


setStorage : Storage -> Cmd msg
setStorage =
    encode >> setStorageJson


unsetStorage : Cmd msg
unsetStorage =
    setStorageJson Json.Encode.null


decode : Json.Decode.Decoder (Maybe Storage)
decode =
    Json.Decode.nullable <|
        Json.Decode.map2 (\jwt expiration -> MkStorage { jwt = jwt, expiration = expiration })
            (Json.Decode.field "jwt" Mensam.Jwt.decode)
            (Json.Decode.field "expiration" <| Json.Decode.nullable Iso8601.decoder)


encode : Storage -> Json.Encode.Value
encode (MkStorage storage) =
    Json.Encode.object
        [ ( "jwt", Mensam.Jwt.encode storage.jwt )
        , ( "expiration"
          , case storage.expiration of
                Nothing ->
                    Json.Encode.null

                Just expiration ->
                    Iso8601.encode expiration
          )
        ]
