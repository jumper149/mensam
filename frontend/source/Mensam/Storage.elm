port module Mensam.Storage exposing
    ( Storage(..)
    , StorageRaw
    , decoder
    , setStorage
    , unsetStorage
    )

import Iso8601
import Json.Decode
import Json.Encode
import Mensam.Auth.Bearer
import Time


type Storage
    = MkStorage
        { jwt : Mensam.Auth.Bearer.Jwt
        , expiration : Maybe Time.Posix
        }


port setStorageJson : Json.Encode.Value -> Cmd msg


setStorage : Storage -> Cmd msg
setStorage =
    encode >> setStorageJson


unsetStorage : Cmd msg
unsetStorage =
    setStorageJson Json.Encode.null


type alias StorageRaw =
    Json.Encode.Value


decoder : Json.Decode.Decoder (Maybe Storage)
decoder =
    Json.Decode.nullable <|
        Json.Decode.map2 (\jwt expiration -> MkStorage { jwt = jwt, expiration = expiration })
            (Json.Decode.field "jwt" Mensam.Auth.Bearer.decoder)
            (Json.Decode.field "expiration" <| Json.Decode.nullable Iso8601.decoder)


encode : Storage -> StorageRaw
encode (MkStorage storage) =
    Json.Encode.object
        [ ( "jwt", Mensam.Auth.Bearer.encode storage.jwt )
        , ( "expiration"
          , case storage.expiration of
                Nothing ->
                    Json.Encode.null

                Just expiration ->
                    Iso8601.encode expiration
          )
        ]
