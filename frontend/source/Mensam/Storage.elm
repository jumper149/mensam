port module Mensam.Storage exposing
    ( Storage(..)
    , StorageRaw
    , parse
    , setStorage
    , unsetStorage
    )

import Iso8601
import Json.Decode
import Json.Encode
import Mensam.Auth.Bearer
import Mensam.Error
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


parse : StorageRaw -> Result Mensam.Error.Error (Maybe Storage)
parse storageRaw =
    case Json.Decode.decodeValue decoder storageRaw of
        Ok (Just storage) ->
            Ok (Just storage)

        Ok Nothing ->
            Ok Nothing

        Err error ->
            Err <|
                Mensam.Error.message "Failed to decode `localStorage`" <|
                    Mensam.Error.message (Json.Decode.errorToString error) <|
                        Mensam.Error.undefined


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
