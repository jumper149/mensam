port module Mensam.Storage exposing
    ( Storage(..)
    , StorageRaw
    , decoder
    , set
    , unset
    )

import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Auth.Bearer
import Mensam.User
import Time


type Storage
    = MkStorage
        { jwt : Mensam.Auth.Bearer.Jwt
        , expiration : Maybe Time.Posix
        , id : Mensam.User.Identifier
        }


port setStorageJson : Encode.Value -> Cmd msg


set : Storage -> Cmd msg
set =
    encode >> setStorageJson


unset : Cmd msg
unset =
    setStorageJson Encode.null


type alias StorageRaw =
    Encode.Value


decoder : Decode.Decoder (Maybe Storage)
decoder =
    Decode.nullable <|
        Decode.map3 (\jwt expiration id -> MkStorage { jwt = jwt, expiration = expiration, id = id })
            (Decode.field "jwt" Mensam.Auth.Bearer.decoder)
            (Decode.field "expiration" <| Decode.nullable Iso8601.decoder)
            (Decode.field "id" <| Mensam.User.identifierDecoder)


encode : Storage -> StorageRaw
encode (MkStorage storage) =
    Encode.object
        [ ( "jwt", Mensam.Auth.Bearer.encode storage.jwt )
        , ( "expiration"
          , case storage.expiration of
                Nothing ->
                    Encode.null

                Just expiration ->
                    Iso8601.encode expiration
          )
        , ( "id", Mensam.User.identifierEncode storage.id )
        ]
