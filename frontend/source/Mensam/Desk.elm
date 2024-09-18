module Mensam.Desk exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type Identifier
    = MkIdentifier Int


identifierToString : Identifier -> String
identifierToString (MkIdentifier identifier) =
    String.fromInt identifier


identifierEncode : Identifier -> Encode.Value
identifierEncode (MkIdentifier identifier) =
    Encode.int identifier


identifierDecoder : Decode.Decoder Identifier
identifierDecoder =
    Decode.map MkIdentifier
        Decode.int


type Name
    = MkName String


nameToString : Name -> String
nameToString (MkName name) =
    name


nameEncode : Name -> Encode.Value
nameEncode =
    Encode.string << nameToString


nameDecoder : Decode.Decoder Name
nameDecoder =
    Decode.map MkName Decode.string


type Location
    = MkLocation
        { position : Position
        , direction : Direction
        , size : Size
        }


locationEncode : Location -> Encode.Value
locationEncode (MkLocation location) =
    Encode.object
        [ ( "position", positionEncode location.position )
        , ( "direction", directionEncode location.direction )
        , ( "size", sizeEncode location.size )
        ]


locationDecoder : Decode.Decoder Location
locationDecoder =
    Decode.map3 (\position direction size -> MkLocation { position = position, direction = direction, size = size })
        (Decode.field "position" positionDecoder)
        (Decode.field "direction" directionDecoder)
        (Decode.field "size" sizeDecoder)


type Position
    = MkPosition
        { x : Float
        , y : Float
        }


positionEncode : Position -> Encode.Value
positionEncode (MkPosition position) =
    Encode.object
        [ ( "x", Encode.float position.x )
        , ( "y", Encode.float position.y )
        ]


positionDecoder : Decode.Decoder Position
positionDecoder =
    Decode.map2 (\x y -> MkPosition { x = x, y = y })
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)


type Direction
    = MkDirection
        { degrees : Float
        }


directionEncode : Direction -> Encode.Value
directionEncode (MkDirection direction) =
    Encode.float direction.degrees


directionDecoder : Decode.Decoder Direction
directionDecoder =
    Decode.map (\direction -> MkDirection { degrees = direction })
        Decode.float


type Size
    = MkSize
        { width : Float
        , depth : Float
        }


sizeEncode : Size -> Encode.Value
sizeEncode (MkSize size) =
    Encode.object
        [ ( "width", Encode.float size.width )
        , ( "depth", Encode.float size.depth )
        ]


sizeDecoder : Decode.Decoder Size
sizeDecoder =
    Decode.map2 (\width depth -> MkSize { width = width, depth = depth })
        (Decode.field "width" Decode.float)
        (Decode.field "depth" Decode.float)
