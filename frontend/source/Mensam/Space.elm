module Mensam.Space exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Mensam.Space.Role
import Mensam.Time
import Mensam.User


type SpaceView
    = MkSpaceView
        { id : Identifier
        , name : Name
        , roles :
            List
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        , timezone : Mensam.Time.TimezoneIdentifier
        , visibility : Visibility
        , owner : Mensam.User.Identifier
        , yourRole :
            Maybe
                { accessibility : Mensam.Space.Role.Accessibility
                , id : Mensam.Space.Role.Identifier
                , name : Mensam.Space.Role.Name
                , permissions : Mensam.Space.Role.Permissions
                }
        }


type Space
    = MkSpace
        { id : Identifier
        , name : Name
        , timezone : Mensam.Time.TimezoneIdentifier
        , owner : Mensam.User.Identifier
        }


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


type Visibility
    = MkVisibilityVisible
    | MkVisibilityHidden


visibilityEncode : Visibility -> Encode.Value
visibilityEncode visibility =
    Encode.string <|
        case visibility of
            MkVisibilityVisible ->
                "visible"

            MkVisibilityHidden ->
                "hidden"


visibilityDecoder : Decode.Decoder Visibility
visibilityDecoder =
    Decode.andThen
        (\string ->
            case string of
                "visible" ->
                    Decode.succeed MkVisibilityVisible

                "hidden" ->
                    Decode.succeed MkVisibilityHidden

                _ ->
                    Decode.fail <| "Trying to decode visibility, but this option is not supported: " ++ string
        )
        Decode.string
