module Mensam.User exposing (..)

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


type Password
    = MkPasswordUnsafe String


passwordValidSymbols : List Char
passwordValidSymbols =
    [ ' '
    , '~'
    , '`'
    , '!'
    , '?'
    , '@'
    , '#'
    , '$'
    , '%'
    , '^'
    , '&'
    , '*'
    , '_'
    , '-'
    , '+'
    , '='
    , '<'
    , '>'
    , '('
    , ')'
    , '{'
    , '}'
    , '['
    , ']'
    , '|'
    , '\''
    , '"'
    , ','
    , '.'
    , ':'
    , ';'
    , '/'
    , '\\'
    ]


passwordRegexPattern : String
passwordRegexPattern =
    let
        escapeCharsForPatternCharacterSet : List Char -> List Char
        escapeCharsForPatternCharacterSet cs =
            case cs of
                [] ->
                    []

                '-' :: chars ->
                    '\\' :: '-' :: escapeCharsForPatternCharacterSet chars

                '[' :: chars ->
                    '\\' :: '[' :: escapeCharsForPatternCharacterSet chars

                ']' :: chars ->
                    '\\' :: ']' :: escapeCharsForPatternCharacterSet chars

                '\\' :: chars ->
                    '\\' :: '\\' :: escapeCharsForPatternCharacterSet chars

                char :: chars ->
                    char :: escapeCharsForPatternCharacterSet chars
    in
    "^[a-zA-Z0-9" ++ String.fromList (escapeCharsForPatternCharacterSet passwordValidSymbols) ++ "]{4,32}$"


parsePassword : String -> Maybe Password
parsePassword string =
    let
        chars =
            String.toList string

        isSupportedChar char =
            Char.isAlphaNum char || List.member char passwordValidSymbols

        usesSupportedChars =
            List.all isSupportedChar chars

        isCorrectLength =
            let
                length =
                    List.length chars
            in
            length >= 4 && length <= 32
    in
    if isCorrectLength && usesSupportedChars then
        Just <| MkPasswordUnsafe string

    else
        Nothing


passwordEncode : Password -> Encode.Value
passwordEncode (MkPasswordUnsafe password) =
    Encode.string password
