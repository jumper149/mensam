module Mensam.User exposing (..)

import Email
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


parsePassword : String -> Result ErrorPasswordParse Password
parsePassword string =
    let
        chars =
            String.toList string

        isSupportedChar char =
            Char.isAlphaNum char || List.member char passwordValidSymbols

        length =
            List.length chars
    in
    if length >= 4 then
        if length <= 32 then
            if List.all isSupportedChar chars then
                Ok <| MkPasswordUnsafe string

            else
                Err MkErrorPasswordParseInvalidCharacter

        else
            Err MkErrorPasswordParseTooLong

    else
        Err MkErrorPasswordParseTooShort


type ErrorPasswordParse
    = MkErrorPasswordParseTooShort
    | MkErrorPasswordParseTooLong
    | MkErrorPasswordParseInvalidCharacter


passwordEncode : Password -> Encode.Value
passwordEncode (MkPasswordUnsafe password) =
    Encode.string password


type ConfirmationSecret
    = MkConfirmationSecret String


confirmationSecretEncode : ConfirmationSecret -> Encode.Value
confirmationSecretEncode (MkConfirmationSecret secret) =
    Encode.string secret


type Email
    = MkEmail Email.Email


emailFromString : String -> Maybe Email
emailFromString string =
    Maybe.map MkEmail <| Email.fromString string


emailToString : Email -> String
emailToString (MkEmail email) =
    Email.toString email


emailEncode : Email -> Encode.Value
emailEncode (MkEmail email) =
    Encode.string <| Email.toString email


emailDecoder : Decode.Decoder Email
emailDecoder =
    Decode.andThen
        (\string ->
            case Email.fromString string of
                Just email ->
                    Decode.succeed <| MkEmail email

                Nothing ->
                    Decode.fail <| "Trying to decode email, but this string cannot be parsed: " ++ string
        )
        Decode.string
