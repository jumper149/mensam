module Mensam.User exposing (..)

import Email
import Json.Decode as Decode
import Json.Encode as Encode


type Identifier
    = MkIdentifierUnsafe Int


identifierParse : String -> Maybe Identifier
identifierParse string =
    Maybe.map MkIdentifierUnsafe <| String.toInt string


identifierToString : Identifier -> String
identifierToString (MkIdentifierUnsafe identifier) =
    String.fromInt identifier


identifierEncode : Identifier -> Encode.Value
identifierEncode (MkIdentifierUnsafe identifier) =
    Encode.int identifier


identifierDecoder : Decode.Decoder Identifier
identifierDecoder =
    Decode.map MkIdentifierUnsafe
        Decode.int


type Name
    = MkNameUnsafe String


nameValidSymbols : List Char
nameValidSymbols =
    []


nameRegexPattern : String
nameRegexPattern =
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
    "^[a-zA-Z0-9" ++ String.fromList (escapeCharsForPatternCharacterSet nameValidSymbols) ++ "]{4,32}$"


nameParse : String -> Result ErrorNameParse Name
nameParse string =
    let
        chars =
            String.toList string

        isSupportedChar char =
            Char.isAlphaNum char || List.member char nameValidSymbols

        length =
            List.length chars
    in
    if length >= 4 then
        if length <= 32 then
            if List.all isSupportedChar chars then
                Ok <| MkNameUnsafe string

            else
                Err MkErrorNameParseInvalidCharacter

        else
            Err MkErrorNameParseTooLong

    else
        Err MkErrorNameParseTooShort


type ErrorNameParse
    = MkErrorNameParseTooShort
    | MkErrorNameParseTooLong
    | MkErrorNameParseInvalidCharacter


nameToString : Name -> String
nameToString (MkNameUnsafe name) =
    name


nameEncode : Name -> Encode.Value
nameEncode =
    Encode.string << nameToString


nameDecoder : Decode.Decoder Name
nameDecoder =
    Decode.map MkNameUnsafe Decode.string


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


passwordParse : String -> Result ErrorPasswordParse Password
passwordParse string =
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


passwordToString : Password -> String
passwordToString (MkPasswordUnsafe password) =
    password


passwordEncode : Password -> Encode.Value
passwordEncode =
    Encode.string << passwordToString


type ConfirmationSecret
    = MkConfirmationSecret String


confirmationSecretEncode : ConfirmationSecret -> Encode.Value
confirmationSecretEncode (MkConfirmationSecret secret) =
    Encode.string secret


type Email
    = MkEmailUnsafe Email.Email


emailParse : String -> Maybe Email
emailParse string =
    Maybe.map MkEmailUnsafe <| Email.fromString string


emailToString : Email -> String
emailToString (MkEmailUnsafe email) =
    Email.toString email


emailEncode : Email -> Encode.Value
emailEncode (MkEmailUnsafe email) =
    Encode.string <| Email.toString email


emailDecoder : Decode.Decoder Email
emailDecoder =
    Decode.andThen
        (\string ->
            case Email.fromString string of
                Just email ->
                    Decode.succeed <| MkEmailUnsafe email

                Nothing ->
                    Decode.fail <| "Trying to decode email, but this string cannot be parsed: " ++ string
        )
        Decode.string
