module Mensam.NameOrIdentifier exposing (..)

import Json.Encode as Encode


type NameOrIdentifier name identifier
    = Name name
    | Identifier identifier


encode : (name -> Encode.Value) -> (identifier -> Encode.Value) -> NameOrIdentifier name identifier -> Encode.Value
encode encodeName encodeIdentifier nameOrIdentifier =
    case nameOrIdentifier of
        Name name ->
            Encode.object
                [ ( "tag"
                  , Encode.string "name"
                  )
                , ( "value"
                  , encodeName name
                  )
                ]

        Identifier identifier ->
            Encode.object
                [ ( "tag"
                  , Encode.string "identifier"
                  )
                , ( "value"
                  , encodeIdentifier identifier
                  )
                ]
