module Mensam.Error exposing
    ( Error
    , group
    , http
    , json
    , message
    , toElement
    , toString
    , undefined
    )

import Element
import Element.Font
import Html.Attributes
import Http
import Json.Decode
import Json.Encode
import Mensam.Element.Font
import Tree


type Error
    = MkError (List (Tree.Tree String))


unError : Error -> List (Tree.Tree String)
unError (MkError error) =
    error


undefined : Error
undefined =
    MkError []


message : String -> Error -> Error
message string (MkError forest) =
    MkError <| [ Tree.tree string forest ]


group : List Error -> Error
group errors =
    MkError <| List.concat <| List.map unError errors



-- Render


toString : Error -> String
toString (MkError forest) =
    forestToString forest


forestToString : List (Tree.Tree String) -> String
forestToString forest =
    String.concat <| List.map treeToString forest


treeToString : Tree.Tree String -> String
treeToString tree =
    let
        labelString =
            Tree.label tree ++ "\n"

        childrenString =
            forestToString <| Tree.children tree
    in
    labelString ++ childrenString


toElement : Error -> Element.Element msg
toElement (MkError forest) =
    Element.el
        [ Element.Font.family [ Mensam.Element.Font.monospace ]
        , Element.Font.size 12
        , Element.Font.alignLeft
        ]
    <|
        forestToElement forest


forestToElement : List (Tree.Tree String) -> Element.Element msg
forestToElement forest =
    Element.column
        [ Element.spacing 7
        ]
    <|
        List.map treeToElement forest


treeToElement : Tree.Tree String -> Element.Element msg
treeToElement tree =
    let
        labelElement =
            Element.paragraph
                [ Element.htmlAttribute <| Html.Attributes.style "line-height" "1.05" ]
                [ Element.text <| Tree.label tree ]

        childrenElement =
            forestToElement <| Tree.children tree
    in
    Element.column
        [ Element.spacing 7
        ]
        [ labelElement
        , Element.el
            [ Element.paddingEach
                { top = 0
                , right = 0
                , bottom = 0
                , left = 4
                }
            ]
            childrenElement
        ]



-- Compatibility


http : Http.Error -> Error
http error =
    message "HTTP" <|
        case error of
            Http.BadUrl url ->
                message "Bad URL" <| message url <| undefined

            Http.Timeout ->
                message "Timeout" <| undefined

            Http.NetworkError ->
                message "Network" <| undefined

            Http.BadStatus status ->
                message "Bad Status" <| message (String.fromInt status) <| undefined

            Http.BadBody body ->
                message "Bad Body" <| message body <| undefined


json : Json.Decode.Error -> Error
json error =
    let
        recurse err =
            case err of
                Json.Decode.Field field fieldErr ->
                    message ("Field " ++ Json.Encode.encode 0 (Json.Encode.string field)) <|
                        recurse fieldErr

                Json.Decode.Index index indexErr ->
                    message ("Index " ++ String.fromInt index) <|
                        recurse indexErr

                Json.Decode.OneOf errs ->
                    message "One of" <|
                        group <|
                            List.map recurse errs

                Json.Decode.Failure msg val ->
                    message msg <|
                        message (Json.Encode.encode 0 val) <|
                            undefined
    in
    message "JSON" <| recurse error
