module Mensam.Error exposing
    ( Error
    , group
    , http
    , isSubError
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
import Json.Decode as Decode
import Json.Encode as Encode
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


isSubError : Error -> Error -> Bool
isSubError (MkError subs) (MkError fulls) =
    let
        isSubTrees : List (Tree.Tree a) -> List (Tree.Tree a) -> Bool
        isSubTrees s f =
            if s == f then
                True

            else
                List.any (isSubTrees s) (List.map Tree.children f)
    in
    isSubTrees subs fulls



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


json : Decode.Error -> Error
json error =
    let
        recurse err =
            case err of
                Decode.Field field fieldErr ->
                    message ("Field " ++ Encode.encode 0 (Encode.string field)) <|
                        recurse fieldErr

                Decode.Index index indexErr ->
                    message ("Index " ++ String.fromInt index) <|
                        recurse indexErr

                Decode.OneOf errs ->
                    message "One of" <|
                        group <|
                            List.map recurse errs

                Decode.Failure msg val ->
                    message msg <|
                        message (Encode.encode 0 val) <|
                            undefined
    in
    message "JSON" <| recurse error
