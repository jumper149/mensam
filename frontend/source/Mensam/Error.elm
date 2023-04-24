module Mensam.Error exposing (Error, group, http, message, toString, undefined)

import Http
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



-- String conversion


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
