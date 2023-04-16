module Mensam.Spaces exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Mensam.Jwt


type alias Model =
    { spaces : List { id : Int, name : String } }


init : Model
init =
    { spaces = [] }


view : Model -> Html.Html Message
view model =
    Html.div []
        [ Html.button
            [ Html.Attributes.id "button-spaces-refresh"
            , Html.Events.onClick <| MessageEffect RefreshSpaces
            , Html.Attributes.type_ "button"
            ]
            [ Html.text "Refresh" ]
        , Html.table [] (List.map viewSpace model.spaces)
        ]


viewSpace : { id : Int, name : String } -> Html.Html Message
viewSpace space =
    Html.tr []
        [ Html.td [] [ Html.text (Debug.toString space.id) ]
        , Html.td [] [ Html.text (Debug.toString space.name) ]
        ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaces (List { id : Int, name : String })


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }


type MessageEffect
    = ReportError String
    | RefreshSpaces


deskListRequest : Mensam.Jwt.Jwt -> Cmd Message
deskListRequest jwt =
    Http.request
        { method = "POST"
        , headers =
            [ Mensam.Jwt.authorizationHeader jwt
            ]
        , url = "api/space/list"
        , body = Http.jsonBody <| Json.Encode.object [ ( "order", Json.Encode.list (\_ -> Json.Encode.null) [] ) ]
        , expect = expectDeskListResponse
        , timeout = Nothing
        , tracker = Nothing
        }


expectDeskListResponse : Http.Expect Message
expectDeskListResponse =
    Http.expectJson handleDeskListResponse decodeDeskListResponse


handleDeskListResponse : Result Http.Error (List { id : Int, name : String }) -> Message
handleDeskListResponse result =
    case result of
        Ok response ->
            MessagePure <| SetSpaces response

        Err err ->
            MessageEffect <| ReportError <| Debug.toString err


decodeDeskListResponse : Json.Decode.Decoder (List { id : Int, name : String })
decodeDeskListResponse =
    let
        decodeSpace =
            Json.Decode.map2 (\id name -> { id = id, name = name })
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "name" Json.Decode.string)
    in
    Json.Decode.field "spaces" <| Json.Decode.list <| decodeSpace
