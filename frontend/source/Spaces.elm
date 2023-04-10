module Spaces exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode


type alias Model =
    { spaces : List () }


init : Model
init =
    { spaces = [] }


view : Model -> Html.Html Message
view model =
    Html.div []
        [ Html.text (Debug.toString model)
        , Html.button
            [ Html.Attributes.id "button-spaces-refresh"
            , Html.Events.onClick <| MessageEffect RefreshSpaces
            , Html.Attributes.type_ "button"
            ]
            [ Html.text "Register" ]
        ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaces (List ())


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }


type MessageEffect
    = ReportError String
    | RefreshSpaces


deskListRequest : { jwt : String } -> Cmd Message
deskListRequest body =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ body.jwt)
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


handleDeskListResponse : Result Http.Error (List ()) -> Message
handleDeskListResponse result =
    case result of
        Ok response ->
            MessagePure <| SetSpaces response

        Err err ->
            MessageEffect <| ReportError <| Debug.toString err


decodeDeskListResponse : Json.Decode.Decoder (List ())
decodeDeskListResponse =
    Json.Decode.field "spaces" <| Json.Decode.list <| Json.Decode.succeed ()
