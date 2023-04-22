module Mensam.Spaces exposing (..)

import Html
import Html.Attributes
import Html.Events
import Mensam.Api.SpaceList
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


spaceList : Mensam.Jwt.Jwt -> Cmd Message
spaceList jwt =
    Mensam.Api.SpaceList.request { jwt = jwt, order = [] } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceList.Success value) ->
                    MessagePure <| SetSpaces value.spaces

                Ok (Mensam.Api.SpaceList.ErrorBody error) ->
                    MessageEffect <| ReportError <| Debug.toString error

                Ok (Mensam.Api.SpaceList.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Debug.toString error

                Err err ->
                    MessageEffect <| ReportError <| Debug.toString err
