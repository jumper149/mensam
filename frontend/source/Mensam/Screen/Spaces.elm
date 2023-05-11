module Mensam.Screen.Spaces exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Mensam.Api.SpaceList
import Mensam.Auth.Bearer
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Error
import Mensam.Space


type alias Model =
    { spaces : List Mensam.Space.Space
    , selected : Maybe Int
    , create : Maybe ()
    }


init : Model
init =
    { spaces = []
    , selected = Nothing
    , create = Nothing
    }


element : Model -> Element.Element Message
element model =
    Element.column
        [ Element.width Element.fill
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.height <| Element.px 70
            , Element.padding 10
            ]
          <|
            Element.row
                [ Element.width Element.fill
                , Element.spacing 30
                ]
                [ Element.el
                    [ Element.alignRight
                    , Element.padding 10
                    , Element.Background.color Mensam.Element.Color.bright.yellow
                    , Element.Font.color Mensam.Element.Color.dark.black
                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                    , Element.Events.onClick <| MessagePure OpenDialogToCreate
                    ]
                  <|
                    Element.el
                        [ Element.centerX
                        , Element.centerY
                        , Element.Font.family [ Mensam.Element.Font.condensed ]
                        , Element.Font.size 17
                        , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                        ]
                    <|
                        Element.text "Create new Space"
                ]
        , Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 10
            , Element.Background.color (Element.rgba 0 0 0 0.1)
            , Element.Font.size 16
            , Element.Font.family [ Mensam.Element.Font.condensed ]
            ]
          <|
            Element.indexedTable
                [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                ]
                { data = model.spaces
                , columns =
                    let
                        cell =
                            Element.el
                                [ Element.height <| Element.px 40
                                , Element.padding 10
                                ]
                    in
                    [ { header =
                            Element.el
                                [ Element.Background.color (Element.rgba 0 0 0 0.3)
                                ]
                            <|
                                cell <|
                                    Element.el
                                        []
                                    <|
                                        Element.text "ID"
                      , width = Element.px 100
                      , view =
                            \n (Mensam.Space.MkSpace space) ->
                                Element.el
                                    [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                    , Element.Events.onClick <| MessageEffect <| ChooseSpace space.id
                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                    , let
                                        alpha =
                                            case model.selected of
                                                Nothing ->
                                                    0.2

                                                Just m ->
                                                    if m == n then
                                                        0.4

                                                    else
                                                        0.2
                                      in
                                      Element.Background.color (Element.rgba 0 0 0 alpha)
                                    ]
                                <|
                                    cell <|
                                        Element.el
                                            [ Element.width <| Element.maximum 100 <| Element.fill ]
                                        <|
                                            Element.text <|
                                                Mensam.Space.identifierToString space.id
                      }
                    , { header =
                            Element.el
                                [ Element.Background.color (Element.rgba 0 0 0 0.3)
                                ]
                            <|
                                cell <|
                                    Element.el
                                        []
                                    <|
                                        Element.text "Name"
                      , width = Element.fill
                      , view =
                            \n (Mensam.Space.MkSpace space) ->
                                Element.el
                                    [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                    , Element.Events.onClick <| MessageEffect <| ChooseSpace space.id
                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                    , let
                                        alpha =
                                            case model.selected of
                                                Nothing ->
                                                    0.2

                                                Just m ->
                                                    if m == n then
                                                        0.4

                                                    else
                                                        0.2
                                      in
                                      Element.Background.color (Element.rgba 0 0 0 alpha)
                                    ]
                                <|
                                    cell <|
                                        Element.el
                                            [ Element.width <| Element.maximum 100 <| Element.fill ]
                                        <|
                                            Element.text <|
                                                Mensam.Space.nameToString space.name
                      }
                    ]
                }
        ]


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaces (List Mensam.Space.Space)
    | SetSelected (Maybe Int)
    | OpenDialogToCreate


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }

        SetSelected selection ->
            { model | selected = selection }

        OpenDialogToCreate ->
            { model | create = Just () }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpaces
    | ChooseSpace Mensam.Space.Identifier


spaceList : Mensam.Auth.Bearer.Jwt -> Cmd Message
spaceList jwt =
    Mensam.Api.SpaceList.request { jwt = jwt, order = [] } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceList.Success value) ->
                    MessagePure <| SetSpaces value.spaces

                Ok (Mensam.Api.SpaceList.ErrorBody error) ->
                    MessageEffect <| ReportError <| Mensam.Error.message "Bad request body" <| Mensam.Error.message error <| Mensam.Error.undefined

                Ok (Mensam.Api.SpaceList.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
