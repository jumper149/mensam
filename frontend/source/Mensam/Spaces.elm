module Mensam.Spaces exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Mensam.Api.SpaceList
import Mensam.Jwt


type alias Model =
    { spaces : List { id : Int, name : String }
    , selected : Maybe Int
    }


init : Model
init =
    { spaces = [], selected = Nothing }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 10
        , Element.Background.color (Element.rgba 0 0 0 0.1)
        , Element.Font.size 16
        , Element.Font.family [ Element.Font.typeface "Fira Sans Condensed" ]
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
                        \n x ->
                            Element.el
                                [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
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
                                            Debug.toString x.id
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
                        \n x ->
                            Element.el
                                [ Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
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
                                            Debug.toString x.name
                  }
                ]
            }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaces (List { id : Int, name : String })
    | SetSelected (Maybe Int)


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }

        SetSelected selection ->
            { model | selected = selection }


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
