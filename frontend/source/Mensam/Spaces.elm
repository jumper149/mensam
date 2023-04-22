module Mensam.Spaces exposing (..)

import Element
import Element.Background
import Element.Font
import Html.Attributes
import Mensam.Api.SpaceList
import Mensam.Jwt


type alias Model =
    { spaces : List { id : Int, name : String } }


init : Model
init =
    { spaces = [] }


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
            []
            { data = model.spaces
            , columns =
                let
                    cell =
                        Element.el
                            [ Element.height <| Element.px 40
                            , Element.padding 10
                            , Element.Background.color (Element.rgba 0 0 0 0.4)
                            ]
                in
                [ { header =
                        cell <|
                            Element.el
                                []
                            <|
                                Element.text "ID"
                  , width = Element.px 100
                  , view =
                        \n x ->
                            cell <|
                                Element.el
                                    [ Element.width <| Element.maximum 100 <| Element.fill ]
                                <|
                                    Element.text <|
                                        Debug.toString x.id
                  }
                , { header =
                        cell <|
                            Element.el
                                []
                            <|
                                Element.text "Name"
                  , width = Element.fill
                  , view =
                        \n x ->
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
