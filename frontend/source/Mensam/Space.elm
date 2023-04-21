module Mensam.Space exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes
import Mensam.Api.DeskList
import Mensam.Jwt
import Time


type alias Model =
    { space : { id : Int }
    , desks :
        List
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            , reservations :
                List
                    { desk : Int
                    , id : Int
                    , status : String
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    , user : Int
                    }
            }
    , selected : Maybe Int
    }


init : { id : Int } -> Model
init space =
    { space = space, desks = [], selected = Nothing }


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
            { data = model.desks
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
                                            Debug.toString x.desk.id
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
                                            Debug.toString x.desk.name
                  }
                ]
            }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetDesks
        (List
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            , reservations :
                List
                    { desk : Int
                    , id : Int
                    , status : String
                    , timeBegin : Time.Posix
                    , timeEnd : Time.Posix
                    , user : Int
                    }
            }
        )
    | SetSelected (Maybe Int)


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetDesks desks ->
            { model | desks = desks }

        SetSelected selection ->
            { model | selected = selection }


type MessageEffect
    = ReportError String
    | RefreshDesks


deskList : Mensam.Jwt.Jwt -> Model -> Cmd Message
deskList jwt model =
    Mensam.Api.DeskList.request { jwt = jwt, space = model.space } <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskList.Success value) ->
                    MessagePure <| SetDesks value.desks

                Ok (Mensam.Api.DeskList.ErrorBody error) ->
                    MessageEffect <| ReportError <| Debug.toString error

                Ok (Mensam.Api.DeskList.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Debug.toString error

                Err err ->
                    MessageEffect <| ReportError <| Debug.toString err
