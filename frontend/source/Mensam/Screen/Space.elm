module Mensam.Screen.Space exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Api.DeskList
import Mensam.Api.Login
import Mensam.Color
import Mensam.Error
import Mensam.Font
import Mensam.Jwt
import Mensam.Time
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
    , viewDetailed :
        Maybe
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            }
    , time :
        { now : Time.Posix
        , zone : Time.Zone
        }
    , modelDate : Mensam.Time.ModelDate
    , dateSelected : Mensam.Time.Date
    , timeSelected : Mensam.Time.Time
    }


init : { id : Int, time : { now : Time.Posix, zone : Time.Zone } } -> Model
init args =
    { space = { id = args.id }
    , desks = []
    , selected = Nothing
    , viewDetailed = Nothing
    , time =
        { now = args.time.now
        , zone = args.time.zone
        }
    , modelDate =
        let
            date =
                (Mensam.Time.unTimestamp <| Mensam.Time.fromPosix args.time.zone args.time.now).date
        in
        Mensam.Time.MkModelDate
            { year = (Mensam.Time.unDate date).year
            , month = (Mensam.Time.unDate date).month
            , selected = [ (Mensam.Time.unDate date).day ]
            }
    , dateSelected = (Mensam.Time.unTimestamp <| Mensam.Time.fromPosix args.time.zone args.time.now).date
    , timeSelected =
        Mensam.Time.MkTime
            { hour = Mensam.Time.MkHour 12
            , minute = Mensam.Time.MkMinute 0
            , second = Mensam.Time.MkSecond 0
            }
    }


element : Model -> Element.Element Message
element model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 10
        , Element.Background.color (Element.rgba 0 0 0 0.1)
        , Element.Font.size 16
        , Element.Font.family [ Mensam.Font.condensed ]
        , Element.inFront <|
            case model.viewDetailed of
                Nothing ->
                    Element.none

                Just x ->
                    Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.paddingXY 30 30
                        ]
                    <|
                        Element.el
                            [ Element.Background.color Mensam.Color.bright.black
                            , Element.centerX
                            , Element.width <| Element.maximum 500 <| Element.fill
                            , Element.height <| Element.maximum 400 <| Element.fill
                            , Element.paddingXY 30 30
                            ]
                        <|
                            Element.column
                                [ Element.spacing 20
                                , Element.width Element.fill
                                ]
                                [ Element.el
                                    [ Element.Font.size 30
                                    , Element.Font.hairline
                                    ]
                                  <|
                                    Element.text "Create reservation"
                                , Element.el
                                    [ Element.width Element.fill
                                    ]
                                  <|
                                    Element.el
                                        [ Element.centerX
                                        ]
                                    <|
                                        Element.map (MessagePure << PickDate) <|
                                            Mensam.Time.elementPickDate model.modelDate
                                , Element.el
                                    [ Element.width Element.fill
                                    ]
                                  <|
                                    Element.el
                                        [ Element.centerX
                                        ]
                                    <|
                                        Element.map (\_ -> MessagePure EmptyMessage) <|
                                            Mensam.Time.elementPickTime model.timeSelected
                                , Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 10
                                    ]
                                    [ Element.Input.button
                                        [ Element.Background.color Mensam.Color.bright.yellow
                                        , Element.mouseOver [ Element.Background.color Mensam.Color.bright.green ]
                                        , Element.Font.color Mensam.Color.dark.black
                                        , Element.width Element.fill
                                        , Element.padding 10
                                        ]
                                        { onPress = Just <| MessagePure <| ViewDetailed Nothing
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Font.condensed ]
                                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                                ]
                                            <|
                                                Element.text "Abort"
                                        }
                                    , Element.Input.button
                                        [ Element.Background.color Mensam.Color.bright.yellow
                                        , Element.mouseOver [ Element.Background.color Mensam.Color.bright.green ]
                                        , Element.Font.color Mensam.Color.dark.black
                                        , Element.width Element.fill
                                        , Element.padding 10
                                        ]
                                        { onPress = Just <| MessagePure <| ViewDetailed Nothing
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Font.condensed ]
                                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                                ]
                                            <|
                                                Element.text "Submit"
                                        }
                                    ]
                                ]
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
                                , Element.Events.onClick <| MessagePure <| ViewDetailed <| Just { desk = x.desk }
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
                                            String.fromInt x.desk.id
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
                                , Element.Events.onClick <| MessagePure <| ViewDetailed <| Just { desk = x.desk }
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
                                            x.desk.name
                  }
                ]
            }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = EmptyMessage
    | SetDesks
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
    | ViewDetailed
        (Maybe
            { desk :
                { id : Int
                , name : String
                , space : Int
                }
            }
        )
    | PickDate Mensam.Time.MessageDate
    | PickTime Mensam.Time.MessageTime


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        EmptyMessage ->
            model

        SetDesks desks ->
            { model | desks = desks }

        SetSelected selection ->
            { model | selected = selection }

        ViewDetailed data ->
            { model | viewDetailed = data }

        PickDate (Mensam.Time.MessageMonth Mensam.Time.MonthNext) ->
            { model | modelDate = Mensam.Time.updateDateNextMonth model.modelDate }

        PickDate (Mensam.Time.MessageMonth Mensam.Time.MonthPrevious) ->
            { model | modelDate = Mensam.Time.updateDatePreviousMonth model.modelDate }

        PickDate (Mensam.Time.MessageDay (Mensam.Time.ClickDay day)) ->
            let
                (Mensam.Time.MkModelDate modelDate) =
                    model.modelDate
            in
            { model
                | modelDate = Mensam.Time.MkModelDate { modelDate | selected = [ day ] }
                , dateSelected =
                    Mensam.Time.MkDate
                        { year = modelDate.year
                        , month = modelDate.month
                        , day = day
                        }
            }

        PickTime (Mensam.Time.SetHour hour) ->
            { model
                | timeSelected =
                    let
                        (Mensam.Time.MkTime timeSelected) =
                            model.timeSelected
                    in
                    Mensam.Time.MkTime { timeSelected | hour = hour }
            }

        PickTime (Mensam.Time.SetMinute minute) ->
            { model
                | timeSelected =
                    let
                        (Mensam.Time.MkTime timeSelected) =
                            model.timeSelected
                    in
                    Mensam.Time.MkTime { timeSelected | minute = minute }
            }

        PickTime (Mensam.Time.SetSecond second) ->
            { model
                | timeSelected =
                    let
                        (Mensam.Time.MkTime timeSelected) =
                            model.timeSelected
                    in
                    Mensam.Time.MkTime { timeSelected | second = second }
            }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshDesks


deskList : Mensam.Jwt.Jwt -> Model -> Cmd Message
deskList jwt model =
    Mensam.Api.DeskList.request { jwt = jwt, space = model.space } <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskList.Success value) ->
                    MessagePure <| SetDesks value.desks

                Ok (Mensam.Api.DeskList.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.DeskList.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Api.Login.errorAuth error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
