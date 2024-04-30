module Mensam.Screen.Spaces exposing (..)

import Element
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Mensam.Api.SpaceCreate
import Mensam.Api.SpaceList
import Mensam.Auth.Bearer
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Time


type alias Model =
    { spaces : List Mensam.Space.Space
    , selected : Maybe Int
    , create :
        Maybe
            { name : Mensam.Space.Name
            , timezone : Mensam.Time.TimezoneIdentifier
            , visible : Bool
            }
    }


type alias MainModelAccess =
    { timezone : Mensam.Time.TimezoneIdentifier
    }


init : Model
init =
    { spaces = []
    , selected = Nothing
    , create = Nothing
    }


element : Model -> Element.Element Message
element model =
    Mensam.Element.Screen.element
        { main =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                [ Element.row
                    [ Element.width Element.fill
                    , Element.height <| Element.px 70
                    , Element.padding 10
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
                , Element.indexedTable
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Background.color (Element.rgba 0 0 0 0.1)
                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                    , Element.Font.size 16
                    , Element.clipY
                    , Element.scrollbarY
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
                                        , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
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
                                        , Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
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
        , popup =
            model.create
                |> (Maybe.map <|
                        \formData ->
                            Element.column
                                [ Element.spacing 20
                                , Element.width Element.fill
                                , Element.height Element.fill
                                ]
                                [ Element.el
                                    [ Element.Font.size 30
                                    , Element.Font.hairline
                                    ]
                                  <|
                                    Element.text "Create space"
                                , Element.Input.text
                                    [ onEnter <| MessageEffect <| SubmitCreate formData
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    ]
                                    { onChange = MessagePure << EnterSpaceName << Mensam.Space.MkName
                                    , text = Mensam.Space.nameToString formData.name
                                    , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                    , label = Element.Input.labelAbove [] <| Element.text "Name"
                                    }
                                , Element.Input.text
                                    [ onEnter <| MessageEffect <| SubmitCreate formData
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    ]
                                    { onChange = MessagePure << EnterSpaceTimezone << Mensam.Time.MkTimezoneIdentifier
                                    , text = Mensam.Time.unTimezoneIdentifier formData.timezone
                                    , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Timezone"
                                    , label = Element.Input.labelAbove [] <| Element.text "Timezone (IANA)"
                                    }
                                , Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 10
                                    , Element.alignBottom
                                    ]
                                    [ Element.Input.button
                                        [ Element.Background.color Mensam.Element.Color.bright.yellow
                                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                        , Element.Font.color Mensam.Element.Color.dark.black
                                        , Element.width Element.fill
                                        , Element.padding 10
                                        ]
                                        { onPress = Just <| MessagePure <| CloseDialogToCreate
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                                ]
                                            <|
                                                Element.text "Abort"
                                        }
                                    , Element.Input.button
                                        [ Element.Background.color Mensam.Element.Color.bright.yellow
                                        , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.green ]
                                        , Element.Font.color Mensam.Element.Color.dark.black
                                        , Element.width Element.fill
                                        , Element.padding 10
                                        ]
                                        { onPress = Just <| MessageEffect <| SubmitCreate formData
                                        , label =
                                            Element.el
                                                [ Element.centerX
                                                , Element.centerY
                                                , Element.Font.family [ Mensam.Element.Font.condensed ]
                                                , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
                                                ]
                                            <|
                                                Element.text "Submit"
                                        }
                                    ]
                                ]
                   )
        , closePopup = MessagePure CloseDialogToCreate
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect


type MessagePure
    = SetSpaces (List Mensam.Space.Space)
    | SetSelected (Maybe Int)
    | OpenDialogToCreate
    | CloseDialogToCreate
    | EnterSpaceName Mensam.Space.Name
    | EnterSpaceTimezone Mensam.Time.TimezoneIdentifier


updatePure : MessagePure -> MainModelAccess -> Model -> Model
updatePure message mainModel model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }

        SetSelected selection ->
            { model | selected = selection }

        OpenDialogToCreate ->
            { model
                | create =
                    Just
                        { name = Mensam.Space.MkName ""
                        , timezone = mainModel.timezone
                        , visible = True
                        }
            }

        CloseDialogToCreate ->
            { model | create = Nothing }

        EnterSpaceName name ->
            { model
                | create =
                    model.create
                        |> (Maybe.map <|
                                \create ->
                                    { create
                                        | name = name
                                    }
                           )
            }

        EnterSpaceTimezone timezone ->
            { model
                | create =
                    model.create
                        |> (Maybe.map <|
                                \create ->
                                    { create
                                        | timezone = timezone
                                    }
                           )
            }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpaces
    | ChooseSpace Mensam.Space.Identifier
    | SubmitCreate
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.TimezoneIdentifier
        , visible : Bool
        }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


spaceList : Mensam.Auth.Bearer.Jwt -> Cmd Message
spaceList jwt =
    Mensam.Api.SpaceList.request { jwt = jwt, order = [] } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceList.Success value) ->
                    MessagePure <| SetSpaces value.spaces

                Ok (Mensam.Api.SpaceList.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting spaces failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceList.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting spaces failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting spaces failed" <|
                                Mensam.Error.http error


spaceCreate :
    { jwt : Mensam.Auth.Bearer.Jwt
    , name : Mensam.Space.Name
    , timezone : Mensam.Time.TimezoneIdentifier
    , visibility : Mensam.Space.Visibility
    }
    -> Cmd Message
spaceCreate req =
    Mensam.Api.SpaceCreate.request req <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceCreate.Success _) ->
                    MessagePure CloseDialogToCreate

                Ok (Mensam.Api.SpaceCreate.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating space failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.SpaceCreate.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating space failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Creating space failed" <|
                                Mensam.Error.http error
