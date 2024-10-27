module Mensam.Screen.Space.Desks exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Element.Input
import Html.Attributes
import Mensam.Api.DeskCreate
import Mensam.Api.DeskDelete
import Mensam.Api.DeskEdit
import Mensam.Api.DeskList
import Mensam.Api.SpaceView
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Element.Button
import Mensam.Element.Color exposing (Transparency(..))
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Space.Role
import Mensam.Url
import Mensam.User


type alias Model =
    { spaceId : Mensam.Space.Identifier
    , spaceName : Mensam.Space.Name
    , desks :
        List
            { id : Mensam.Desk.Identifier
            , name : Mensam.Desk.Name
            , location : Maybe Mensam.Desk.Location
            }
    , selected : Maybe Int
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupCreateDesk
        { name : Mensam.Desk.Name
        }
    | PopupDeleteDesk
        { id : Mensam.Desk.Identifier
        }
    | PopupEditDesk
        { id : Mensam.Desk.Identifier
        , oldName : Mensam.Desk.Name
        , newName : Maybe Mensam.Desk.Name
        , oldLocation : Maybe Mensam.Desk.Location
        , newLocation : Maybe (Maybe Mensam.Desk.Location)
        }


init : { id : Mensam.Space.Identifier } -> Model
init args =
    { spaceId = args.id
    , spaceName = Mensam.Space.MkName ""
    , desks = []
    , selected = Nothing
    , popup = Nothing
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
                    , Element.spacing 10
                    ]
                    [ Element.el
                        [ Element.Font.size 30
                        , Element.Font.hairline
                        , Element.alignLeft
                        , Element.centerY
                        ]
                      <|
                        Element.text "Edit Desks"
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , enabled = True
                            , label = Element.text "New Desk"
                            , message = Just <| MessagePure OpenDialogToCreateDesk
                            , size = Mensam.Element.Button.Medium
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Gray
                            , enabled = True
                            , label = Element.text "Go back"
                            , message = Just <| MessageEffect ReturnToSpace
                            , size = Mensam.Element.Button.Medium
                            }
                    ]
                , Element.indexedTable
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Background.color (Element.rgba 0 0 0 0.1)
                    , Element.Font.family [ Mensam.Element.Font.condensed ]
                    , Element.Font.size 16
                    , Element.clipY
                    , Element.scrollbarY
                    , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
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
                          , width = Element.px 40
                          , view =
                                \n desk ->
                                    Element.el
                                        [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseDesk { id = desk.id, name = desk.name, location = desk.location }
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
                                                    Mensam.Desk.identifierToString desk.id
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
                                \n desk ->
                                    Element.el
                                        [ Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetSelected Nothing
                                        , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetSelected <| Just n
                                        , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseDesk { id = desk.id, name = desk.name, location = desk.location }
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
                                                    Mensam.Desk.nameToString desk.name
                          }
                        ]
                    }
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupCreateDesk popupModel) ->
                    Just <|
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
                                Element.text "Create Desk"
                            , Element.Input.text
                                [ Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                ]
                                { onChange = MessagePure << CreateDeskEnterName << Mensam.Desk.MkName
                                , text = Mensam.Desk.nameToString popupModel.name
                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                , label = Element.Input.labelHidden "Name"
                                }
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure <| CloseDialogToCreateDesk
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , enabled = True
                                        , label = Element.text "Create Desk"
                                        , message = Just <| MessageEffect <| SubmitCreateDesk popupModel
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]

                Just (PopupDeleteDesk popupModel) ->
                    Just <|
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
                                Element.text "Delete Desk"
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure <| CloseDialogToDeleteDesk
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , enabled = True
                                        , label = Element.text "Delete Desk"
                                        , message = Just <| MessageEffect <| SubmitDeleteDesk { id = popupModel.id }
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]

                Just (PopupEditDesk popupModel) ->
                    Just <|
                        Element.column
                            [ Element.spacing 20
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]
                            [ Element.row
                                [ Element.width Element.fill
                                , Element.height <| Element.px 70
                                , Element.padding 10
                                , Element.spacing 30
                                ]
                                [ Element.column
                                    [ Element.spacing 20
                                    , Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                    [ Element.el
                                        [ Element.Font.size 30
                                        , Element.Font.hairline
                                        ]
                                      <|
                                        Element.text "Edit Desks"
                                    ]
                                , Element.el
                                    [ Element.alignRight
                                    , Element.padding 10
                                    , Element.Background.color <| Mensam.Element.Color.bright.red Mensam.Element.Color.Opaque100
                                    , Element.mouseOver [ Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100 ]
                                    , Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                    , Element.Events.Pointer.onClick <| \_ -> MessagePure <| OpenDialogToDeleteDesk popupModel.id
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
                                        Element.text "Delete Desk"
                                ]
                            , Element.el
                                [ Element.paddingXY 30 5
                                , Element.width Element.fill
                                , Element.height <| Element.px 40
                                ]
                              <|
                                case popupModel.newName of
                                    Nothing ->
                                        Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Yellow
                                                , enabled = True
                                                , label = Element.text "Edit Name"
                                                , message = Just <| MessagePure <| EditDeskEnterName <| Just popupModel.oldName
                                                , size = Mensam.Element.Button.Medium
                                                }

                                    Just name ->
                                        Element.Input.text
                                            [ Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                            ]
                                            { onChange = MessagePure << EditDeskEnterName << Just << Mensam.Desk.MkName
                                            , text = Mensam.Desk.nameToString name
                                            , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                            , label = Element.Input.labelHidden "Name"
                                            }
                            , Element.el
                                [ Element.paddingXY 30 5
                                , Element.width Element.fill
                                , Element.height <| Element.px 40
                                ]
                              <|
                                case popupModel.newLocation of
                                    Nothing ->
                                        Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Yellow
                                                , enabled = True
                                                , label = Element.text "Edit Location"
                                                , message = Just <| MessagePure <| EditDeskEnterLocation <| Just popupModel.oldLocation
                                                , size = Mensam.Element.Button.Medium
                                                }

                                    Just maybeLocation ->
                                        case maybeLocation of
                                            Nothing ->
                                                Mensam.Element.Button.button <|
                                                    Mensam.Element.Button.MkButton
                                                        { attributes = [ Element.width Element.fill ]
                                                        , color = Mensam.Element.Button.Blue
                                                        , enabled = True
                                                        , label = Element.text "Add Location"
                                                        , message =
                                                            Just <|
                                                                MessagePure <|
                                                                    EditDeskEnterLocation <|
                                                                        Just <|
                                                                            Just <|
                                                                                Mensam.Desk.MkLocation
                                                                                    { position =
                                                                                        Mensam.Desk.MkPosition
                                                                                            { x = 0
                                                                                            , y = 0
                                                                                            }
                                                                                    , direction =
                                                                                        Mensam.Desk.MkDirection
                                                                                            { degrees = 0
                                                                                            }
                                                                                    , size =
                                                                                        Mensam.Desk.MkSize
                                                                                            { width = 120
                                                                                            , depth = 90
                                                                                            }
                                                                                    }
                                                        , size = Mensam.Element.Button.Medium
                                                        }

                                            Just (Mensam.Desk.MkLocation location) ->
                                                Element.column
                                                    [ Element.width Element.fill
                                                    , Element.spacing 10
                                                    ]
                                                    [ Mensam.Element.Button.button <|
                                                        Mensam.Element.Button.MkButton
                                                            { attributes = [ Element.width Element.fill ]
                                                            , color = Mensam.Element.Button.Red
                                                            , enabled = True
                                                            , label = Element.text "Remove Location"
                                                            , message = Just <| MessagePure <| EditDeskEnterLocation <| Just <| Nothing
                                                            , size = Mensam.Element.Button.Medium
                                                            }
                                                    , Element.Input.slider
                                                        [ Element.width Element.fill
                                                        , Element.height <| Element.px 15
                                                        , Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque25
                                                        ]
                                                        { onChange =
                                                            \newValue ->
                                                                MessagePure <|
                                                                    EditDeskEnterLocation <|
                                                                        Just <|
                                                                            Just <|
                                                                                Mensam.Desk.MkLocation
                                                                                    { location
                                                                                        | position =
                                                                                            case location.position of
                                                                                                Mensam.Desk.MkPosition position ->
                                                                                                    Mensam.Desk.MkPosition { position | x = newValue }
                                                                                    }
                                                        , label =
                                                            Element.Input.labelAbove [] <|
                                                                Element.text <|
                                                                    String.concat
                                                                        [ "Position X: "
                                                                        , case location.position of
                                                                            Mensam.Desk.MkPosition position ->
                                                                                String.fromFloat (position.x / 100)
                                                                        , "m"
                                                                        ]
                                                        , min = -1000
                                                        , max = 1000
                                                        , value =
                                                            case location.position of
                                                                Mensam.Desk.MkPosition position ->
                                                                    position.x
                                                        , thumb =
                                                            Element.Input.thumb
                                                                [ Element.width <| Element.px 20
                                                                , Element.height <| Element.px 15
                                                                , Element.Background.color <| Mensam.Element.Color.bright.yellow Opaque100
                                                                ]
                                                        , step = Just 50
                                                        }
                                                    , Element.Input.slider
                                                        [ Element.width Element.fill
                                                        , Element.height <| Element.px 15
                                                        , Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque25
                                                        ]
                                                        { onChange =
                                                            \newValue ->
                                                                MessagePure <|
                                                                    EditDeskEnterLocation <|
                                                                        Just <|
                                                                            Just <|
                                                                                Mensam.Desk.MkLocation
                                                                                    { location
                                                                                        | position =
                                                                                            case location.position of
                                                                                                Mensam.Desk.MkPosition position ->
                                                                                                    Mensam.Desk.MkPosition { position | y = newValue }
                                                                                    }
                                                        , label =
                                                            Element.Input.labelAbove [] <|
                                                                Element.text <|
                                                                    String.concat
                                                                        [ "Position Y: "
                                                                        , case location.position of
                                                                            Mensam.Desk.MkPosition position ->
                                                                                String.fromFloat (position.y / 100)
                                                                        , "m"
                                                                        ]
                                                        , min = -1000
                                                        , max = 1000
                                                        , value =
                                                            case location.position of
                                                                Mensam.Desk.MkPosition position ->
                                                                    position.y
                                                        , thumb =
                                                            Element.Input.thumb
                                                                [ Element.width <| Element.px 20
                                                                , Element.height <| Element.px 15
                                                                , Element.Background.color <| Mensam.Element.Color.bright.yellow Opaque100
                                                                ]
                                                        , step = Just 50
                                                        }
                                                    , Element.Input.slider
                                                        [ Element.width Element.fill
                                                        , Element.height <| Element.px 15
                                                        , Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque25
                                                        ]
                                                        { onChange =
                                                            \newValue ->
                                                                MessagePure <|
                                                                    EditDeskEnterLocation <|
                                                                        Just <|
                                                                            Just <|
                                                                                Mensam.Desk.MkLocation
                                                                                    { location
                                                                                        | direction =
                                                                                            case location.direction of
                                                                                                Mensam.Desk.MkDirection direction ->
                                                                                                    Mensam.Desk.MkDirection { direction | degrees = newValue }
                                                                                    }
                                                        , label =
                                                            Element.Input.labelAbove [] <|
                                                                Element.text <|
                                                                    case location.direction of
                                                                        Mensam.Desk.MkDirection direction ->
                                                                            String.concat
                                                                                [ "Direction: "
                                                                                , String.fromFloat direction.degrees
                                                                                , "°"
                                                                                , " ("
                                                                                , if direction.degrees >= 0 && direction.degrees < 45 then
                                                                                    "North"

                                                                                  else if direction.degrees >= 45 && direction.degrees < 135 then
                                                                                    "East"

                                                                                  else if direction.degrees >= 135 && direction.degrees < 225 then
                                                                                    "South"

                                                                                  else if direction.degrees >= 225 && direction.degrees < 315 then
                                                                                    "West"

                                                                                  else if direction.degrees >= 315 && direction.degrees < 360 then
                                                                                    "North"

                                                                                  else
                                                                                    "???"
                                                                                , ")"
                                                                                ]
                                                        , min = 0
                                                        , max = 350
                                                        , value =
                                                            case location.direction of
                                                                Mensam.Desk.MkDirection direction ->
                                                                    direction.degrees
                                                        , thumb =
                                                            Element.Input.thumb
                                                                [ Element.width <| Element.px 20
                                                                , Element.height <| Element.px 15
                                                                , Element.Background.color <| Mensam.Element.Color.bright.yellow Opaque100
                                                                ]
                                                        , step = Just 10
                                                        }
                                                    , Element.Input.slider
                                                        [ Element.width Element.fill
                                                        , Element.height <| Element.px 15
                                                        , Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque25
                                                        ]
                                                        { onChange =
                                                            \newValue ->
                                                                MessagePure <|
                                                                    EditDeskEnterLocation <|
                                                                        Just <|
                                                                            Just <|
                                                                                Mensam.Desk.MkLocation
                                                                                    { location
                                                                                        | size =
                                                                                            case location.size of
                                                                                                Mensam.Desk.MkSize size ->
                                                                                                    Mensam.Desk.MkSize { size | width = newValue }
                                                                                    }
                                                        , label =
                                                            Element.Input.labelAbove [] <|
                                                                Element.text <|
                                                                    String.concat
                                                                        [ "Width: "
                                                                        , case location.size of
                                                                            Mensam.Desk.MkSize size ->
                                                                                String.fromFloat size.width
                                                                        , "cm"
                                                                        ]
                                                        , min = 30
                                                        , max = 600
                                                        , value =
                                                            case location.size of
                                                                Mensam.Desk.MkSize size ->
                                                                    size.width
                                                        , thumb =
                                                            Element.Input.thumb
                                                                [ Element.width <| Element.px 20
                                                                , Element.height <| Element.px 15
                                                                , Element.Background.color <| Mensam.Element.Color.bright.yellow Opaque100
                                                                ]
                                                        , step = Just 5
                                                        }
                                                    , Element.Input.slider
                                                        [ Element.width Element.fill
                                                        , Element.height <| Element.px 15
                                                        , Element.Background.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque25
                                                        ]
                                                        { onChange =
                                                            \newValue ->
                                                                MessagePure <|
                                                                    EditDeskEnterLocation <|
                                                                        Just <|
                                                                            Just <|
                                                                                Mensam.Desk.MkLocation
                                                                                    { location
                                                                                        | size =
                                                                                            case location.size of
                                                                                                Mensam.Desk.MkSize size ->
                                                                                                    Mensam.Desk.MkSize { size | depth = newValue }
                                                                                    }
                                                        , label =
                                                            Element.Input.labelAbove [] <|
                                                                Element.text <|
                                                                    String.concat
                                                                        [ "Depth: "
                                                                        , case location.size of
                                                                            Mensam.Desk.MkSize size ->
                                                                                String.fromFloat size.depth
                                                                        , "cm"
                                                                        ]
                                                        , min = 30
                                                        , max = 600
                                                        , value =
                                                            case location.size of
                                                                Mensam.Desk.MkSize size ->
                                                                    size.depth
                                                        , thumb =
                                                            Element.Input.thumb
                                                                [ Element.width <| Element.px 20
                                                                , Element.height <| Element.px 15
                                                                , Element.Background.color <| Mensam.Element.Color.bright.yellow Opaque100
                                                                ]
                                                        , step = Just 5
                                                        }
                                                    ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Yellow
                                        , enabled = True
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure <| CloseDialogToEditDesk
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , enabled = True
                                        , label = Element.text "Apply Changes"
                                        , message =
                                            Just <|
                                                MessageEffect <|
                                                    SubmitEditDesk
                                                        { id = popupModel.id
                                                        , name = popupModel.newName
                                                        , location = popupModel.newLocation
                                                        }
                                        , size = Mensam.Element.Button.Medium
                                        }
                                ]
                            ]
        , closePopup = MessagePure ClosePopup
        }


type Message
    = MessagePure MessagePure
    | MessageEffect MessageEffect
    | Messages (List Message)


type MessagePure
    = SetSpaceName Mensam.Space.Name
    | SetDesks
        (List
            { id : Mensam.Desk.Identifier
            , name : Mensam.Desk.Name
            , location : Maybe Mensam.Desk.Location
            }
        )
    | SetSelected (Maybe Int)
    | ClosePopup
    | ChooseDesk { id : Mensam.Desk.Identifier, name : Mensam.Desk.Name, location : Maybe Mensam.Desk.Location }
    | OpenDialogToCreateDesk
    | CreateDeskEnterName Mensam.Desk.Name
    | CloseDialogToCreateDesk
    | OpenDialogToDeleteDesk Mensam.Desk.Identifier
    | CloseDialogToDeleteDesk
    | OpenDialogToEditDesk { id : Mensam.Desk.Identifier, name : Mensam.Desk.Name, location : Maybe Mensam.Desk.Location }
    | EditDeskEnterName (Maybe Mensam.Desk.Name)
    | EditDeskEnterLocation (Maybe (Maybe Mensam.Desk.Location))
    | CloseDialogToEditDesk


updatePure : MessagePure -> Model -> Model
updatePure message model =
    case message of
        SetSpaceName name ->
            { model | spaceName = name }

        SetDesks desks ->
            { model | desks = desks }

        SetSelected n ->
            { model | selected = n }

        ClosePopup ->
            { model | popup = Nothing }

        ChooseDesk { id, name, location } ->
            updatePure (OpenDialogToEditDesk { id = id, name = name, location = location }) model

        OpenDialogToCreateDesk ->
            { model | popup = Just <| PopupCreateDesk { name = Mensam.Desk.MkName "" } }

        CreateDeskEnterName name ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupCreateDesk popupModel) ->
                            Just <| PopupCreateDesk { popupModel | name = name }

                        Just (PopupDeleteDesk popupModel) ->
                            Just <| PopupDeleteDesk popupModel

                        Just (PopupEditDesk popupModel) ->
                            Just <| PopupEditDesk popupModel
            }

        CloseDialogToCreateDesk ->
            { model | popup = Nothing }

        OpenDialogToDeleteDesk id ->
            { model | popup = Just <| PopupDeleteDesk { id = id } }

        CloseDialogToDeleteDesk ->
            { model | popup = Nothing }

        OpenDialogToEditDesk { id, name, location } ->
            { model | popup = Just <| PopupEditDesk { id = id, oldName = name, newName = Nothing, oldLocation = location, newLocation = Nothing } }

        EditDeskEnterName name ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupCreateDesk popupModel) ->
                            Just <| PopupCreateDesk popupModel

                        Just (PopupDeleteDesk popupModel) ->
                            Just <| PopupDeleteDesk popupModel

                        Just (PopupEditDesk popupModel) ->
                            Just <| PopupEditDesk { popupModel | newName = name }
            }

        EditDeskEnterLocation location ->
            { model
                | popup =
                    case model.popup of
                        Nothing ->
                            Nothing

                        Just (PopupCreateDesk popupModel) ->
                            Just <| PopupCreateDesk popupModel

                        Just (PopupDeleteDesk popupModel) ->
                            Just <| PopupDeleteDesk popupModel

                        Just (PopupEditDesk popupModel) ->
                            Just <| PopupEditDesk { popupModel | newLocation = location }
            }

        CloseDialogToEditDesk ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpace
    | RefreshDesks
    | SubmitCreateDesk { name : Mensam.Desk.Name }
    | SubmitEditDesk { id : Mensam.Desk.Identifier, name : Maybe Mensam.Desk.Name, location : Maybe (Maybe Mensam.Desk.Location) }
    | SubmitDeleteDesk { id : Mensam.Desk.Identifier }
    | ReturnToSpace


spaceView : Mensam.Url.BaseUrl -> { jwt : Mensam.Auth.Bearer.Jwt, yourUserId : Mensam.User.Identifier } -> Mensam.Space.Identifier -> Cmd Message
spaceView baseUrl auth id =
    Mensam.Api.SpaceView.request baseUrl { jwt = auth.jwt, yourUserId = auth.yourUserId, id = id } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success view) ->
                    Messages <|
                        [ MessagePure <| SetSpaceName view.name
                        ]

                Ok (Mensam.Api.SpaceView.Success403Restricted view) ->
                    Messages <|
                        [ MessagePure <| SetSpaceName view.name
                        , MessageEffect ReturnToSpace
                        ]

                Ok Mensam.Api.SpaceView.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Space not found" <|
                                Mensam.Error.undefined

                Ok (Mensam.Api.SpaceView.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.SpaceView.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


createDesk :
    Mensam.Url.BaseUrl
    ->
        { jwt : Mensam.Auth.Bearer.Jwt
        , space : Mensam.Space.Identifier
        , name : Mensam.Desk.Name
        , location : Maybe Mensam.Desk.Location
        }
    -> Cmd Message
createDesk baseUrl args =
    Mensam.Api.DeskCreate.request baseUrl
        { jwt = args.jwt
        , space = args.space
        , name = args.name
        , location = args.location
        }
    <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskCreate.Success _) ->
                    Messages <|
                        [ MessagePure CloseDialogToCreateDesk
                        , MessageEffect RefreshDesks
                        ]

                Ok Mensam.Api.DeskCreate.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Space not found" <|
                                Mensam.Error.undefined

                Ok (Mensam.Api.DeskCreate.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.DeskCreate.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.DeskCreate.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


deleteDesk :
    Mensam.Url.BaseUrl
    ->
        { jwt : Mensam.Auth.Bearer.Jwt
        , id : Mensam.Desk.Identifier
        }
    -> Cmd Message
deleteDesk baseUrl args =
    Mensam.Api.DeskDelete.request baseUrl
        { jwt = args.jwt
        , id = args.id
        }
    <|
        \result ->
            case result of
                Ok Mensam.Api.DeskDelete.Success ->
                    Messages <|
                        [ MessagePure CloseDialogToDeleteDesk
                        , MessageEffect RefreshDesks
                        ]

                Ok Mensam.Api.DeskDelete.ErrorDeskNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Desk not found" <|
                                Mensam.Error.undefined

                Ok (Mensam.Api.DeskDelete.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.DeskDelete.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.DeskDelete.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


editDesk :
    Mensam.Url.BaseUrl
    ->
        { jwt : Mensam.Auth.Bearer.Jwt
        , id : Mensam.Desk.Identifier
        , name : Maybe Mensam.Desk.Name
        , location : Maybe (Maybe Mensam.Desk.Location)
        }
    -> Cmd Message
editDesk baseUrl args =
    Mensam.Api.DeskEdit.request baseUrl
        { jwt = args.jwt
        , id = args.id
        , name = args.name
        , location = args.location
        }
    <|
        \result ->
            case result of
                Ok Mensam.Api.DeskEdit.Success ->
                    Messages <|
                        [ MessagePure CloseDialogToEditDesk
                        , MessageEffect RefreshDesks
                        ]

                Ok Mensam.Api.DeskEdit.ErrorDeskNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Desk not found" <|
                                Mensam.Error.undefined

                Ok (Mensam.Api.DeskEdit.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok (Mensam.Api.DeskEdit.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.DeskEdit.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error


listDesks : Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
listDesks baseUrl jwt spaceId =
    Mensam.Api.DeskList.request baseUrl { jwt = jwt, space = spaceId, timeWindow = { start = Nothing, end = Nothing } } <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskList.Success value) ->
                    Messages <|
                        [ MessagePure <| SetDesks <| List.map (\x -> { id = x.desk.id, name = x.desk.name, location = x.desk.location }) value.desks
                        ]

                Ok (Mensam.Api.DeskList.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.DeskList.ErrorSpaceNotFound ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Space not found" <|
                                Mensam.Error.undefined

                Ok (Mensam.Api.DeskList.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Bad request body" <|
                                Mensam.Error.message error <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.DeskList.ErrorAuth error) ->
                    MessageEffect <| ReportError <| Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <| ReportError <| Mensam.Error.http error
