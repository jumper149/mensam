module Mensam.Screen.Space.Desks exposing (..)

import Element
import Element.Background
import Element.Events
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
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Space
import Mensam.Space.Role


type alias Model =
    { spaceId : Mensam.Space.Identifier
    , spaceName : Mensam.Space.Name
    , desks :
        List
            { id : Mensam.Desk.Identifier
            , name : Mensam.Desk.Name
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
        , newName : Maybe Mensam.Desk.Name
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
                    , Element.spacing 20
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
                            , message = Just <| MessagePure OpenDialogToCreateDesk
                            , text = "New Desk"
                            }
                    , Mensam.Element.Button.button <|
                        Mensam.Element.Button.MkButton
                            { attributes = [ Element.alignRight, Element.centerY ]
                            , color = Mensam.Element.Button.Yellow
                            , message = Just <| MessageEffect ReturnToSpace
                            , text = "Go back"
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
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessagePure <| ChooseDesk desk.id
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
                                        [ Element.Events.onMouseLeave <| MessagePure <| SetSelected Nothing
                                        , Element.Events.onMouseEnter <| MessagePure <| SetSelected <| Just n
                                        , Element.Events.onClick <| MessagePure <| ChooseDesk desk.id
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
                                [ Element.Font.color Mensam.Element.Color.dark.black
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
                                        , message = Just <| MessagePure <| CloseDialogToCreateDesk
                                        , text = "Abort"
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , message = Just <| MessageEffect <| SubmitCreateDesk popupModel
                                        , text = "Create Desk"
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
                                        , message = Just <| MessagePure <| CloseDialogToDeleteDesk
                                        , text = "Abort"
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Red
                                        , message = Just <| MessageEffect <| SubmitDeleteDesk { id = popupModel.id }
                                        , text = "Delete Desk"
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
                                    , Element.Background.color Mensam.Element.Color.bright.red
                                    , Element.mouseOver [ Element.Background.color Mensam.Element.Color.bright.white ]
                                    , Element.Font.color Mensam.Element.Color.dark.black
                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                    , Element.Events.onClick <| MessagePure <| OpenDialogToDeleteDesk popupModel.id
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
                                                , message = Just <| MessagePure <| EditDeskEnterName <| Just <| Mensam.Desk.MkName ""
                                                , text = "Edit Name"
                                                }

                                    Just name ->
                                        Element.Input.text
                                            [ Element.Font.color Mensam.Element.Color.dark.black
                                            ]
                                            { onChange = MessagePure << EditDeskEnterName << Just << Mensam.Desk.MkName
                                            , text = Mensam.Desk.nameToString name
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
                                        , message = Just <| MessagePure <| CloseDialogToEditDesk
                                        , text = "Abort"
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , message =
                                            Just <|
                                                MessageEffect <|
                                                    SubmitEditDesk
                                                        { id = popupModel.id
                                                        , name = popupModel.newName
                                                        }
                                        , text = "Apply Changes"
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
            }
        )
    | SetSelected (Maybe Int)
    | ClosePopup
    | ChooseDesk Mensam.Desk.Identifier
    | OpenDialogToCreateDesk
    | CreateDeskEnterName Mensam.Desk.Name
    | CloseDialogToCreateDesk
    | OpenDialogToDeleteDesk Mensam.Desk.Identifier
    | CloseDialogToDeleteDesk
    | OpenDialogToEditDesk Mensam.Desk.Identifier
    | EditDeskEnterName (Maybe Mensam.Desk.Name)
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

        ChooseDesk deskId ->
            updatePure (OpenDialogToEditDesk deskId) model

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

        OpenDialogToEditDesk id ->
            { model | popup = Just <| PopupEditDesk { id = id, newName = Nothing } }

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

        CloseDialogToEditDesk ->
            { model | popup = Nothing }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpace
    | RefreshDesks
    | SubmitCreateDesk { name : Mensam.Desk.Name }
    | SubmitEditDesk { id : Mensam.Desk.Identifier, name : Maybe Mensam.Desk.Name }
    | SubmitDeleteDesk { id : Mensam.Desk.Identifier }
    | ReturnToSpace


spaceView : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
spaceView jwt id =
    Mensam.Api.SpaceView.request { jwt = jwt, id = id } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceView.Success value) ->
                    let
                        (Mensam.Space.MkSpaceView view) =
                            value.space
                    in
                    Messages <|
                        [ MessagePure <| SetSpaceName view.name
                        ]

                Ok (Mensam.Api.SpaceView.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

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
    { jwt : Mensam.Auth.Bearer.Jwt
    , space : Mensam.Space.Identifier
    , name : Mensam.Desk.Name
    }
    -> Cmd Message
createDesk args =
    Mensam.Api.DeskCreate.request
        { jwt = args.jwt
        , space = args.space
        , name = args.name
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
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Desk.Identifier
    }
    -> Cmd Message
deleteDesk args =
    Mensam.Api.DeskDelete.request
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
    { jwt : Mensam.Auth.Bearer.Jwt
    , id : Mensam.Desk.Identifier
    , name : Maybe Mensam.Desk.Name
    }
    -> Cmd Message
editDesk args =
    Mensam.Api.DeskEdit.request
        { jwt = args.jwt
        , id = args.id
        , name = args.name
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


listDesks : Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
listDesks jwt spaceId =
    Mensam.Api.DeskList.request { jwt = jwt, space = spaceId } <|
        \result ->
            case result of
                Ok (Mensam.Api.DeskList.Success value) ->
                    Messages <|
                        [ MessagePure <| SetDesks <| List.map (\x -> { id = x.desk.id, name = x.desk.name }) value.desks
                        ]

                Ok (Mensam.Api.DeskList.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

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
