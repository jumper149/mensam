module Mensam.Screen.Dashboard exposing (..)

import Dict
import Element
import Element.Background
import Element.Border
import Element.Events.Pointer
import Element.Font
import Element.Input
import Html.Attributes
import List.Extra
import Mensam.Api.Profile
import Mensam.Api.ReservationCancel
import Mensam.Api.ReservationList
import Mensam.Api.SpaceCreate
import Mensam.Api.SpaceList
import Mensam.Api.SpacePictureDownload
import Mensam.Auth.Bearer
import Mensam.Desk
import Mensam.Element.Button
import Mensam.Element.Color
import Mensam.Element.Font
import Mensam.Element.Screen
import Mensam.Error
import Mensam.Reservation
import Mensam.Space
import Mensam.Space.Role
import Mensam.Time
import Mensam.Url
import Mensam.User
import Mensam.Widget.Timezone
import Time


type alias Model =
    { spaces :
        List
            { id : Mensam.Space.Identifier
            , name : Mensam.Space.Name
            , timezone : Mensam.Time.Timezone
            , owner : Mensam.User.Identifier
            , users : Int
            , desks : Int
            }
    , hoveringSpace : Maybe Int
    , owners : Dict.Dict Int Mensam.User.Name
    , pictureUrls : Dict.Dict Int String
    , reservations :
        List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , location : Maybe Mensam.Desk.Location
                }
            , reservation :
                { id : Mensam.Reservation.Identifier
                , status : Mensam.Reservation.Status
                , timeBegin : Time.Posix
                , timeEnd : Time.Posix
                }
            , space :
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                , timezone : Mensam.Time.Timezone
                , owner : Mensam.User.Identifier
                }
            , user :
                { id : Mensam.User.Identifier
                }
            }
    , hoveringReservation : Maybe Int
    , timezone : Mensam.Time.Timezone
    , modelDateBegin : Mensam.Time.Date
    , modelDateEnd : Mensam.Time.Date
    , enableButtonForInitWizard : Bool
    , popup : Maybe PopupModel
    }


type PopupModel
    = PopupViewReservation Mensam.Reservation.Identifier
    | PopupCreateFirstSpace
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visible : Bool
        , timezonePicker : Maybe Mensam.Widget.Timezone.Model
        }


type alias MainModelAccess =
    { timezone : Mensam.Time.Timezone
    , username : Maybe Mensam.User.Name
    }


init : { time : { now : Time.Posix, zone : Mensam.Time.Timezone } } -> Model
init value =
    let
        initialDateModel =
            let
                timestampNow =
                    Mensam.Time.fromPosix value.time.zone value.time.now

                timestampOneWeekFromNow =
                    let
                        weekInMillis =
                            1000 * 60 * 60 * 24 * 7
                    in
                    Mensam.Time.fromPosix value.time.zone <| Time.millisToPosix <| Time.posixToMillis value.time.now + weekInMillis
            in
            { begin = (Mensam.Time.unTimestamp timestampNow).date
            , end = (Mensam.Time.unTimestamp timestampOneWeekFromNow).date
            }
    in
    { spaces = []
    , hoveringSpace = Nothing
    , owners = Dict.empty
    , pictureUrls = Dict.empty
    , reservations = []
    , hoveringReservation = Nothing
    , timezone = value.time.zone
    , modelDateBegin = initialDateModel.begin
    , modelDateEnd = initialDateModel.end
    , enableButtonForInitWizard = False
    , popup = Nothing
    }


element : Mensam.Url.BaseUrl -> Model -> Element.Element Message
element baseUrl model =
    Mensam.Element.Screen.element
        { main =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 10
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 1
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.Font.size 22
                            , Element.Font.hairline
                            , Element.alignBottom
                            , Element.alignLeft
                            ]
                          <|
                            Element.text "Your Spaces"
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight ]
                                , enabled = True
                                , color = Mensam.Element.Button.Yellow
                                , label = Element.text "Browse"
                                , message = Just <| MessageEffect OpenPageToBrowseSpaces
                                , size = Mensam.Element.Button.Small
                                }
                        ]
                    , Element.indexedTable
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.Background.color (Element.rgba 0 0 0 0.1)
                        , Element.Border.widthEach
                            { bottom = 1
                            , left = 0
                            , right = 0
                            , top = 1
                            }
                        , Element.Border.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                        , Element.Font.family [ Mensam.Element.Font.condensed ]
                        , Element.Font.size 16
                        , Element.clipY
                        , Element.scrollbarY
                        , Element.htmlAttribute <| Html.Attributes.style "contain" "size"
                        , Element.inFront <|
                            if model.enableButtonForInitWizard then
                                Element.column
                                    [ Element.spacing 10
                                    , Element.centerY
                                    , Element.width Element.fill
                                    ]
                                    [ Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.centerX ]
                                            , enabled = True
                                            , color = Mensam.Element.Button.Blue
                                            , label = Element.text "Create new Space"
                                            , message = Just <| MessagePure OpenDialogToCreate
                                            , size = Mensam.Element.Button.Medium
                                            }
                                    , Element.paragraph
                                        [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                        , Element.Font.center
                                        , Element.width <| Element.px 300
                                        , Element.centerX
                                        , Element.paddingXY 40 0
                                        , Element.htmlAttribute <| Html.Attributes.style "cursor" "default"
                                        , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                        ]
                                        [ Element.text "or join an existing space via a URL, QR-Code or by browsing public spaces"
                                        ]
                                    ]

                            else
                                Element.none
                        ]
                        { data = model.spaces
                        , columns =
                            [ { header = Element.none
                              , width = Element.fill
                              , view =
                                    \n space ->
                                        Element.el
                                            [ Element.height <| Element.px 80
                                            , Element.width Element.fill
                                            , Element.padding 10
                                            , Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetHoveringSpace <| Just n
                                            , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetHoveringSpace Nothing
                                            , Element.Events.Pointer.onClick <| \_ -> MessageEffect <| ChooseSpace space.id
                                            , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                            , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                            , let
                                                alpha =
                                                    case model.hoveringSpace of
                                                        Nothing ->
                                                            0.2

                                                        Just m ->
                                                            if m == n then
                                                                0.4

                                                            else
                                                                0.2
                                              in
                                              Element.Background.color (Element.rgba 0 0 0 alpha)
                                            , Element.Border.widthEach
                                                { bottom = 0
                                                , left = 0
                                                , right = 0
                                                , top =
                                                    if n == 0 then
                                                        0

                                                    else
                                                        1
                                                }
                                            , Element.Border.color <| Mensam.Element.Color.bright.white Mensam.Element.Color.Opaque100
                                            , Element.inFront <|
                                                Element.el
                                                    [ Element.width <| Element.px 80
                                                    , Element.height <| Element.px 80
                                                    , Element.alignRight
                                                    , Element.centerY
                                                    , Element.Background.color <| Mensam.Element.Color.dark.cyan Mensam.Element.Color.Opaque25
                                                    ]
                                                <|
                                                    Element.image
                                                        [ Element.width <| Element.px 60
                                                        , Element.height <| Element.px 60
                                                        , Element.centerX
                                                        , Element.centerY
                                                        , Element.clip
                                                        ]
                                                        { src =
                                                            case
                                                                Dict.get
                                                                    (case space.id of
                                                                        Mensam.Space.MkIdentifier id ->
                                                                            id
                                                                    )
                                                                    model.pictureUrls
                                                            of
                                                                Nothing ->
                                                                    Mensam.Url.absolute baseUrl
                                                                        [ "static"
                                                                        , "default-space-picture.jpeg"
                                                                        ]
                                                                        []

                                                                Just url ->
                                                                    url
                                                        , description = "Profile picture."
                                                        }
                                            ]
                                        <|
                                            Element.row
                                                [ Element.width Element.fill
                                                , Element.height Element.fill
                                                , Element.spacing 10
                                                ]
                                                [ Element.column
                                                    [ Element.width <| Element.px 150
                                                    , Element.alignLeft
                                                    , Element.spacing 1
                                                    , Element.clip
                                                    ]
                                                    [ Element.paragraph
                                                        [ Element.height <| Element.px 40
                                                        , Element.alignTop
                                                        , Element.clipY
                                                        , Element.Font.size 15
                                                        , Element.Font.semiBold
                                                        ]
                                                        [ Element.text <|
                                                            Mensam.Space.nameToString space.name
                                                        ]
                                                    , Element.el
                                                        [ Element.alignBottom
                                                        , Element.Font.size 12
                                                        , Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                                        , Element.paddingEach
                                                            { top = 0
                                                            , right = 0
                                                            , bottom = 0
                                                            , left = 5
                                                            }
                                                        ]
                                                      <|
                                                        Element.text <|
                                                            Mensam.Time.timezoneToString space.timezone
                                                    ]
                                                , Element.column
                                                    [ Element.width Element.fill
                                                    , Element.height Element.fill
                                                    , Element.alignLeft
                                                    , Element.clip
                                                    ]
                                                    [ Element.column
                                                        [ Element.alignTop
                                                        , Element.spacing 1
                                                        ]
                                                        [ Element.el
                                                            [ Element.alignTop
                                                            , Element.Font.size 10
                                                            ]
                                                          <|
                                                            Element.text "Owner"
                                                        , Element.el
                                                            [ Element.alignTop
                                                            , Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                                            ]
                                                          <|
                                                            Element.text <|
                                                                case
                                                                    Dict.get
                                                                        (case space.id of
                                                                            Mensam.Space.MkIdentifier id ->
                                                                                id
                                                                        )
                                                                        model.owners
                                                                of
                                                                    Nothing ->
                                                                        ""

                                                                    Just name ->
                                                                        Mensam.User.nameToString name
                                                        ]
                                                    , Element.el
                                                        [ Element.alignBottom
                                                        , Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                                        ]
                                                      <|
                                                        Element.text <|
                                                            String.concat
                                                                [ String.fromInt space.users
                                                                    ++ (if space.users == 1 then
                                                                            " User"

                                                                        else
                                                                            " Users"
                                                                       )
                                                                , ", "
                                                                , String.fromInt space.desks
                                                                    ++ (if space.desks == 1 then
                                                                            " Desk"

                                                                        else
                                                                            " Desks"
                                                                       )
                                                                ]
                                                    ]
                                                ]
                              }
                            ]
                        }
                    ]
                , Element.column
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 2
                    ]
                    [ Element.row
                        [ Element.width Element.fill
                        , Element.height <| Element.px 45
                        , Element.padding 10
                        , Element.spacing 30
                        ]
                        [ Element.el
                            [ Element.Font.size 22
                            , Element.Font.hairline
                            , Element.alignBottom
                            , Element.alignLeft
                            ]
                          <|
                            Element.text "Upcoming Reservations"
                        , Mensam.Element.Button.button <|
                            Mensam.Element.Button.MkButton
                                { attributes = [ Element.alignRight ]
                                , color = Mensam.Element.Button.Yellow
                                , enabled = True
                                , label = Element.text "More"
                                , message = Just <| MessageEffect OpenPageToViewReservations
                                , size = Mensam.Element.Button.Small
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
                        { data = model.reservations
                        , columns =
                            let
                                cell =
                                    Element.el
                                        [ Element.height <| Element.px 50
                                        , Element.padding 10
                                        ]
                            in
                            [ { header = Element.none
                              , width = Element.px 160
                              , view =
                                    \n entry ->
                                        Element.el
                                            (case entry.reservation.status of
                                                Mensam.Reservation.MkStatusPlanned ->
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetHoveringReservation <| Just n
                                                    , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetHoveringReservation Nothing
                                                    , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseReservation entry.reservation.id
                                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                    , let
                                                        alpha =
                                                            case model.hoveringReservation of
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

                                                Mensam.Reservation.MkStatusCancelled ->
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetHoveringReservation <| Just n
                                                    , Element.Background.color (Element.rgba 1 0 0 0.2)
                                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                    ]
                                            )
                                        <|
                                            cell <|
                                                Element.column
                                                    [ Element.width <| Element.maximum 160 <| Element.fill ]
                                                    [ Element.row [ Element.alignRight, Element.spacing 3 ]
                                                        [ Element.el
                                                            [ Element.Font.size 10
                                                            , Element.alignBottom
                                                            , Element.padding 1
                                                            ]
                                                          <|
                                                            Element.text "from"
                                                        , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                            Element.text <|
                                                                Mensam.Time.timestampToString <|
                                                                    Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeBegin
                                                        ]
                                                    , Element.row [ Element.alignRight, Element.spacing 3 ]
                                                        [ Element.el
                                                            [ Element.Font.size 10
                                                            , Element.alignBottom
                                                            , Element.padding 1
                                                            ]
                                                          <|
                                                            Element.text "to"
                                                        , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                            Element.text <|
                                                                Mensam.Time.timestampToString <|
                                                                    Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeEnd
                                                        ]
                                                    ]
                              }
                            , { header = Element.none
                              , width = Element.fill
                              , view =
                                    \n entry ->
                                        Element.el
                                            (case entry.reservation.status of
                                                Mensam.Reservation.MkStatusPlanned ->
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetHoveringReservation <| Just n
                                                    , Element.Events.Pointer.onLeave <| \_ -> MessagePure <| SetHoveringReservation Nothing
                                                    , Element.Events.Pointer.onClick <| \_ -> MessagePure <| ChooseReservation entry.reservation.id
                                                    , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                    , let
                                                        alpha =
                                                            case model.hoveringReservation of
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

                                                Mensam.Reservation.MkStatusCancelled ->
                                                    [ Element.Events.Pointer.onEnter <| \_ -> MessagePure <| SetHoveringReservation <| Just n
                                                    , Element.Background.color (Element.rgba 1 0 0 0.2)
                                                    , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                                                    ]
                                            )
                                        <|
                                            cell <|
                                                Element.column
                                                    [ Element.width <| Element.maximum 150 <| Element.fill ]
                                                    [ Element.row [ Element.alignLeft, Element.spacing 3 ]
                                                        [ Element.el
                                                            [ Element.Font.size 10
                                                            , Element.alignBottom
                                                            , Element.padding 1
                                                            ]
                                                          <|
                                                            Element.text "Space"
                                                        , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                            Element.text <|
                                                                Mensam.Space.nameToString entry.space.name
                                                        ]
                                                    , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                                        [ Element.el
                                                            [ Element.Font.size 10
                                                            , Element.alignBottom
                                                            , Element.padding 1
                                                            ]
                                                          <|
                                                            Element.text "Desk"
                                                        , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300 ] <|
                                                            Element.text <|
                                                                Mensam.Desk.nameToString entry.desk.name
                                                        ]
                                                    ]
                              }
                            ]
                        }
                    ]
                ]
        , popup =
            case model.popup of
                Nothing ->
                    Nothing

                Just (PopupViewReservation reservationId) ->
                    case List.Extra.find (\entry -> entry.reservation.id == reservationId) <| model.reservations of
                        -- This case should never occur. The user cannot click on a reservation that doesn't exist.
                        Nothing ->
                            Nothing

                        Just entry ->
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
                                        Element.text "Reservation"
                                    , Element.column
                                        [ Element.centerX
                                        , Element.spacing 10
                                        ]
                                        [ Element.row [ Element.alignRight, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "from"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Time.timestampToString <|
                                                        Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeBegin
                                            ]
                                        , Element.row [ Element.alignRight, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "to"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Time.timestampToString <|
                                                        Mensam.Time.fromPosix entry.space.timezone entry.reservation.timeEnd
                                            ]
                                        , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Timezone"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Time.timezoneToString <|
                                                        entry.space.timezone
                                            ]
                                        , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Status"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Reservation.statusToString <|
                                                        entry.reservation.status
                                            ]
                                        ]
                                    , Element.column
                                        [ Element.centerX
                                        , Element.spacing 10
                                        ]
                                        [ Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Space"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Space.nameToString entry.space.name
                                            ]
                                        , Element.row [ Element.alignLeft, Element.spacing 3 ]
                                            [ Element.el
                                                [ Element.Font.size 12
                                                , Element.alignBottom
                                                , Element.padding 1
                                                ]
                                              <|
                                                Element.text "Desk"
                                            , Element.el [ Mensam.Element.Font.fontWeight Mensam.Element.Font.Regular400 ] <|
                                                Element.text <|
                                                    Mensam.Desk.nameToString <|
                                                        entry.desk.name
                                            ]
                                        ]
                                    , Element.paragraph
                                        [ Element.alignBottom
                                        , Element.centerX
                                        , Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                        ]
                                        [ Element.text "Do you want to cancel the reservation?"
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
                                                , label = Element.text "Go back"
                                                , message = Just <| MessagePure <| ClosePopup
                                                , size = Mensam.Element.Button.Medium
                                                }
                                        , Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Red
                                                , enabled = True
                                                , label = Element.text "Cancel Reservation"
                                                , message = Just <| MessageEffect <| CancelReservation reservationId
                                                , size = Mensam.Element.Button.Medium
                                                }
                                        ]
                                    ]

                Just (PopupCreateFirstSpace popupModel) ->
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
                                Element.text "Create space"
                            , Element.Input.text
                                [ Element.Font.color <| Mensam.Element.Color.dark.black Mensam.Element.Color.Opaque100
                                ]
                                { onChange = MessagePure << DialogToCreateEnterSpaceName << Mensam.Space.MkName
                                , text = Mensam.Space.nameToString popupModel.name
                                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Name"
                                , label = Element.Input.labelAbove [] <| Element.text "Name"
                                }
                            , Element.column
                                [ Element.width Element.fill
                                ]
                                [ Element.row [ Element.width Element.fill ]
                                    [ Element.el [ Element.alignLeft, Element.alignBottom, Element.paddingXY 0 5 ] <| Element.text "Timezone"
                                    , Mensam.Element.Button.button <|
                                        Mensam.Element.Button.MkButton
                                            { attributes = [ Element.alignRight, Element.alignBottom ]
                                            , color = Mensam.Element.Button.Transparent
                                            , enabled = True
                                            , label = Element.text "Reset"
                                            , message =
                                                Just <|
                                                    Messages
                                                        [ MessagePure DialogToCreateTimezonePickerClose
                                                        , MessagePure DialogToCreateSetTimezoneToLocalTimezone
                                                        ]
                                            , size = Mensam.Element.Button.Small
                                            }
                                    ]
                                , case popupModel.timezonePicker of
                                    Nothing ->
                                        Mensam.Element.Button.button <|
                                            Mensam.Element.Button.MkButton
                                                { attributes = [ Element.width Element.fill ]
                                                , color = Mensam.Element.Button.Yellow
                                                , enabled = True
                                                , label =
                                                    Element.el
                                                        [ Element.paddingXY 0 4
                                                        ]
                                                    <|
                                                        Element.text <|
                                                            Mensam.Time.timezoneToString popupModel.timezone
                                                , message = Just <| MessagePure <| DialogToCreateTimezonePickerOpen
                                                , size = Mensam.Element.Button.Small
                                                }

                                    Just timezonePickerModel ->
                                        Element.el
                                            [ Element.width <| Element.px 250
                                            ]
                                        <|
                                            Element.map (MessagePure << DialogToCreateTimezonePickerMessage) <|
                                                Mensam.Widget.Timezone.elementPickTimezone timezonePickerModel
                                ]
                            , Element.paragraph
                                [ Element.alignBottom
                                , Mensam.Element.Font.fontWeight Mensam.Element.Font.Light300
                                , Element.Font.justify
                                ]
                                [ Element.text "You will be able to add desks later on. "
                                , Element.text "Create this new space now?"
                                ]
                            , Element.row
                                [ Element.width Element.fill
                                , Element.spacing 10
                                , Element.alignBottom
                                ]
                                [ Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Gray
                                        , enabled = True
                                        , label = Element.text "Abort"
                                        , message = Just <| MessagePure <| CloseDialogToCreate
                                        , size = Mensam.Element.Button.Medium
                                        }
                                , Mensam.Element.Button.button <|
                                    Mensam.Element.Button.MkButton
                                        { attributes = [ Element.width Element.fill ]
                                        , color = Mensam.Element.Button.Blue
                                        , enabled = True
                                        , label = Element.text "Submit"
                                        , message =
                                            Just <|
                                                MessageEffect <|
                                                    SubmitCreateFirstSpace
                                                        { name = popupModel.name
                                                        , timezone = popupModel.timezone
                                                        , visible = popupModel.visible
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
    = SetSpaces
        (List
            { id : Mensam.Space.Identifier
            , name : Mensam.Space.Name
            , timezone : Mensam.Time.Timezone
            , owner : Mensam.User.Identifier
            , users : Int
            , desks : Int
            }
        )
    | SetHoveringSpace (Maybe Int)
    | SetOwnerName
        { space : Mensam.Space.Identifier
        , owner : Mensam.User.Name
        }
    | SetSpacePictureUrl
        { space : Mensam.Space.Identifier
        , url : String
        }
    | SetReservations
        (List
            { desk :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , location : Maybe Mensam.Desk.Location
                }
            , reservation :
                { id : Mensam.Reservation.Identifier
                , status : Mensam.Reservation.Status
                , timeBegin : Time.Posix
                , timeEnd : Time.Posix
                }
            , space :
                { id : Mensam.Space.Identifier
                , name : Mensam.Space.Name
                , timezone : Mensam.Time.Timezone
                , owner : Mensam.User.Identifier
                }
            , user :
                { id : Mensam.User.Identifier
                }
            }
        )
    | SetHoveringReservation (Maybe Int)
    | ChooseReservation Mensam.Reservation.Identifier
    | ClosePopup
    | EnableButtonForInitWizard Bool
    | OpenDialogToCreate
    | CloseDialogToCreate
    | DialogToCreateEnterSpaceName Mensam.Space.Name
    | DialogToCreateSetTimezoneToLocalTimezone
    | DialogToCreateTimezonePickerOpen
    | DialogToCreateTimezonePickerClose
    | DialogToCreateTimezonePickerMessage Mensam.Widget.Timezone.Message


updatePure : MessagePure -> MainModelAccess -> Model -> Model
updatePure message mainModel model =
    case message of
        SetSpaces spaces ->
            { model | spaces = spaces }

        SetHoveringSpace maybeN ->
            { model | hoveringSpace = maybeN }

        SetOwnerName { space, owner } ->
            { model
                | owners =
                    Dict.insert
                        (case space of
                            Mensam.Space.MkIdentifier id ->
                                id
                        )
                        owner
                        model.owners
            }

        SetSpacePictureUrl { space, url } ->
            { model
                | pictureUrls =
                    Dict.insert
                        (case space of
                            Mensam.Space.MkIdentifier id ->
                                id
                        )
                        url
                        model.pictureUrls
            }

        SetReservations reservations ->
            { model | reservations = reservations }

        SetHoveringReservation maybeN ->
            { model | hoveringReservation = maybeN }

        ChooseReservation id ->
            { model | popup = Just <| PopupViewReservation id }

        ClosePopup ->
            { model | popup = Nothing }

        EnableButtonForInitWizard enable ->
            { model | enableButtonForInitWizard = enable }

        OpenDialogToCreate ->
            { model
                | popup =
                    Just <|
                        PopupCreateFirstSpace
                            { name =
                                case mainModel.username of
                                    Nothing ->
                                        Mensam.Space.MkName ""

                                    Just username ->
                                        Mensam.Space.MkName <| Mensam.User.nameToString username ++ "'s Space"
                            , timezone = mainModel.timezone
                            , visible = False
                            , timezonePicker = Nothing
                            }
            }

        CloseDialogToCreate ->
            { model | popup = Nothing }

        DialogToCreateEnterSpaceName name ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupViewReservation _) ->
                    model

                Just (PopupCreateFirstSpace popupModel) ->
                    { model | popup = Just <| PopupCreateFirstSpace { popupModel | name = name } }

        DialogToCreateSetTimezoneToLocalTimezone ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupViewReservation _) ->
                    model

                Just (PopupCreateFirstSpace popupModel) ->
                    { model
                        | popup =
                            Just <|
                                PopupCreateFirstSpace
                                    { popupModel
                                        | timezone = mainModel.timezone
                                    }
                    }

        DialogToCreateTimezonePickerOpen ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupViewReservation _) ->
                    model

                Just (PopupCreateFirstSpace popupModel) ->
                    { model | popup = Just <| PopupCreateFirstSpace { popupModel | timezonePicker = Just <| Mensam.Widget.Timezone.init popupModel.timezone } }

        DialogToCreateTimezonePickerClose ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupViewReservation _) ->
                    model

                Just (PopupCreateFirstSpace popupModel) ->
                    case popupModel.timezonePicker of
                        Nothing ->
                            model

                        Just (Mensam.Widget.Timezone.MkModel timezonePickerModel) ->
                            { model | popup = Just <| PopupCreateFirstSpace { popupModel | timezone = timezonePickerModel.selected, timezonePicker = Nothing } }

        DialogToCreateTimezonePickerMessage msg ->
            case model.popup of
                Nothing ->
                    model

                Just (PopupViewReservation _) ->
                    model

                Just (PopupCreateFirstSpace popupModel) ->
                    case popupModel.timezonePicker of
                        Nothing ->
                            model

                        Just timezonePickerModel ->
                            case msg of
                                Mensam.Widget.Timezone.Select _ ->
                                    updatePure
                                        DialogToCreateTimezonePickerClose
                                        mainModel
                                        { model | popup = Just <| PopupCreateFirstSpace { popupModel | timezonePicker = Just <| Mensam.Widget.Timezone.update msg timezonePickerModel } }

                                Mensam.Widget.Timezone.Hover _ ->
                                    { model | popup = Just <| PopupCreateFirstSpace { popupModel | timezonePicker = Just <| Mensam.Widget.Timezone.update msg timezonePickerModel } }


type MessageEffect
    = ReportError Mensam.Error.Error
    | RefreshSpaces
    | RefreshOwnerName { space : Mensam.Space.Identifier, owner : Mensam.User.Identifier }
    | RefreshSpacePicture Mensam.Space.Identifier
    | ChooseSpace Mensam.Space.Identifier
    | RefreshReservations
    | CancelReservation Mensam.Reservation.Identifier
    | SubmitCreateFirstSpace
        { name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visible : Bool
        }
    | OpenPageToBrowseSpaces
    | OpenPageToViewReservations


spaceList : Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Cmd Message
spaceList baseUrl jwt =
    Mensam.Api.SpaceList.request baseUrl { jwt = jwt, order = [], member = Just True } <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceList.Success value) ->
                    case value.spaces of
                        [] ->
                            MessagePure <| EnableButtonForInitWizard True

                        _ ->
                            Messages <|
                                (MessagePure <| EnableButtonForInitWizard False)
                                    :: (MessagePure <| SetSpaces value.spaces)
                                    :: (List.map
                                            (\space ->
                                                MessageEffect <|
                                                    RefreshOwnerName
                                                        { space = space.id
                                                        , owner = space.owner
                                                        }
                                            )
                                            value.spaces
                                            ++ List.map (\space -> MessageEffect <| RefreshSpacePicture space.id)
                                                value.spaces
                                       )

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


ownerGetName : Mensam.Url.BaseUrl -> { jwt : Mensam.Auth.Bearer.Jwt, space : Mensam.Space.Identifier, owner : Mensam.User.Identifier } -> Cmd Message
ownerGetName baseUrl args =
    Mensam.Api.Profile.request baseUrl { jwt = args.jwt, id = args.owner } <|
        \result ->
            case result of
                Ok (Mensam.Api.Profile.Success value) ->
                    MessagePure <| SetOwnerName { space = args.space, owner = value.name }

                Ok Mensam.Api.Profile.ErrorUnknownUser ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting owner failed" <|
                                Mensam.Error.message "Unknown user" <|
                                    Mensam.Error.undefined

                Ok (Mensam.Api.Profile.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting owner failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.Profile.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting owner failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting owner failed" <|
                                Mensam.Error.http error


reservationList : Mensam.Url.BaseUrl -> { jwt : Mensam.Auth.Bearer.Jwt, model : Model } -> Cmd Message
reservationList baseUrl argument =
    Mensam.Api.ReservationList.request baseUrl
        { jwt = argument.jwt
        , timeWindow =
            { start =
                Just <|
                    Mensam.Time.toPosix argument.model.timezone <|
                        Mensam.Time.MkTimestamp
                            { date = argument.model.modelDateBegin
                            , time =
                                Mensam.Time.MkTime
                                    { hour = Mensam.Time.MkHour 0
                                    , minute = Mensam.Time.MkMinute 0
                                    , second = Mensam.Time.MkSecond 0
                                    }
                            }
            , end =
                Just <|
                    Mensam.Time.toPosix argument.model.timezone <|
                        Mensam.Time.MkTimestamp
                            { date = argument.model.modelDateEnd
                            , time =
                                Mensam.Time.MkTime
                                    { hour = Mensam.Time.MkHour 23
                                    , minute = Mensam.Time.MkMinute 59
                                    , second = Mensam.Time.MkSecond 59
                                    }
                            }
            }
        }
    <|
        \result ->
            case result of
                Ok (Mensam.Api.ReservationList.Success value) ->
                    MessagePure <| SetReservations value.reservations

                Ok (Mensam.Api.ReservationList.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationList.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Error.http error


reservationCancel : Mensam.Url.BaseUrl -> { jwt : Mensam.Auth.Bearer.Jwt, id : Mensam.Reservation.Identifier } -> Cmd Message
reservationCancel baseUrl argument =
    Mensam.Api.ReservationCancel.request baseUrl
        { jwt = argument.jwt
        , id = argument.id
        }
    <|
        \result ->
            case result of
                Ok Mensam.Api.ReservationCancel.Success ->
                    Messages
                        [ MessagePure ClosePopup
                        , MessageEffect RefreshReservations
                        ]

                Ok (Mensam.Api.ReservationCancel.ErrorInsufficientPermission permission) ->
                    MessageEffect <| ReportError <| Mensam.Space.Role.errorInsufficientPermission permission

                Ok Mensam.Api.ReservationCancel.ErrorAlreadyCancelled ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Cancelling reservation failed" <|
                                Mensam.Error.message "Reservation is already cancelled" <|
                                    Mensam.Error.undefined

                Ok Mensam.Api.ReservationCancel.ErrorAlreadyHappened ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Cancelling reservation failed" <|
                                Mensam.Error.message "Reservation already happened in the past" <|
                                    Mensam.Error.message "It is not possible to cancel fulfilled reservations" <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCancel.ErrorBody error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Cancelling reservation failed" <|
                                Mensam.Error.message "Bad request body" <|
                                    Mensam.Error.message error <|
                                        Mensam.Error.undefined

                Ok (Mensam.Api.ReservationCancel.ErrorAuth error) ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Auth.Bearer.error error

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Requesting reservations failed" <|
                                Mensam.Error.http error


spaceCreate :
    Mensam.Url.BaseUrl
    ->
        { jwt : Mensam.Auth.Bearer.Jwt
        , name : Mensam.Space.Name
        , timezone : Mensam.Time.Timezone
        , visibility : Mensam.Space.Visibility
        }
    -> Cmd Message
spaceCreate baseUrl req =
    Mensam.Api.SpaceCreate.request baseUrl req <|
        \result ->
            case result of
                Ok (Mensam.Api.SpaceCreate.Success _) ->
                    Messages
                        [ MessagePure CloseDialogToCreate
                        , MessageEffect RefreshSpaces
                        ]

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


downloadSpacePicture : Mensam.Url.BaseUrl -> Mensam.Auth.Bearer.Jwt -> Mensam.Space.Identifier -> Cmd Message
downloadSpacePicture baseUrl jwt space =
    Mensam.Api.SpacePictureDownload.request baseUrl
        { jwt = jwt
        , space = space
        }
    <|
        \response ->
            case response of
                Ok (Mensam.Api.SpacePictureDownload.Success picture) ->
                    MessagePure <| SetSpacePictureUrl { space = space, url = picture.url }

                Err error ->
                    MessageEffect <|
                        ReportError <|
                            Mensam.Error.message "Failed to download space picture" <|
                                Mensam.Error.http error
