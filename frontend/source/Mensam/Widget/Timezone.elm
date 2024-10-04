module Mensam.Widget.Timezone exposing (..)

import Element
import Element.Background
import Element.Events.Pointer
import Element.Font
import Html.Attributes
import Mensam.Element.Font
import Mensam.Time


type Model
    = MkModel
        { selected : Mensam.Time.Timezone
        , hovering : Maybe Int
        }


unModel : Model -> { selected : Mensam.Time.Timezone, hovering : Maybe Int }
unModel (MkModel value) =
    value


init : Mensam.Time.Timezone -> Model
init initialTimezone =
    MkModel
        { selected = initialTimezone
        , hovering = Nothing
        }


type Message
    = Hover (Maybe Int)
    | Select Mensam.Time.Timezone


elementPickTimezone : Model -> Element.Element Message
elementPickTimezone model =
    Element.indexedTable
        [ Element.width <| Element.minimum 250 <| Element.maximum 320 Element.fill
        , Element.height <| Element.minimum 180 <| Element.maximum 230 Element.fill
        , Element.Background.color (Element.rgba 0 0 0 0.1)
        , Element.Font.family [ Mensam.Element.Font.condensed ]
        , Element.Font.size 16
        , Element.clipY
        , Element.scrollbarY
        ]
        { data = Mensam.Time.allTimezones
        , columns =
            [ { header = Element.none
              , width = Element.minimum 250 <| Element.maximum 320 Element.fill
              , view =
                    \n timezone ->
                        Element.el
                            [ Element.Events.Pointer.onLeave <| \_ -> Hover Nothing
                            , Element.Events.Pointer.onEnter <| \_ -> Hover <| Just n
                            , Element.Events.Pointer.onClick <| \_ -> Select timezone
                            , Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                            , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
                            , let
                                alpha =
                                    case (unModel model).hovering of
                                        Nothing ->
                                            0.2

                                        Just m ->
                                            if m == n then
                                                0.4

                                            else
                                                0.2
                              in
                              if (unModel model).selected == timezone then
                                Element.Background.color (Element.rgba 0 0.2 0 alpha)

                              else
                                Element.Background.color (Element.rgba 0 0 0 alpha)
                            , Element.height <| Element.px 40
                            , Element.width Element.fill
                            , Element.clipX
                            ]
                        <|
                            Element.el
                                [ Element.centerY
                                , Element.alignLeft
                                , Element.paddingXY 10 0
                                ]
                            <|
                                Element.text <|
                                    Mensam.Time.timezoneToString timezone
              }
            ]
        }


update : Message -> Model -> Model
update message (MkModel model) =
    case message of
        Hover maybeN ->
            MkModel { model | hovering = maybeN }

        Select timezone ->
            MkModel { model | selected = timezone }
