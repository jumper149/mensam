module Element.Events.Pointer exposing (..)

import Element
import Html.Events.Extra.Pointer


type alias Event =
    { isPrimary : Bool
    , clientPos : { x : Float, y : Float }
    , offsetPos : { x : Float, y : Float }
    , pagePos : { x : Float, y : Float }
    , screenPos : { x : Float, y : Float }
    }


onDown : (Event -> msg) -> Element.Attribute msg
onDown handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onDown <| handler << translateEvent


onUp : (Event -> msg) -> Element.Attribute msg
onUp handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onUp <| handler << translateEvent


onEnter : (Event -> msg) -> Element.Attribute msg
onEnter handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onEnter <| handler << translateEvent


onLeave : (Event -> msg) -> Element.Attribute msg
onLeave handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onLeave <| handler << translateEvent


onMove : (Event -> msg) -> Element.Attribute msg
onMove handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onMove <| handler << translateEvent


translateEvent : Html.Events.Extra.Pointer.Event -> Event
translateEvent event =
    { isPrimary = event.isPrimary
    , clientPos =
        case event.pointer.clientPos of
            ( x, y ) ->
                { x = x, y = y }
    , offsetPos =
        case event.pointer.offsetPos of
            ( x, y ) ->
                { x = x, y = y }
    , pagePos =
        case event.pointer.pagePos of
            ( x, y ) ->
                { x = x, y = y }
    , screenPos =
        case event.pointer.screenPos of
            ( x, y ) ->
                { x = x, y = y }
    }
