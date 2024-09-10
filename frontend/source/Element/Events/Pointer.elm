module Element.Events.Pointer exposing (onClick, onDown, onEnter, onLeave, onMove, onUp)

import Element
import Html.Events.Extra.Mouse
import Html.Events.Extra.Pointer


type alias Event =
    { isPrimary : Bool
    , clientPos : { x : Float, y : Float }
    , offsetPos : { x : Float, y : Float }
    , pagePos : { x : Float, y : Float }
    , screenPos : { x : Float, y : Float }
    }


onClick : (Event -> msg) -> Element.Attribute msg
onClick handler =
    Element.htmlAttribute <| Html.Events.Extra.Mouse.onClick <| handler << translateMouseEvent


onDown : (Event -> msg) -> Element.Attribute msg
onDown handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onDown <| handler << translatePointerEvent


onUp : (Event -> msg) -> Element.Attribute msg
onUp handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onUp <| handler << translatePointerEvent


onEnter : (Event -> msg) -> Element.Attribute msg
onEnter handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onEnter <| handler << translatePointerEvent


onLeave : (Event -> msg) -> Element.Attribute msg
onLeave handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onLeave <| handler << translatePointerEvent


onMove : (Event -> msg) -> Element.Attribute msg
onMove handler =
    Element.htmlAttribute <| Html.Events.Extra.Pointer.onMove <| handler << translatePointerEvent


translateMouseEvent : Html.Events.Extra.Mouse.Event -> Event
translateMouseEvent event =
    { isPrimary = True
    , clientPos =
        case event.clientPos of
            ( x, y ) ->
                { x = x, y = y }
    , offsetPos =
        case event.offsetPos of
            ( x, y ) ->
                { x = x, y = y }
    , pagePos =
        case event.pagePos of
            ( x, y ) ->
                { x = x, y = y }
    , screenPos =
        case event.screenPos of
            ( x, y ) ->
                { x = x, y = y }
    }


translatePointerEvent : Html.Events.Extra.Pointer.Event -> Event
translatePointerEvent event =
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
