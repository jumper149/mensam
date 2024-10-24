module Mensam.Room exposing (..)

import Element
import Mensam.Desk
import Mensam.Space
import Mensam.Svg.Color
import Svg
import Svg.Attributes
import Svg.Events


type Room msg
    = MkRoom
        { dimensions : { minX : Int, minY : Int, maxX : Int, maxY : Int }
        , drawingInstructions : List (DrawingInstruction msg)
        , messages :
            { onClickTable :
                { id : Mensam.Desk.Identifier
                , name : Mensam.Desk.Name
                , space : Mensam.Space.Identifier
                , location : Maybe Mensam.Desk.Location
                }
                -> msg
            , onEnterTable : msg
            , onLeaveTable : msg
            }
        }


type DrawingInstruction msg
    = DrawNothing
    | DrawGrid { spacing : Distance }
    | DrawTable
        { position : Position
        , direction : Direction
        , size :
            { width : Distance
            , depth : Distance
            }
        , desk :
            { id : Mensam.Desk.Identifier
            , name : Mensam.Desk.Name
            , space : Mensam.Space.Identifier
            , location : Maybe Mensam.Desk.Location
            }
        }


{-| A point on a 2D grid.
-}
type Position
    = MkPosition { x : Float, y : Float }


positionGetX : Position -> Float
positionGetX (MkPosition position) =
    position.x


positionGetY : Position -> Float
positionGetY (MkPosition position) =
    position.y


type Distance
    = MkDistance Float


distanceGet : Distance -> Float
distanceGet (MkDistance distance) =
    distance


{-| A direction on a 2D grid.
The value `degrees` should satisfy `0 <= degrees < 360`.

Here are some example values to get an idea:

  - North: 0
  - East: 90
  - South: 180
  - West: 270

-}
type Direction
    = MkDirection { degrees : Float }


directionGetDegrees : Direction -> Float
directionGetDegrees (MkDirection direction) =
    -direction.degrees


drawRoom : Room msg -> Element.Element msg
drawRoom (MkRoom room) =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.width <| String.fromInt <| room.dimensions.maxX - room.dimensions.minX
            , Svg.Attributes.height <| String.fromInt <| room.dimensions.maxY - room.dimensions.minY
            , Svg.Attributes.viewBox <|
                String.concat
                    [ String.fromInt room.dimensions.minX
                    , " "
                    , String.fromInt room.dimensions.minY
                    , " "
                    , String.fromInt <| room.dimensions.maxX - room.dimensions.minX
                    , " "
                    , String.fromInt <| room.dimensions.maxY - room.dimensions.minY
                    ]
            , Svg.Attributes.transform <|
                String.concat
                    [ "scale(0.5)"
                    , " "
                    , "scale(1,-1)"
                    ]
            ]
        <|
            List.concatMap (drawInstruction room.messages) <|
                room.drawingInstructions


drawInstruction :
    { onClickTable :
        { id : Mensam.Desk.Identifier
        , name : Mensam.Desk.Name
        , space : Mensam.Space.Identifier
        , location : Maybe Mensam.Desk.Location
        }
        -> msg
    , onEnterTable : msg
    , onLeaveTable : msg
    }
    -> DrawingInstruction msg
    -> List (Svg.Svg msg)
drawInstruction messages instruction =
    case instruction of
        DrawNothing ->
            []

        DrawGrid { spacing } ->
            let
                smallSpacing =
                    String.fromFloat <| distanceGet spacing

                largeSpacing =
                    String.fromFloat <| 10 * distanceGet spacing
            in
            [ Svg.g
                []
                [ Svg.defs
                    []
                    [ Svg.pattern
                        [ Svg.Attributes.id "smallGrid"
                        , Svg.Attributes.width smallSpacing
                        , Svg.Attributes.height smallSpacing
                        , Svg.Attributes.patternUnits "userSpaceOnUse"
                        ]
                        [ Svg.path
                            [ Svg.Attributes.d <| "M " ++ smallSpacing ++ " 0 L 0 0 0 " ++ smallSpacing
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
                            , Svg.Attributes.strokeWidth "0.5"
                            ]
                            []
                        ]
                    , Svg.pattern
                        [ Svg.Attributes.id "largeGrid"
                        , Svg.Attributes.width largeSpacing
                        , Svg.Attributes.height largeSpacing
                        , Svg.Attributes.patternUnits "userSpaceOnUse"
                        ]
                        [ Svg.rect
                            [ Svg.Attributes.width largeSpacing
                            , Svg.Attributes.height largeSpacing
                            , Svg.Attributes.fill "url(#smallGrid)"
                            ]
                            []
                        , Svg.path
                            [ Svg.Attributes.d <| "M " ++ largeSpacing ++ " 0 L 0 0 0 " ++ largeSpacing
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke Mensam.Svg.Color.dark.black
                            , Svg.Attributes.strokeWidth "1"
                            ]
                            []
                        ]
                    ]
                , Svg.rect
                    [ Svg.Attributes.width "100%"
                    , Svg.Attributes.height "100%"
                    , Svg.Attributes.fill "url(#largeGrid)"
                    , Svg.Attributes.transform "rotate(0)"
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.width "100%"
                    , Svg.Attributes.height "100%"
                    , Svg.Attributes.fill "url(#largeGrid)"
                    , Svg.Attributes.transform "rotate(90)"
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.width "100%"
                    , Svg.Attributes.height "100%"
                    , Svg.Attributes.fill "url(#largeGrid)"
                    , Svg.Attributes.transform "rotate(180)"
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.width "100%"
                    , Svg.Attributes.height "100%"
                    , Svg.Attributes.fill "url(#largeGrid)"
                    , Svg.Attributes.transform "rotate(270)"
                    ]
                    []
                ]
            ]

        DrawTable { position, direction, size, desk } ->
            [ Svg.g
                [ Svg.Attributes.transform <|
                    String.concat
                        [ "translate("
                        , String.fromFloat <| positionGetX position
                        , ","
                        , String.fromFloat <| positionGetY position
                        , ")"
                        , " "
                        , "rotate("
                        , String.fromFloat <| directionGetDegrees direction
                        , ")"
                        ]
                ]
                [ Svg.rect
                    [ Svg.Attributes.x "-25"
                    , Svg.Attributes.y <| String.fromFloat <| -(10 + 30 + distanceGet size.depth / 2)
                    , Svg.Attributes.width "50"
                    , Svg.Attributes.height "30"
                    , Svg.Attributes.fill Mensam.Svg.Color.bright.white
                    ]
                    []
                , Svg.rect
                    [ Svg.Attributes.x <| String.fromFloat <| 0 - distanceGet size.width / 2
                    , Svg.Attributes.y <| String.fromFloat <| 0 - distanceGet size.depth / 2
                    , Svg.Attributes.width <| String.fromFloat <| distanceGet size.width
                    , Svg.Attributes.height <| String.fromFloat <| distanceGet size.depth
                    , Svg.Attributes.fill Mensam.Svg.Color.bright.white
                    , Svg.Events.onClick <| messages.onClickTable desk
                    , Svg.Events.onMouseOver messages.onEnterTable
                    , Svg.Events.onMouseOut messages.onLeaveTable
                    ]
                    []
                ]
            ]


instructGrid : DrawingInstruction msg
instructGrid =
    DrawGrid { spacing = MkDistance 10 }


instructDesk :
    { id : Mensam.Desk.Identifier
    , name : Mensam.Desk.Name
    , space : Mensam.Space.Identifier
    , location : Maybe Mensam.Desk.Location
    }
    -> DrawingInstruction msg
instructDesk desk =
    case desk.location of
        Nothing ->
            DrawNothing

        Just (Mensam.Desk.MkLocation location) ->
            DrawTable
                { position =
                    case location.position of
                        Mensam.Desk.MkPosition position ->
                            MkPosition
                                position
                , direction =
                    case location.direction of
                        Mensam.Desk.MkDirection direction ->
                            MkDirection direction
                , size =
                    case location.size of
                        Mensam.Desk.MkSize size ->
                            { width = MkDistance size.width
                            , depth = MkDistance size.depth
                            }
                , desk = desk
                }
