module Mensam.Room exposing (..)

import Element
import Mensam.Svg.Color
import Svg
import Svg.Attributes
import Svg.Events


type Room msg
    = MkRoom
        { dimensions : { minX : Int, minY : Int, maxX : Int, maxY : Int }
        , drawingInstructions : List (DrawingInstructions msg)
        , messages :
            { onClickTable : msg
            , onEnterTable : msg
            , onLeaveTable : msg
            }
        }


type DrawingInstructions msg
    = DrawGrid { spacing : Distance }
    | DrawTable
        { position : Position
        , direction : Direction
        , size :
            { width : Distance
            , depth : Distance
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
    = MkDirection { degrees : Int }


directionGetDegrees : Direction -> Int
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


drawInstruction : { onClickTable : msg, onEnterTable : msg, onLeaveTable : msg } -> DrawingInstructions msg -> List (Svg.Svg msg)
drawInstruction messages instruction =
    case instruction of
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

        DrawTable { position, direction, size } ->
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
                        , String.fromInt <| directionGetDegrees direction
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
                    , Svg.Events.onClick messages.onClickTable
                    , Svg.Events.onMouseOver messages.onEnterTable
                    , Svg.Events.onMouseOut messages.onLeaveTable
                    ]
                    []
                ]
            ]


example : { onClickTable : msg, onEnterTable : msg, onLeaveTable : msg } -> Room msg
example messages =
    MkRoom
        { dimensions = { minX = -5000, minY = -5000, maxX = 5000, maxY = 5000 }
        , drawingInstructions =
            let
                studentTable x y dir =
                    DrawTable
                        { position = MkPosition { x = x, y = y }
                        , direction = MkDirection { degrees = dir }
                        , size = { width = MkDistance 160, depth = MkDistance 90 }
                        }
            in
            [ DrawGrid { spacing = MkDistance 10 }
            , studentTable -100 -300 10
            , studentTable 100 -300 -4
            , studentTable -100 -100 -5
            , studentTable 100 -100 0
            , studentTable -100 100 3
            , studentTable 100 100 -2
            , DrawTable
                { position = MkPosition { x = 0, y = 300 }
                , direction = MkDirection { degrees = 180 }
                , size = { width = MkDistance 200, depth = MkDistance 90 }
                }
            ]
        , messages = messages
        }
