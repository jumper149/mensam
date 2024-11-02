module Mensam.Tracker exposing
    ( State
    , Tracker
    , clear
    , init
    , register
    , toHttp
    )

import Http


{-| A token to identify a tracked HTTP request.
-}
type Tracker
    = MkTracker Int


{-| Track registered and cancelled HTTP requests.
-}
type State
    = MkState
        { cancelled : Int
        , registered : Int
        }


init : State
init =
    MkState
        { cancelled = 0
        , registered = 0
        }


{-| We are using an explicit maximum tracker to avoid integer overflow.
-}
maxTracker : Tracker
maxTracker =
    MkTracker 32767


{-| Cancel all registered HTTP requests.
-}
clear : State -> ( State, Cmd msg )
clear (MkState state) =
    ( MkState { state | cancelled = state.registered }
    , Cmd.batch <|
        List.map (Http.cancel << toHttp << MkTracker) <|
            if state.registered >= state.cancelled then
                List.range state.cancelled (state.registered - 1)

            else
                List.concat
                    [ List.range state.cancelled
                        (case maxTracker of
                            MkTracker n ->
                                n
                        )
                    , List.range
                        0
                        (state.registered - 1)
                    ]
    )


{-| Register a new HTTP request.
-}
register :
    State
    -> (Tracker -> Cmd msg)
    -> ( State, Cmd msg )
register oldState useTracker =
    let
        ( state, tracker ) =
            newTracker oldState
    in
    ( state, useTracker tracker )


{-| Add a new tracker to `State`.
-}
newTracker : State -> ( State, Tracker )
newTracker (MkState state) =
    ( MkState
        { state
            | registered =
                modBy
                    (case maxTracker of
                        MkTracker n ->
                            n + 1
                    )
                <|
                    state.registered
                        + 1
        }
    , MkTracker state.registered
    )


{-| Convert a `Tracker` to a type that is compatible with the elm/http library.
-}
toHttp : Tracker -> String
toHttp (MkTracker n) =
    "tracker_registered_" ++ String.fromInt n
